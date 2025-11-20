package tf.bug.fe

import calico.*
import calico.html.io.{*, given}
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.SignallingRef
import fs2.dom.*
import org.scalajs.dom.FileReader
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}
import typings.monacoEditor.esmVsEditorEditorDotapiMod as monaco

object SquirrelExplorerFrontend {

  def program(
    compilerWk: CompilerWorkerThread,
    compiledRenderWk: RendererWorkerThread,
    uploadedRenderWk: RendererWorkerThread,
    compiledResult: SignallingRef[IO, RenderResult],
    uploadedResult: SignallingRef[IO, RenderResult]
  ): Resource[IO, HtmlElement[IO]] = {
    Resource.eval(MonacoSquirrelLanguage.register) >>
    Resource.eval(uploadedRenderWk.setEncodingSjis(true)) >>
      div(
        idAttr := "grid-container",
        leftEditor(compilerWk, compiledRenderWk, compiledResult),
        rightEditor(uploadedRenderWk, compiledResult, uploadedResult)
      )
  }

  def leftEditor(cwk: CompilerWorkerThread, rwk: RendererWorkerThread, compiledResult: SignallingRef[IO, RenderResult]): Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "left-editor"
    ).flatTap { container =>
      MonacoEditor.create(container, language = Some("squirrel")).flatMap { editor =>
        editor.discreteModel.switchMap(model => model.discreteValue.foreach { v =>
          cwk.compileClosure(v).flatMap {
            case Left(_) => IO.unit
            case Right(ui8a) =>
              rwk.tryToRender(ui8a).flatMap {
                case Left(_) => IO.unit
                case Right(rr) => compiledResult.set(rr)
              }
          }
        }).compile.drain.background
      }
    }
  }

  def readFile(file: org.scalajs.dom.File): IO[Uint8Array] =
    IO.async_[Uint8Array] { cb =>
      val reader = new FileReader()
      reader.onload = { p =>
        cb(new Uint8Array(reader.result.asInstanceOf[ArrayBuffer]).asRight)
      }
      reader.readAsArrayBuffer(file)
    }

  def rightEditor(
    rwk: RendererWorkerThread,
    compiledResult: SignallingRef[IO, RenderResult],
    uploadedResult: SignallingRef[IO, RenderResult]
  ): Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "right-panel",
      input.withSelf { self => (
        `type` := "file",
        onChange --> (_.foreach { ev =>
          val files = IO.delay(self.asInstanceOf[org.scalajs.dom.HTMLInputElement].files)
          files.flatMap { fl =>
            if(fl.length < 1) IO.unit
            else {
              SquirrelExplorerFrontend.readFile(fl.item(0)).flatMap { buf =>
                rwk.tryToRender(buf).flatMap {
                  case Left(_) => IO.unit
                  case Right(rr) => uploadedResult.set(rr)
                }
              }
            }
          }
        })
      )},
      div(idAttr := "right-editor").flatTap { container =>
        (
          Resource.eval(Ref.of[IO, scalajs.js.Array[monaco.languages.InlayHint]](scalajs.js.Array())),
          Resource.eval(Ref.of[IO, scalajs.js.Array[monaco.languages.InlayHint]](scalajs.js.Array())),
        ).parFlatMapN {
          (uploadedHints, compiledHints) =>
            Resource.eval(IO(monaco.languages.register(monaco.languages.ILanguageExtensionPoint("cnut")))) >>
            MonacoDiffEditor.create(container, "cnut", "cnut_diff").flatMap { editor =>
              MonacoDiffEditorViewModel.of(editor).flatMap { vm =>
                InlayHintsProvider.register("cnut") { (model, range, ct) =>
                  editor.model.flatMap { dm =>
                    val hintArray =
                      if (model.id == dm.original.id) {
                        uploadedHints.get
                      } else if (model.id == dm.modified.id) {
                        compiledHints.get
                      } else IO.raiseError(new RuntimeException("Unknown model with cnut language"))
                    hintArray.map { arr =>
                      val slice = SquirrelExplorerFrontend.binarySearchSlice(arr, range)
                      monaco.languages.InlayHintList(() => (), slice)
                    }
                  }
                }.flatMap { _ =>
                  val renderedDiscrete = uploadedResult.discrete.either(compiledResult.discrete).foreach {
                    case Left(uploaded) =>
                      editor.setOriginalResult(vm, uploaded, uploadedHints)
                    case Right(compiled) =>
                      editor.setModifiedResult(vm, compiled, compiledHints)
                  }
                  renderedDiscrete.compile.drain.background
                }
              }
            }
        }
      }
    )
  }

  def binarySearchSlice(
    arr: scalajs.js.Array[monaco.languages.InlayHint],
    range: monaco.Range
  ): scalajs.js.Array[monaco.languages.InlayHint] = {
    if (arr.length <= 2) return arr

    val firstLessThanLine = range.startLineNumber
    val firstGreaterThanLine = range.endLineNumber

    // Do a binary search to find a lower transition point
    var fltLo = -1
    var fltHi = arr.length
    while ((1 + fltLo) < fltHi) {
      val mid = fltLo + ((fltHi - fltLo) >> 1)

      val elem = arr(mid)
      // pred(elem) = is the elem after the first LT line
      if (elem.position.lineNumber >= firstLessThanLine) {
        fltHi = mid
      } else {
        fltLo = mid
      }
    }
    val ixOfFirst = fltHi - 1

    var fgtLo = -1
    var fgtHi = arr.length
    while ((1 + fgtLo) < fgtHi) {
      val mid = fgtLo + ((fgtHi - fgtLo) >> 1)

      val elem = arr(mid)
      // pred(elem) = is the elem after the first GT line
      if (elem.position.lineNumber > firstGreaterThanLine) {
        fgtHi = mid
      } else {
        fgtLo = mid
      }
    }
    val ixOfLast = fgtHi - 1

    arr.jsSlice(ixOfFirst.max(0), 1 + ixOfLast)
  }
}
