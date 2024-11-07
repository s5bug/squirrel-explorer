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
import scodec.{Attempt, DecodeResult}

object SquirrelExplorer extends IOWebApp {

  override def render: Resource[IO, HtmlElement[IO]] = {
    (
      CompilerWorkerThread.default,
      RendererWorkerThread.default,
      RendererWorkerThread.default,
      SignallingRef.of[IO, RenderResult](RenderResult.empty).toResource,
      SignallingRef.of[IO, RenderResult](RenderResult.empty).toResource
    ).parFlatMapN { (compilerWk, compiledRenderWk, uploadedRenderWk, compiledResult, uploadedResult) =>
      program(compilerWk, compiledRenderWk, uploadedRenderWk, compiledResult, uploadedResult)
    }
  }

  def program(
    compilerWk: CompilerWorkerThread,
    compiledRenderWk: RendererWorkerThread,
    uploadedRenderWk: RendererWorkerThread,
    compiledResult: SignallingRef[IO, RenderResult],
    uploadedResult: SignallingRef[IO, RenderResult]
  ): Resource[IO, HtmlElement[IO]] = {
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
      MonacoEditor.create(container).flatMap { editor =>
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
              // TODO move this to a utility so it's easier to read
              val getUintArray = IO.async_[Uint8Array] { cb =>
                val reader = new FileReader()
                reader.onload = { p =>
                  cb(new Uint8Array(reader.result.asInstanceOf[ArrayBuffer]).asRight)
                }
                reader.readAsArrayBuffer(fl.item(0))
              }
              getUintArray.flatMap { buf =>
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
          Resource.eval(Ref.of[IO, scalajs.js.Array[typings.monacoEditor.mod.languages.InlayHint]](scalajs.js.Array())),
          Resource.eval(Ref.of[IO, scalajs.js.Array[typings.monacoEditor.mod.languages.InlayHint]](scalajs.js.Array())),
        ).parFlatMapN {
          (uploadedHints, compiledHints) =>
            Resource.eval(IO(typings.monacoEditor.mod.languages.register(typings.monacoEditor.mod.languages.ILanguageExtensionPoint("cnut")))) >>
            MonacoDiffEditor.create(container, "cnut", "cnut_diff").flatMap { editor =>
              MonacoDiffEditorViewModel.of(editor).flatMap { vm =>
                InlayHintsProvider.register("cnut") { (model, range, ct) =>
                  editor.model.flatMap { dm =>
                    if (model.id == dm.original.id) {
                      uploadedHints.get.map(r => typings.monacoEditor.mod.languages.InlayHintList(() => (), r))
                    } else if (model.id == dm.modified.id) {
                      compiledHints.get.map(r => typings.monacoEditor.mod.languages.InlayHintList(() => (), r))
                    } else IO.raiseError(new RuntimeException("Unknown model with cnut language"))
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

}
