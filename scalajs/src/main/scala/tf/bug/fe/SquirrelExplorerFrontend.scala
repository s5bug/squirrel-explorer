package tf.bug.fe

import calico.*
import calico.html.io.{*, given}
import cats.effect.std.Dispatcher
import cats.effect.{IO, Ref, Resource, SyncIO}
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.SignallingRef
import fs2.dom.*
import org.scalajs.dom.FileReader
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

object SquirrelExplorerFrontend {

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

  def compileAndRenderLeftContent(content: String, cwk: CompilerWorkerThread, rwk: RendererWorkerThread): IO[Option[RenderResult]] = {
    cwk.compileClosure(content).flatMap {
      case Left(error) => IO.none // TODO
      case Right(bytes) => rwk.tryToRender(bytes).flatMap {
        case Left(error) => IO.none // TODO
        case Right(result) => IO.some(result)
      }
    }
  }

  def leftEditor(cwk: CompilerWorkerThread, rwk: RendererWorkerThread, compiledResult: SignallingRef[IO, RenderResult]): Resource[IO, HtmlElement[IO]] = Dispatcher.sequential(true).flatMap { dispatch =>
    div(
      idAttr := "left-editor",
      CodemirrorView(CodemirrorViewConfig().setExtensionsVarargs(
        Codemirror.basicSetup,
        Codemirror.minimap(dispatch)(Codemirror.MinimapConfig(_ => SyncIO { org.scalajs.dom.document.createElement("div").asInstanceOf })),
        LezerSquirrelLanguage.squirrel,
        CodemirrorView.updateListener(dispatch) { vu =>
          IO.whenA(vu.docChanged) {
            compileAndRenderLeftContent(vu.state.doc.toString, cwk, rwk).flatMap {
              case None => IO.unit
              case Some(rr) => compiledResult.set(rr)
            }
          }
        }
      )).map(_.dom)
    )
  }

  def readFile(file: org.scalajs.dom.File): IO[Uint8Array] =
    IO.async_[Uint8Array] { cb =>
      val reader = new FileReader()
      reader.onload = { p =>
        cb(new Uint8Array(reader.result.asInstanceOf[ArrayBuffer]).asRight)
      }
      reader.readAsArrayBuffer(file)
    }

  def replaceView(v: CodemirrorView, s: String): IO[Unit] = {
    val change = typings.codemirrorState.anon.From(0)
    change.to = v.state.doc.length
    change.insert = s

    val transactionSpec =
      typings.codemirrorState.mod.TransactionSpec()
        .setChanges(change)

    v.dispatch(transactionSpec)
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
            if fl.length < 1 then IO.unit
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
      div(
        idAttr := "right-editor",
        CodemirrorMergeView(
          CodemirrorStateConfig()
            .setExtensionsVarargs(
              Codemirror.minimalSetup,
              CodemirrorView.lineNumbers,
              CodemirrorState.readOnly.of(true),
              CodemirrorView.editable.of(false),
            ),
          CodemirrorStateConfig()
            .setExtensionsVarargs(
              Codemirror.minimalSetup,
              CodemirrorView.lineNumbers,
              CodemirrorState.readOnly.of(true),
              CodemirrorView.editable.of(false),
            )
        ).flatTap { mv =>
          val renderUploadedToA = uploadedResult.discrete.foreach { rr =>
            replaceView(mv.a, rr.rawText)
          }
          val renderCompiledToB = (Stream.eval(compiledResult.get) ++ compiledResult.discrete).foreach { rr =>
            replaceView(mv.b, rr.rawText)
          }
          (renderUploadedToA.merge(renderCompiledToB)).compile.drain.background
        }.map(_.dom)
      )
    )
  }
}
