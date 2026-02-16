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
import tf.bug.cnut.RenderedCnut
import tf.bug.worker.DragonboxApi

object SquirrelExplorerFrontend {

  def program(
    compilerWk: CompilerWorkerThread,
    compiledRenderWk: RendererWorkerThread,
    uploadedRenderWk: RendererWorkerThread,
    compiledResult: SignallingRef[IO, Option[RenderedCnut]],
    uploadedResult: SignallingRef[IO, Option[RenderedCnut]]
  ): Resource[IO, HtmlElement[IO]] = {
    Resource.eval(uploadedRenderWk.setEncodingSjis(true)) >>
      div(
        idAttr := "grid-container",
        leftEditor(compilerWk, compiledRenderWk, compiledResult),
        rightEditor(uploadedRenderWk, compiledResult, uploadedResult)
      )
  }

  def compileAndRenderLeftContent(content: String, cwk: CompilerWorkerThread, rwk: RendererWorkerThread): IO[Option[RenderedCnut]] = {
    cwk.compileClosure(content).flatMap {
      case Left(error) => IO.none // TODO
      case Right(bytes) => rwk.tryToRender(bytes).flatMap {
        case Left(error) => IO.none // TODO
        case Right(result) => IO.some(result)
      }
    }
  }

  def leftEditor(cwk: CompilerWorkerThread, rwk: RendererWorkerThread, compiledResult: SignallingRef[IO, Option[RenderedCnut]]): Resource[IO, HtmlElement[IO]] = Dispatcher.sequential(true).flatMap { dispatch =>
    Resource.eval(compileAndRenderLeftContent("", cwk, rwk).flatMap(_.traverse_(r => compiledResult.set(Some(r))))) >> div(
      idAttr := "left-editor",
      CodemirrorView(CodemirrorViewConfig().setExtensionsVarargs(
        Codemirror.basicSetup,
        Codemirror.minimap(dispatch)(Codemirror.MinimapConfig(_ => SyncIO { org.scalajs.dom.document.createElement("div").asInstanceOf[fs2.dom.HtmlElement[IO]] })),
        LezerSquirrelLanguage.squirrel,
        CodemirrorView.updateListener(dispatch) { vu =>
          IO.whenA(vu.docChanged) {
            compileAndRenderLeftContent(vu.state.doc.toString, cwk, rwk).flatMap(_.traverse_(r => compiledResult.set(Some(r))))
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

  def replaceView(v: CodemirrorView, next: RenderedCnut): IO[Unit] = v.state.field(CnutEditorState.cnutField).flatMap { current =>
    val transactionSpec =
      typings.codemirrorState.mod.TransactionSpec()

    val nextAnnotation = CnutEditorState.cnutAnnotationType.of(Some(next))
    // ST incorrectly types this as Annotation[Any] instead of Annotation[?]
    transactionSpec.setAnnotationsVarargs(nextAnnotation.asInstanceOf[typings.codemirrorState.mod.Annotation[Any]])

    current match {
      case None =>
        val change = typings.codemirrorState.anon.From(0)
        change.to = v.state.doc.length
        change.insert = next.string

        transactionSpec.setChanges(change)
      case Some(preDiff) =>
        val changes: scalajs.js.Array[typings.codemirrorState.anon.From] = RenderedCnut.diff(preDiff, next)
        transactionSpec.setChanges(changes.asInstanceOf[scalajs.js.Array[Any]])
    }

    v.dispatch(transactionSpec)
  }

  def rightEditor(
    rwk: RendererWorkerThread,
    compiledResult: SignallingRef[IO, Option[RenderedCnut]],
    uploadedResult: SignallingRef[IO, Option[RenderedCnut]]
  ): Resource[IO, HtmlElement[IO]] = Dispatcher.sequential(true).flatMap { dispatch =>
    div(
      idAttr := "right-panel",
      input.withSelf { self =>
        (
          `type` := "file",
          onChange --> (_.foreach { ev =>
            val files = IO.delay(self.asInstanceOf[org.scalajs.dom.HTMLInputElement].files)
            files.flatMap { fl =>
              if fl.length < 1 then IO.unit
              else {
                SquirrelExplorerFrontend.readFile(fl.item(0)).flatMap { buf =>
                  rwk.tryToRender(buf).flatMap {
                    case Left(_) => IO.unit
                    case Right(rr) => uploadedResult.set(Some(rr))
                  }
                }
              }
            }
          })
        )
      },
      div(
        idAttr := "right-editor",
        CodemirrorMergeView(
          CodemirrorStateConfig()
            .setExtensionsVarargs(
              Codemirror.minimalSetup,
              CodemirrorView.lineNumbers,
              CodemirrorState.readOnly.of(true),
              CodemirrorView.editable.of(false),
              CnutEditorState.cnutField,
              LezerCnutLanguage.cnut,
              LezerCnutLanguage.cnutLinter(dispatch),
            ),
          CodemirrorStateConfig()
            .setExtensionsVarargs(
              Codemirror.minimalSetup,
              CodemirrorView.lineNumbers,
              CodemirrorState.readOnly.of(true),
              CodemirrorView.editable.of(false),
              CnutEditorState.cnutField,
              LezerCnutLanguage.cnut,
              LezerCnutLanguage.cnutLinter(dispatch),
            ),
          LezerCnutLanguage.diffCnut
        ).flatTap { mv =>
          val renderUploadedToA = uploadedResult.discrete.unNone.foreach { rr =>
            replaceView(mv.a, rr)
          }
          val renderCompiledToB = (Stream.eval(compiledResult.get) ++ compiledResult.discrete).unNone.foreach { rr =>
            replaceView(mv.b, rr)
          }
          (renderUploadedToA.merge(renderCompiledToB)).compile.drain.background
        }.map(_.dom)
      )
    )
  }
}
