package tf.bug.fe

import calico.*
import calico.html.io.{*, given}
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.SignallingRef
import fs2.dom.*
import org.scalajs.dom.FileReader
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}
import scodec.{Attempt, DecodeResult}

object SquirrelExplorer extends IOWebApp {

  override def render: Resource[IO, HtmlElement[IO]] =
    for {
      // TODO make this a request/response API rather than request-stream/response-stream
      compilerWk <- WorkerConnection.ofCompiler
      explorerWk <- WorkerConnection.ofExplorer
      renderedCompiled <- SignallingRef.of[IO, String]("").toResource
      renderedUploaded <- SignallingRef.of[IO, String]("").toResource
      p <- program(compilerWk, explorerWk, renderedCompiled, renderedUploaded)
    } yield p

  def program(
    compilerWk: WorkerConnection[String, String],
    explorerWk: WorkerConnection[Uint8Array, String],
    renderedCompiled: SignallingRef[IO, String],
    renderedUploaded: SignallingRef[IO, String]
  ): Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "grid-container",
      leftEditor(compilerWk, renderedCompiled),
      rightEditor(explorerWk, renderedCompiled, renderedUploaded)
    )
  }

  def leftEditor(wk: WorkerConnection[String, String], renderedCompiled: SignallingRef[IO, String]): Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "left-editor"
    ).flatTap { container =>
      MonacoEditor.create(container).flatMap { editor =>
        editor.discreteModel.switchMap(model => model.discreteValue.foreach { v =>
          wk.callResponse(v).flatMap { resp =>
            if(resp.startsWith("[error]")) IO.unit
            else renderedCompiled.set(resp)
          }
        }).compile.drain.background
      }
    }
  }

  def rightEditor(
    explorerWk: WorkerConnection[Uint8Array, String],
    renderedCompiled: SignallingRef[IO, String],
    renderedUploaded: SignallingRef[IO, String]
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
              getUintArray >>= explorerWk.callResponse >>= renderedUploaded.set
            }
          }
        })
      )},
      div(idAttr := "right-editor").flatTap { container =>
        MonacoDiffEditor.create(container).flatMap { editor =>
          val renderedDiscrete = renderedUploaded.discrete.either(renderedCompiled.discrete).foreach {
            case Left(uploaded) => editor.setOriginalDiffCalc(uploaded)
            case Right(compiled) => editor.setModifiedDiffCalc(compiled)
          }
          renderedDiscrete.compile.drain.background
        }
      }
    )
  }

}
