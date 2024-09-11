package tf.bug.fe

import calico.*
import calico.html.io.{*, given}
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import fs2.*
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
      p <- program(compilerWk, explorerWk)
    } yield p

  def program(compilerWk: WorkerConnection[String, String], explorerWk: WorkerConnection[Uint8Array, String]): Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "grid-container",
      leftEditor(compilerWk),
      rightEditor(compilerWk, explorerWk)
    )
  }

  def leftEditor(wk: WorkerConnection[String, String]): Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "left-editor"
    ).flatTap { container =>
      MonacoEditor.create(container).flatMap { editor =>
        editor.discreteModel.switchMap(model => model.discreteValue.evalMap(wk.send))
          .compile.drain.background
      }
    }
  }

  def rightEditor(compilerWk: WorkerConnection[String, String], explorerWk: WorkerConnection[Uint8Array, String]): Resource[IO, HtmlElement[IO]] = {
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
              getUintArray >>= explorerWk.send
            }
          }
        })
      )},
      div(idAttr := "right-editor").flatTap { container =>
        MonacoDiffEditor.create(container).flatMap { editor =>
          val setModified = compilerWk.stream.foreach { content =>
            editor.model.flatMap { model =>
              model.modified.setValue(content)
            }
          }
          val setOriginal = explorerWk.stream.foreach { content =>
            editor.model.flatMap { model =>
              model.original.setValue(content)
            }
          }
          setModified.merge(setOriginal).compile.drain.background
        }
      }
    )
  }

}
