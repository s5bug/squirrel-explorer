package tf.bug.fe

import calico.*
import calico.html.io.{*, given}
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import fs2.*
import fs2.dom.*
import scodec.{Attempt, DecodeResult}

object SquirrelExplorer extends IOWebApp {

  override def render: Resource[IO, HtmlElement[IO]] =
    for {
      // TODO make this a request/response API rather than request-stream/response-stream
      wk <- WorkerConnection.default
      p <- program(wk)
    } yield p

  def program(wk: WorkerConnection): Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "grid-container",
      leftEditor(wk),
      topRightEditor(wk),
      bottomRightEditor
    )
  }

  def leftEditor(wk: WorkerConnection): Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "left-editor"
    ).flatTap { container =>
      MonacoEditor.create(container).flatMap { editor =>
        editor.discreteModel.switchMap(model => model.discreteValue.evalMap(wk.send))
          .compile.drain.background
      }
    }
  }

  def topRightEditor(wk: WorkerConnection): Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "topright-editor"
    ).flatTap { container =>
      MonacoEditor.create(container, readOnly = true).flatMap { editor =>
        wk.stream.foreach { content =>
          editor.getModel.flatMap { model =>
            model.setValue(content)
          }
        }.compile.drain.background
      }
    }
  }

  def bottomRightEditor: Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "bottomright-editor"
    ).flatTap(MonacoEditor.create(_, readOnly = true))
  }

}
