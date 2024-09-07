package tf.bug

import calico.*
import calico.html.io.{*, given}
import cats.syntax.all.*
import cats.effect.{IO, Resource}
import fs2.*
import fs2.concurrent.Channel
import fs2.dom.*
import scodec.{Attempt, DecodeResult}
import tf.bug.cnut.Cnut

object SquirrelExplorer extends IOWebApp {

  override def render: Resource[IO, HtmlElement[IO]] =
    for {
      sq <- WasmApi.get.map(Sq.of).toResource
      ob <- sq.outBuffer
      vm <- sq.sqVm
      cc <- Channel.unbounded[IO, String].toResource
      p <- program(sq, ob, vm, cc)
    } yield p

  def program(sq: Sq, ob: Sq.OutBuffer, vm: Sq.SqVm, contentChan: Channel[IO, String]): Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "grid-container",
      leftEditor(contentChan),
      topRightEditor(sq, vm, ob, contentChan),
      bottomRightEditor
    )
  }

  def leftEditor(contentChan: Channel[IO, String]): Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "left-editor"
    ).flatTap { container =>
      MonacoEditor.create(container).flatMap { editor =>
        editor.model.discrete.switchMap(model => model.value.discrete.through(contentChan.sendAll))
          .compile.drain.background
      }
    }
  }

  def topRightEditor(sq: Sq, vm: Sq.SqVm, outBuf: Sq.OutBuffer, contentChan: Channel[IO, String]): Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "topright-editor"
    ).flatTap { container =>
      MonacoEditor.create(container, readOnly = true).flatMap { editor =>
        editor.model.discrete.switchMap(model => contentChan.stream.foreach { content =>
          sq.compileAndSerialize(vm, content, None, outBuf).flatMap { serialized =>
            val parsed = Cnut.cnut.decode(serialized.bits)
            parsed match {
              case Attempt.Successful(DecodeResult(v, r)) if r.isEmpty =>
                model.value.set(v.doc.renderTrim(0))
              case _ => IO.unit
            }
          }
        }).compile.drain.background
      }
    }
  }

  def bottomRightEditor: Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "bottomright-editor"
    ).flatTap(MonacoEditor.create(_, readOnly = true))
  }

}
