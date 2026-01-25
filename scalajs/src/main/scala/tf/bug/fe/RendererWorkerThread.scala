package tf.bug.fe

import cats.effect.*
import cats.effect.std.Mutex
import com.github.plokhotnyuk.jsoniter_scala
import org.scalajs.dom.{Worker, WorkerOptions, WorkerType}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array
import tf.bug.worker.{RenderCommand, RenderEncodingSjis, RenderLineInfos}

abstract class RendererWorkerThread {

  def setRenderLineInfos(value: Boolean): IO[Unit]
  def setEncodingSjis(value: Boolean): IO[Unit]

  def tryToRender(cnut: Uint8Array): IO[Either[String, RenderResult]]

}

object RendererWorkerThread {

  @js.native
  @JSImport("scalajs:rendererworker.js?worker&url", JSImport.Default)
  private val rendererWorkerJsUrl: String = js.native

  def default: Resource[IO, RendererWorkerThread] = {
    val acquire = Mutex[IO].flatMap { mut =>
      IO.delay {
        val opts = new WorkerOptions {}
        opts.`type` = WorkerType.module
        val internal = new Worker(rendererWorkerJsUrl, opts)

        new OfWorker(internal, mut)
      }
    }

    Resource.make(acquire)(_.terminate)
  }

  private final class OfWorker(val internal: Worker, val lock: Mutex[IO]) extends RendererWorkerThread {

    override def setRenderLineInfos(value: Boolean): IO[Unit] = {
      val j = jsoniter_scala.core.writeToString[RenderCommand](RenderLineInfos(value))
      IO(internal.postMessage(j))
    }

    override def setEncodingSjis(value: Boolean): IO[Unit] = {
      val j = jsoniter_scala.core.writeToString[RenderCommand](RenderEncodingSjis(value))
      IO(internal.postMessage(j))
    }

    override def tryToRender(cnut: Uint8Array): IO[Either[String, RenderResult]] =
      lock.lock.use { _ =>
        IO.async_[Either[String, RenderResult]] { cb =>
          // FIXME this is really ugly but I don't know a better way
          val stuff: js.Array[String] = js.Array()
          internal.onmessage = e => e.data match {
            case s: String =>
              if s.startsWith("[error] ") then cb(Right(Left(s.substring(8))))
              else {
                stuff.push(s)
                if stuff.length >= 3 then {
                  cb(Right(Right(
                    RenderResult(
                      stuff(0),
                      js.JSON.parse(stuff(1)).asInstanceOf,
                      js.JSON.parse(stuff(2)).asInstanceOf
                    )
                  )))
                }
              }
          }
          internal.postMessage(cnut)
        }
      }.uncancelable

    def terminate: IO[Unit] = lock.lock.use { _ =>
      println("terminating")
      IO(internal.terminate())
    }.uncancelable

  }

}
