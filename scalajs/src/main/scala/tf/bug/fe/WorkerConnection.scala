package tf.bug.fe

import cats.effect.*
import cats.effect.std.Dispatcher
import fs2.*
import fs2.concurrent.Channel
import org.scalajs.dom.{Worker, WorkerOptions, WorkerType}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

abstract class WorkerConnection[-A, +B] {

  def callResponse(element: A): IO[B]

}

object WorkerConnection {

  @js.native
  @JSImport("scalajs:compilerworker.js?worker&url", JSImport.Default)
  private val compilerWorkerJsUrl: String = js.native

  @js.native
  @JSImport("scalajs:explorerworker.js?worker&url", JSImport.Default)
  private val explorerWorkerJsUrl: String = js.native

  private def ofUrl[A, B](url: String): Resource[IO, WorkerConnection[A, B]] = {
    val acquire = IO.delay {
      val opts = new WorkerOptions {}
      opts.`type` = WorkerType.module
      val internal = new Worker(url, opts)

      new WorkerConnectionWrapper(internal)
    }

    Resource.make(acquire)(_.terminate)
  }

  def ofCompiler: Resource[IO, WorkerConnection[String, String]] =
    ofUrl(compilerWorkerJsUrl)

  def ofExplorer: Resource[IO, WorkerConnection[Uint8Array, String]] =
    ofUrl(explorerWorkerJsUrl)
  
  private final class WorkerConnectionWrapper[-A, +B](val internal: Worker) extends WorkerConnection[A, B] {

    override def callResponse(element: A): IO[B] = IO.async_[B] { cb =>
      internal.onmessage = m => cb(Right(m.data.asInstanceOf[B]))
      internal.postMessage(element.asInstanceOf[js.Any])
    }.uncancelable

    def terminate: IO[Unit] = IO.delay {
      internal.onmessage = js.undefined.asInstanceOf
      internal.terminate()
    }

  }

}
