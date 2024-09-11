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

  def send(element: A): IO[Unit]

  def stream: Stream[IO, B]

}

object WorkerConnection {

  @js.native
  @JSImport("scalajs:compilerworker.js?worker&url", JSImport.Default)
  private val compilerWorkerJsUrl: String = js.native
  
  @js.native
  @JSImport("scalajs:explorerworker.js?worker&url", JSImport.Default)
  private val explorerWorkerJsUrl: String = js.native

  private def ofUrl[A, B](url: String): Resource[IO, WorkerConnection[A, B]] =
    Dispatcher.sequential[IO].flatMap { disp =>
      Channel.unbounded[IO, B].toResource.flatMap { chan =>
        val opts = new WorkerOptions {}
        opts.`type` = WorkerType.module
        val internal = new Worker(url, opts)

        val acquire = IO.delay {
          internal.onmessage = ev => disp.unsafeRunAndForget(chan.send(ev.data.asInstanceOf[B]))
          new WorkerConnectionWrapper(internal, chan.stream)
        }

        Resource.make(acquire)(_.terminate)
      }
    }

  def ofCompiler: Resource[IO, WorkerConnection[String, String]] =
    ofUrl(compilerWorkerJsUrl)

  def ofExplorer: Resource[IO, WorkerConnection[Uint8Array, String]] =
    ofUrl(explorerWorkerJsUrl)
  
  private final class WorkerConnectionWrapper[-A, +B](val internal: Worker, val stream: Stream[IO, B]) extends WorkerConnection[A, B] {
    
    def terminate: IO[Unit] = IO.delay {
      internal.onmessage = js.undefined.asInstanceOf
      internal.terminate()
    }

    override def send(code: A): IO[Unit] =
      IO.delay(internal.postMessage(code.asInstanceOf[js.Any]))
    
  }

}
