package tf.bug.fe

import cats.effect.*
import cats.effect.std.Dispatcher
import fs2.*
import fs2.concurrent.Channel
import org.scalajs.dom.{Worker, WorkerOptions, WorkerType}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

abstract class WorkerConnection {

  def send(code: String): IO[Unit]

  def stream: Stream[IO, String]

}

object WorkerConnection {

  @js.native
  @JSImport("scalajs:worker.js?worker&url", JSImport.Default)
  private val workerJsUrl: String = js.native

  def default: Resource[IO, WorkerConnection] =
    Dispatcher.sequential[IO].flatMap { disp =>
      Channel.unbounded[IO, String].toResource.flatMap { chan =>
        val opts = new WorkerOptions {}
        opts.`type` = WorkerType.module
        val internal = new Worker(workerJsUrl, opts)

        val acquire = IO.delay {
          internal.onmessage = ev => disp.unsafeRunAndForget(chan.send(ev.data.asInstanceOf[String]))
          new WorkerConnectionWrapper(internal, chan.stream)
        }

        Resource.make(acquire)(_.terminate)
      }
    }
  
  private final class WorkerConnectionWrapper(val internal: Worker, val stream: Stream[IO, String]) extends WorkerConnection {
    
    def terminate: IO[Unit] = IO.delay {
      internal.onmessage = js.undefined.asInstanceOf
      internal.terminate()
    }

    override def send(code: String): IO[Unit] =
      IO.delay(internal.postMessage(code))
    
  }

}
