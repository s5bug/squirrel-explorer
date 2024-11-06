package tf.bug.fe

import cats.effect.std.Mutex
import cats.effect.{IO, Resource}
import org.scalajs.dom.{Worker, WorkerOptions, WorkerType}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

abstract class CompilerWorkerThread {

  def compileClosure(input: String): IO[Either[String, Uint8Array]]

}

object CompilerWorkerThread {

  @js.native
  @JSImport("scalajs:compilerworker.js?worker&url", JSImport.Default)
  private val compilerWorkerJsUrl: String = js.native

  def default: Resource[IO, CompilerWorkerThread] = {
    val acquire = Mutex[IO].flatMap { mut =>
      IO.delay {
        val opts = new WorkerOptions {}
        opts.`type` = WorkerType.module
        val internal = new Worker(compilerWorkerJsUrl, opts)

        new OfWorker(internal, mut)
      }
    }

    Resource.make(acquire)(_.terminate)
  }

  private final class OfWorker(val internal: Worker, val lock: Mutex[IO]) extends CompilerWorkerThread {

    override def compileClosure(input: String): IO[Either[String, Uint8Array]] =
      lock.lock.use { _ =>
        IO.async_[Either[String, Uint8Array]] { cb =>
          internal.onmessage = e => e.data match {
            case s: String => cb(Right(Left(s)))
            case u: Uint8Array => cb(Right(Right(u)))
          }
          internal.postMessage(input)
        }
      }.uncancelable
      
    def terminate: IO[Unit] = lock.lock.use { _ =>
      IO(internal.terminate())
    }.uncancelable

  }

}
