package tf.bug

import calico.*
import calico.html.io.{*, given}
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.SignallingRef
import fs2.dom.*
import tf.bug.fe.*

object SquirrelExplorer extends IOWebApp {

  override def render: Resource[IO, HtmlElement[IO]] = {
    (
      CompilerWorkerThread.default,
      RendererWorkerThread.default,
      RendererWorkerThread.default,
      SignallingRef.of[IO, RenderResult](RenderResult.empty).toResource,
      SignallingRef.of[IO, RenderResult](RenderResult.empty).toResource
    ).parFlatMapN { (compilerWk, compiledRenderWk, uploadedRenderWk, compiledResult, uploadedResult) =>
      Resource.suspend(IO.fromPromise(IO.delay { scalajs.js.dynamicImport {
        SquirrelExplorerFrontend.program(compilerWk, compiledRenderWk, uploadedRenderWk, compiledResult, uploadedResult)
      }}))
    }
  }

}
