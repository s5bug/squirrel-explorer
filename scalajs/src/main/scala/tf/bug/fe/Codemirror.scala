package tf.bug.fe

import cats.data.State
import cats.effect.kernel.Concurrent
import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.{Channel, SignallingRef}
import scala.scalajs.js
import typings.codemirror.mod as codemirror
import typings.codemirrorMerge.mod as codemirrorMerge
import typings.codemirrorState.mod as codemirrorState
import typings.codemirrorView.mod as codemirrorView

import codemirrorState.Extension
import codemirrorState.Facet

object Codemirror {
  inline def basicSetup: Extension = codemirror.basicSetup
  inline def minimalSetup: Extension = codemirror.minimalSetup

  // TODO maybe replace the minimap so this sucks less
  final case class MinimapConfig(create: CodemirrorView => IO[fs2.dom.HtmlElement[IO]])
  def minimap(dispatcher: Dispatcher[IO])(of: MinimapConfig): Extension = typings.replitCodemirrorMinimap.mod.showMinimap.of {
    typings.replitCodemirrorMinimap.mod.MinimapConfig { (ev: codemirrorView.EditorView) =>
      of.create(ev.asInstanceOf).syncStep(Int.MaxValue).unsafeRunSync() match {
        case Left(io) => throw new IllegalArgumentException("could not create minimap element synchronously")
        case Right(elem) => typings.replitCodemirrorMinimap.anon.Dom(elem.asInstanceOf)
      }
    }
  }
}

type CodemirrorStateConfig = codemirrorState.EditorStateConfig
inline final def CodemirrorStateConfig = codemirrorState.EditorStateConfig
export codemirrorState.EditorStateConfig.MutableBuilder as EditorStateConfigMutableBuilder

type CodemirrorViewConfig = codemirrorView.EditorViewConfig
inline final def CodemirrorViewConfig = codemirrorView.EditorViewConfig
export codemirrorView.EditorViewConfig.MutableBuilder as EditorViewConfigMutableBuilder

opaque type CodemirrorState = codemirrorState.EditorState

object CodemirrorState {
  inline def allowMultipleSelections: Facet[Boolean, Boolean] = codemirrorState.EditorState.allowMultipleSelections
  inline def readOnly: Facet[Boolean, Boolean] = codemirrorState.EditorState.readOnly

  extension(cms: CodemirrorState) {
    inline def doc: codemirrorState.Text = cms.doc
  }
}

opaque type CodemirrorView = codemirrorView.EditorView

object CodemirrorView {
  def apply(config: CodemirrorViewConfig): Resource[IO, CodemirrorView] = {
    Resource.make(IO {
      new codemirrorView.EditorView(config)
    })(ev => IO {
      ev.destroy()
    })
  }

  inline def editable: Facet[Boolean, Boolean] = codemirrorView.EditorView.editable
  inline def lineNumbers: Extension = codemirrorView.lineNumbers()
  def updateListener(dispatcher: Dispatcher[IO])(of: codemirrorView.ViewUpdate => IO[Unit]): Extension =
    codemirrorView.EditorView.updateListener.of(vu => of(vu).syncStep(64).unsafeRunSync() match {
      case Left(io) => dispatcher.unsafeRunAndForget(io)
      case Right(()) => ()
    })

  extension(cmv: CodemirrorView) {
    def dispatch(args: codemirrorState.TransactionSpec*): IO[Unit] = IO { cmv.dispatch(args*) }
    inline def dom: fs2.dom.HtmlElement[IO] = (cmv.dom: org.scalajs.dom.HTMLElement).asInstanceOf
    inline def state: CodemirrorState = cmv.state
  }
}

opaque type CodemirrorMergeView = codemirrorMerge.MergeView

object CodemirrorMergeView {
  def apply(
    a: codemirrorState.EditorStateConfig,
    b: codemirrorState.EditorStateConfig,
  ): Resource[IO, CodemirrorMergeView] = {
    Resource.make(IO {
      new codemirrorMerge.MergeView(
        codemirrorMerge.DirectMergeConfig(a, b)
        // TODO implement smart structural diffing between compiled files
        // this will allow sending smaller changesets to the merge viewer
//          .setDiffConfig(codemirrorMerge.DiffConfig().setScanLimit(8192))
      )
    })(ev => IO {
      ev.destroy()
    })
  }
  
  extension(cmmv: CodemirrorMergeView) {
    inline def dom: fs2.dom.HtmlElement[IO] = (cmmv.dom: org.scalajs.dom.HTMLElement).asInstanceOf

    inline def a: CodemirrorView = cmmv.a
    inline def b: CodemirrorView = cmmv.b
  }
}
