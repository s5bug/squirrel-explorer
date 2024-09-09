package tf.bug.fe

import cats.data.State
import cats.effect.kernel.Concurrent
import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.{Channel, SignallingRef}
import typings.monacoEditor.mod.IDisposable
import typings.monacoEditor.mod.editor.IStandaloneEditorConstructionOptions

opaque type MonacoEditor = typings.monacoEditor.mod.editor.IStandaloneCodeEditor
opaque type MonacoModel = typings.monacoEditor.mod.editor.ITextModel

object MonacoEditor {

  def create(container: fs2.dom.HtmlElement[IO], readOnly: Boolean = false): Resource[IO, MonacoEditor] = {
    val opts: IStandaloneEditorConstructionOptions = IStandaloneEditorConstructionOptions()
    opts.automaticLayout = true
    opts.readOnly = readOnly

    Resource.make[IO, typings.monacoEditor.mod.editor.IStandaloneCodeEditor](
      IO.delay(typings.monacoEditor.mod.editor.create(container.asInstanceOf, opts))
    )(ed => IO.delay(ed.dispose()))
  }

  private def modelStateHolder(editor: MonacoEditor): Resource[IO, SignallingRef[IO, MonacoModel]] =
    IO.delay(editor.getModel().asInstanceOf[MonacoModel]).toResource.flatMap { current =>
      Dispatcher.sequential[IO].flatMap { dispatcher =>
        SignallingRef.of[IO, MonacoModel](current).toResource.flatMap { r =>
          val acquire: IO[IDisposable] = IO.delay {
            editor.onDidChangeModel { ev =>
              dispatcher.unsafeRunAndForget(r.set(editor.getModel().asInstanceOf[MonacoModel]))
            }
          }
          val release = (i: IDisposable) => IO.delay(i.dispose())
          Resource.make(acquire)(release).as(r)
        }
      }
    }

  extension(editor: MonacoEditor) {

    // an IStandaloneCodeEditor will always have an ITextModel
    def getModel: IO[MonacoModel] = IO.delay(editor.getModel().asInstanceOf[MonacoModel])

    def discreteModel: Stream[IO, MonacoModel] =
      Stream.resource(modelStateHolder(editor)).flatMap(_.discrete)

  }

}

object MonacoModel {

  private def valueStateHolder(model: MonacoModel): Resource[IO, SignallingRef[IO, String]] =
    IO.delay(model.getValue()).toResource.flatMap { current =>
      Dispatcher.sequential[IO].flatMap { dispatcher =>
        SignallingRef.of[IO, String](current).toResource.flatMap { r =>
          val acquire: IO[IDisposable] = IO.delay {
            model.onDidChangeContent { ev =>
              dispatcher.unsafeRunAndForget(r.set(model.getValue()))
            }
          }
          val release = (i: IDisposable) => IO.delay(i.dispose())
          Resource.make(acquire)(release).as(r)
        }
      }
    }

  extension (model: MonacoModel) {

    def discreteValue: Stream[IO, String] =
      Stream.resource(valueStateHolder(model)).flatMap(_.discrete)

    def setValue(s: String): IO[Unit] =
      IO.delay(model.setValue(s))

  }

}
