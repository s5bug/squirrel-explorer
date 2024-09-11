package tf.bug.fe

import cats.data.State
import cats.effect.kernel.Concurrent
import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.{Channel, SignallingRef}
import typings.monacoEditor.mod.IDisposable
import typings.monacoEditor.mod.editor.{IStandaloneDiffEditorConstructionOptions, IStandaloneEditorConstructionOptions}

opaque type MonacoEditor = typings.monacoEditor.mod.editor.IStandaloneCodeEditor
opaque type MonacoDiffEditor = typings.monacoEditor.mod.editor.IStandaloneDiffEditor
opaque type MonacoModel = typings.monacoEditor.mod.editor.ITextModel
opaque type MonacoDiffModel = typings.monacoEditor.mod.editor.IDiffEditorModel

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

object MonacoDiffEditor {

  def create(container: fs2.dom.HtmlElement[IO], readOnly: Boolean = true): Resource[IO, MonacoDiffEditor] = {
    val opts: IStandaloneDiffEditorConstructionOptions = IStandaloneDiffEditorConstructionOptions()
    opts.automaticLayout = true
    opts.readOnly = readOnly
    opts.maxComputationTime = 20 * 60 * 1000 // 20 minutes

    Resource.make[IO, typings.monacoEditor.mod.editor.IStandaloneDiffEditor](
      MonacoModel.create.flatMap { originalModel =>
        MonacoModel.create.flatMap { modifiedModel =>
          IO.delay {
            val ed = typings.monacoEditor.mod.editor.createDiffEditor(container.asInstanceOf, opts)
            val diffMod = typings.monacoEditor.mod.editor.IDiffEditorModel(original = originalModel, modified = modifiedModel)
            ed.setModel(diffMod)
            ed
          }
        }
      }
    )(ed => IO.delay(ed.dispose()))
  }

  extension(editor: MonacoDiffEditor) {

    def setOriginalDiffCalc(value: String): IO[Unit] = IO.async[Unit] { cb =>
      IO.delay {
        val disposeCb: IDisposable = editor.onDidUpdateDiff(ev => {
          cb(Right(()))
        })
        editor.getOriginalEditor().setValue(value)
        Some(IO.delay(disposeCb.dispose()))
      }
    }

    def setModifiedDiffCalc(value: String): IO[Unit] = IO.async[Unit] { cb =>
      IO.delay {
        val disposeCb: IDisposable = editor.onDidUpdateDiff(ev => {
          cb(Right(()))
        })
        editor.getModifiedEditor().setValue(value)
        Some(IO.delay(disposeCb.dispose()))
      }
    }

    def model: IO[MonacoDiffModel] = IO.delay(editor.getModel().asInstanceOf[MonacoDiffModel])

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

  def create: IO[MonacoModel] = IO.delay {
    typings.monacoEditor.mod.editor.createModel("")
  }

  extension (model: MonacoModel) {

    def discreteValue: Stream[IO, String] =
      Stream.resource(valueStateHolder(model)).flatMap(_.discrete)

    def setValue(s: String): IO[Unit] =
      IO.delay(model.setValue(s))

  }

}

object MonacoDiffModel {

  extension (model: MonacoDiffModel) {

    def modified: MonacoModel = model.modified
    def original: MonacoModel = model.original

  }

}
