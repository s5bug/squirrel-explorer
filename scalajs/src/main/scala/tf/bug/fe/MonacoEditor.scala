package tf.bug.fe

import cats.data.State
import cats.effect.kernel.Concurrent
import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.{Channel, SignallingRef}
import scala.scalajs.js.Promise
import typings.monacoEditor.mod
import typings.monacoEditor.mod.{CancellationToken, IDisposable, Thenable}
import typings.monacoEditor.mod.editor.{IStandaloneDiffEditorConstructionOptions, IStandaloneEditorConstructionOptions}
import typings.monacoEditor.mod.languages.InlayHintList

opaque type MonacoEditor = typings.monacoEditor.mod.editor.IStandaloneCodeEditor
opaque type MonacoDiffEditor = typings.monacoEditor.mod.editor.IStandaloneDiffEditor
opaque type MonacoModel = typings.monacoEditor.mod.editor.ITextModel
opaque type MonacoDiffModel = typings.monacoEditor.mod.editor.IDiffEditorModel
opaque type MonacoDiffEditorViewModel = typings.monacoEditor.mod.editor.IDiffEditorViewModel
opaque type InlayHintsProvider = typings.monacoEditor.mod.languages.InlayHintsProvider

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

  def create(container: fs2.dom.HtmlElement[IO], lang: String, name: String, readOnly: Boolean = true): Resource[IO, MonacoDiffEditor] = {
    val opts: IStandaloneDiffEditorConstructionOptions = IStandaloneDiffEditorConstructionOptions()
    opts.automaticLayout = true
    opts.readOnly = readOnly
    opts.maxComputationTime = 20 * 60 * 1000 // 20 minutes
    opts.renderValidationDecorations = typings.monacoEditor.monacoEditorStrings.on

    Resource.make[IO, typings.monacoEditor.mod.editor.IStandaloneDiffEditor](
      MonacoModel.create(lang, mod.Uri.parse(s"inmemory://${name}_original")).flatMap { originalModel =>
        MonacoModel.create(lang, mod.Uri.parse(s"inmemory://${name}_modified")).flatMap { modifiedModel =>
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

  import MonacoDiffEditorViewModel.waitDiff
  
  extension(editor: MonacoDiffEditor) {
    
    def setOriginalResult(
      viewModel: MonacoDiffEditorViewModel,
      value: RenderResult,
      hints: Ref[IO, scalajs.js.Array[typings.monacoEditor.mod.languages.InlayHint]]
    ): IO[Unit] = {
      val setValue = IO.delay { editor.getOriginalEditor().setValue(value.rawText) }
      val setMarks = model.flatMap { m =>
        IO.delay {
          typings.monacoEditor.mod.editor.setModelMarkers(m.original, "cnut", value.markers)
        }
      }
      val setHints = hints.set(value.hints)
      (setHints, setValue >> viewModel.waitDiff, setMarks).tupled.void
    }

    def setModifiedResult(
      viewModel: MonacoDiffEditorViewModel,
      value: RenderResult,
      hints: Ref[IO, scalajs.js.Array[typings.monacoEditor.mod.languages.InlayHint]]
    ): IO[Unit] = {
      val setValue = IO.delay { editor.getModifiedEditor().setValue(value.rawText) }
      val setMarks = model.flatMap { m =>
        IO.delay {
          typings.monacoEditor.mod.editor.setModelMarkers(m.modified, "cnut", value.markers)
        }
      }
      val setHints = hints.set(value.hints)
      (setHints, setValue >> viewModel.waitDiff, setMarks).tupled.void
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

  def create(lang: String, uri: mod.Uri): IO[MonacoModel] = IO.delay {
    typings.monacoEditor.mod.editor.createModel("", lang, uri)
  }

  extension (model: MonacoModel) {

    inline def id: String = model.id

    def discreteValue: Stream[IO, String] =
      Stream.resource(valueStateHolder(model)).flatMap(_.discrete)

    def setValue(s: String): IO[Unit] =
      IO.delay(model.setValue(s))

    def setLanguage(language: String): IO[Unit] =
      IO(typings.monacoEditor.mod.editor.setModelLanguage(model, language))

    def setMarkers(marks: scalajs.js.Array[typings.monacoEditor.mod.editor.IMarkerData]): IO[Unit] =
      IO(typings.monacoEditor.mod.editor.setModelMarkers(model, "owner", marks))

  }

}

object MonacoDiffModel {

  extension (model: MonacoDiffModel) {

    def modified: MonacoModel = model.modified
    def original: MonacoModel = model.original

  }

}

object MonacoDiffEditorViewModel {

  def of(diffEditor: MonacoDiffEditor): Resource[IO, MonacoDiffEditorViewModel] = {
    val acquire: IO[MonacoDiffEditorViewModel] = IO.delay {
      diffEditor.createViewModel(diffEditor.getModel().asInstanceOf)
    }
    val release: MonacoDiffEditorViewModel => IO[Unit] = vm => IO.delay(vm.dispose())
    Resource.make(acquire)(release)
  }

  extension (vm: MonacoDiffEditorViewModel) {
    def waitDiff: IO[Unit] =
      IO.fromPromise(IO.delay {
        vm.waitForDiff()
      })
  }

}

object InlayHintsProvider {

  def register(lang: String)(func: (MonacoModel, typings.monacoEditor.mod.Range, typings.monacoEditor.mod.CancellationToken) =>
    IO[typings.monacoEditor.mod.languages.InlayHintList]): Resource[IO, InlayHintsProvider] = {
    create(func).flatTap { prov =>
      val acquire = IO {
        typings.monacoEditor.mod.languages.registerInlayHintsProvider(lang, prov)
      }
      val release = (disp: typings.monacoEditor.mod.IDisposable) => IO(disp.dispose())
      Resource.make(acquire)(release)
    }
  }

  def create(func: (MonacoModel, typings.monacoEditor.mod.Range, typings.monacoEditor.mod.CancellationToken) =>
    IO[typings.monacoEditor.mod.languages.InlayHintList]): Resource[IO, InlayHintsProvider] = {
    Dispatcher.sequential[IO].map { disp =>
      new InlayHintsProvider {
        override def provideInlayHints(model: mod.editor.ITextModel, range: mod.Range, token: mod.CancellationToken): mod.languages.ProviderResult[InlayHintList] =
          disp.unsafeToPromise(func(model, range, token))
            .asInstanceOf[typings.std.Promise[InlayHintList]]
            .asInstanceOf
      }
    }
  }

}
