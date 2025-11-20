package tf.bug.fe

import cats.data.State
import cats.effect.kernel.Concurrent
import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.{Channel, SignallingRef}
import scala.scalajs.js.Promise
import typings.monacoEditor.esmVsEditorEditorDotapiMod as monaco

opaque type MonacoEditor = monaco.editor.IStandaloneCodeEditor
opaque type MonacoDiffEditor = monaco.editor.IStandaloneDiffEditor
opaque type MonacoModel = monaco.editor.ITextModel
opaque type MonacoDiffModel = monaco.editor.IDiffEditorModel
opaque type MonacoDiffEditorViewModel = monaco.editor.IDiffEditorViewModel
opaque type InlayHintsProvider = monaco.languages.InlayHintsProvider

object MonacoEditor {

  def create(container: fs2.dom.HtmlElement[IO], language: Option[String] = None, readOnly: Boolean = false): Resource[IO, MonacoEditor] = {
    val opts: monaco.editor.IStandaloneEditorConstructionOptions =
      monaco.editor.IStandaloneEditorConstructionOptions()
    opts.automaticLayout = true
    opts.language = language.getOrElse(scalajs.js.undefined)
    opts.readOnly = readOnly

    Resource.make[IO, MonacoEditor](
      IO.delay(monaco.editor.create(container.asInstanceOf, opts))
    )(ed => IO.delay(ed.dispose()))
  }

  private def modelStateHolder(editor: MonacoEditor): Resource[IO, SignallingRef[IO, MonacoModel]] =
    IO.delay(editor.getModel().asInstanceOf[MonacoModel]).toResource.flatMap { current =>
      Dispatcher.sequential[IO].flatMap { dispatcher =>
        SignallingRef.of[IO, MonacoModel](current).toResource.flatMap { r =>
          val acquire: IO[monaco.IDisposable] = IO.delay {
            editor.onDidChangeModel { ev =>
              dispatcher.unsafeRunAndForget(r.set(editor.getModel().asInstanceOf[MonacoModel]))
            }
          }
          val release = (i: monaco.IDisposable) => IO.delay(i.dispose())
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
    val opts: monaco.editor.IStandaloneDiffEditorConstructionOptions =
      monaco.editor.IStandaloneDiffEditorConstructionOptions()
    opts.automaticLayout = true
    opts.readOnly = readOnly
    opts.maxComputationTime = 20 * 60 * 1000 // 20 minutes
    opts.renderValidationDecorations = typings.monacoEditor.monacoEditorStrings.on

    Resource.make[IO, MonacoDiffEditor](
      MonacoModel.create(lang, monaco.Uri.parse(s"inmemory://${name}_original")).flatMap { originalModel =>
        MonacoModel.create(lang, monaco.Uri.parse(s"inmemory://${name}_modified")).flatMap { modifiedModel =>
          IO.delay {
            val ed = monaco.editor.createDiffEditor(container.asInstanceOf, opts)
            val diffMod = monaco.editor.IDiffEditorModel(original = originalModel, modified = modifiedModel)
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
      hints: Ref[IO, scalajs.js.Array[monaco.languages.InlayHint]]
    ): IO[Unit] = {
      val setValue = IO.delay { editor.getOriginalEditor().setValue(value.rawText) }
      val setMarks = model.flatMap { m =>
        IO.delay {
          monaco.editor.setModelMarkers(m.original, "cnut", value.markers)
        }
      }
      val setHints = hints.set(value.hints)
      (setHints, setValue >> viewModel.waitDiff, setMarks).tupled.void
    }

    def setModifiedResult(
      viewModel: MonacoDiffEditorViewModel,
      value: RenderResult,
      hints: Ref[IO, scalajs.js.Array[monaco.languages.InlayHint]]
    ): IO[Unit] = {
      val setValue = IO.delay { editor.getModifiedEditor().setValue(value.rawText) }
      val setMarks = model.flatMap { m =>
        IO.delay {
          monaco.editor.setModelMarkers(m.modified, "cnut", value.markers)
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
          val acquire: IO[monaco.IDisposable] = IO.delay {
            model.onDidChangeContent { ev =>
              dispatcher.unsafeRunAndForget(r.set(model.getValue()))
            }
          }
          val release = (i: monaco.IDisposable) => IO.delay(i.dispose())
          Resource.make(acquire)(release).as(r)
        }
      }
    }

  def create(lang: String, uri: monaco.Uri): IO[MonacoModel] = IO.delay {
    monaco.editor.createModel("", lang, uri)
  }

  extension (model: MonacoModel) {

    inline def id: String = model.id

    def discreteValue: Stream[IO, String] =
      Stream.resource(valueStateHolder(model)).flatMap(_.discrete)

    def setValue(s: String): IO[Unit] =
      IO.delay(model.setValue(s))

    def setLanguage(language: String): IO[Unit] =
      IO(monaco.editor.setModelLanguage(model, language))

    def setMarkers(marks: scalajs.js.Array[monaco.editor.IMarkerData]): IO[Unit] =
      IO(monaco.editor.setModelMarkers(model, "owner", marks))

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

  def register(lang: String)(func: (MonacoModel, monaco.Range, monaco.CancellationToken) =>
    IO[monaco.languages.InlayHintList]): Resource[IO, InlayHintsProvider] = {
    create(func).flatTap { prov =>
      val acquire = IO {
        monaco.languages.registerInlayHintsProvider(lang, prov)
      }
      val release = (disp: monaco.IDisposable) => IO(disp.dispose())
      Resource.make(acquire)(release)
    }
  }

  def create(func: (MonacoModel, monaco.Range, monaco.CancellationToken) =>
    IO[monaco.languages.InlayHintList]): Resource[IO, InlayHintsProvider] = {
    Dispatcher.sequential[IO].map { disp =>
      new InlayHintsProvider {
        override def provideInlayHints(model: monaco.editor.ITextModel, range: monaco.Range, token: monaco.CancellationToken): monaco.languages.ProviderResult[monaco.languages.InlayHintList] =
          disp.unsafeToPromise(func(model, range, token))
            .asInstanceOf[typings.std.Promise[monaco.languages.InlayHintList]]
            .asInstanceOf
      }
    }
  }

}
