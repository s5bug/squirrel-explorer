package tf.bug

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

  extension(editor: MonacoEditor) {

    // an IStandaloneCodeEditor will always have an ITextModel
    private def getModelFixed(): MonacoModel = editor.getModel().asInstanceOf[MonacoModel]

    def model: SignallingRef[IO, MonacoModel] = new SignallingRef[IO, MonacoModel] {

      override def access: IO[(MonacoModel, MonacoModel => IO[Boolean])] = IO.delay {
        val snapshot = editor.getModelFixed()
        val setter = (a: MonacoModel) =>
          IO.delay {
            if (editor.getModel() eq snapshot) {
              editor.setModel(a)
              true
            } else false
          }
        (snapshot, setter)
      }

      override def tryUpdate(f: MonacoModel => MonacoModel): IO[Boolean] =
        update(f).as(true)

      override def tryModify[B](f: MonacoModel => (MonacoModel, B)): IO[Option[B]] =
        modify(f).map(Some(_))

      override def update(f: MonacoModel => MonacoModel): IO[Unit] =
        IO.delay(editor.setModel(f(editor.getModelFixed())))

      override def modify[B](f: MonacoModel => (MonacoModel, B)): IO[B] =
        IO.delay {
          val (a, b) = f(editor.getModelFixed())
          editor.setModel(a)
          b
        }

      override def tryModifyState[B](state: State[MonacoModel, B]): IO[Option[B]] =
        tryModify(state.run(_).value)

      override def modifyState[B](state: State[MonacoModel, B]): IO[B] =
        modify(state.run(_).value)

      override def set(a: MonacoModel): IO[Unit] =
        IO.delay(editor.setModel(a))

      override def getAndDiscreteUpdates(implicit F: Concurrent[IO]): Resource[IO, (MonacoModel, Stream[IO, MonacoModel])] = {
        Dispatcher.sequential[IO].flatMap { dispatcher =>
          Channel.unbounded[IO, MonacoModel].toResource.flatMap { c =>
            val acquire: IO[IDisposable] = IO.delay {
              editor.onDidChangeModel { ev =>
                dispatcher.unsafeRunAndForget(c.send(editor.getModelFixed()))
              }
            }
            val release = (i: IDisposable) => IO.delay(i.dispose())
            (this.get.toResource, Resource.make(acquire)(release).as(c.stream)).tupled
          }
        }
      }

      override def discrete: Stream[IO, MonacoModel] =
        Stream.resource(getAndDiscreteUpdates).flatMap { case (h, t) => Stream.emit(h) ++ t }

      override def continuous: Stream[IO, MonacoModel] =
        Stream.repeatEval(get)

      override def get: IO[MonacoModel] =
        IO.delay(editor.getModelFixed())

    }

  }

}

object MonacoModel {

  extension (model: MonacoModel) {

    def value: SignallingRef[IO, String] = new SignallingRef[IO, String] {

      override def access: IO[(String, String => IO[Boolean])] = IO.delay {
        val snapshot = model.getValue()
        val setter = (a: String) =>
          IO.delay {
            if (model.getValue() eq snapshot) {
              model.setValue(a)
              true
            } else false
          }
        (snapshot, setter)
      }

      override def tryUpdate(f: String => String): IO[Boolean] =
        update(f).as(true)

      override def tryModify[B](f: String => (String, B)): IO[Option[B]] =
        modify(f).map(Some(_))

      override def update(f: String => String): IO[Unit] =
        IO.delay(model.setValue(f(model.getValue())))

      override def modify[B](f: String => (String, B)): IO[B] =
        IO.delay {
          val (a, b) = f(model.getValue())
          model.setValue(a)
          b
        }

      override def tryModifyState[B](state: State[String, B]): IO[Option[B]] =
        tryModify(state.run(_).value)

      override def modifyState[B](state: State[String, B]): IO[B] =
        modify(state.run(_).value)

      override def set(a: String): IO[Unit] =
        IO.delay(model.setValue(a))

      override def getAndDiscreteUpdates(implicit F: Concurrent[IO]): Resource[IO, (String, Stream[IO, String])] = {
        Dispatcher.sequential[IO].flatMap { dispatcher =>
          Channel.unbounded[IO, String].toResource.flatMap { c =>
            val acquire: IO[IDisposable] = IO.delay {
              model.onDidChangeContent { ev =>
                dispatcher.unsafeRunAndForget(c.send(model.getValue()))
              }
            }
            val release = (i: IDisposable) => IO.delay(i.dispose())
            (this.get.toResource, Resource.make(acquire)(release).as(c.stream)).tupled
          }
        }
      }

      override def discrete: Stream[IO, String] =
        Stream.resource(getAndDiscreteUpdates).flatMap { case (h, t) => Stream.emit(h) ++ t }

      override def continuous: fs2.Stream[IO, String] =
        Stream.repeatEval(get)

      override def get: IO[String] =
        IO.delay(model.getValue())
    }

  }

}
