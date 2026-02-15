package tf.bug.fe

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import fs2.{Chunk, Collector, Stream}
import narr.*
import org.scalablytyped.runtime.StringDictionary
import scala.annotation.switch
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import tf.bug.NArrayFactory
import tf.bug.cnut.{Diagnostic, RenderedCnut, SqInstructionType}
import tf.bug.worker.DragonboxApi
import typings.lezerCommon.mod as lezerCommon
import typings.lezerLr.mod as lezerLr
import typings.codemirrorLanguage.mod as codemirrorLanguage
import typings.codemirrorLint.{mod, mod as codemirrorLint}
import typings.codemirrorState.mod.Extension
import typings.lezerCommon.mod.SyntaxNode

object LezerCnutLanguage {
  @js.native
  @JSImport("@/cnut_lezer.grammar", "parser")
  object parser extends lezerLr.LRParser

  val configuredParser: lezerLr.LRParser =
    parser.configure(lezerLr.ParserConfig())

  val language: codemirrorLanguage.LRLanguage = {
    val data = typings.codemirrorLanguage.anon.LanguageData(configuredParser)
    data.setName("cnut")
    codemirrorLanguage.LRLanguage.define(data)
  }

  val cnut: codemirrorLanguage.LanguageSupport =
    new codemirrorLanguage.LanguageSupport(language, js.Array(
      language.data.of(StringDictionary()),
    ))

  def makeInfoDiagnostic(from: Double, to: Double, text: String): codemirrorLint.Diagnostic = {
    val base = codemirrorLint.Diagnostic(
      from = from,
      to = to,
      severity = codemirrorLint.Severity.hint,
      message = text,
    )

    base.setRenderMessage(_ => {
      val c = org.scalajs.dom.document.createElement("code")
      c.innerText = text
      c
    })

    base.setActionsVarargs(codemirrorLint.Action((_, _, _) => {
      org.scalajs.dom.window.navigator.clipboard.writeText(text)
    }, "Copy"))
  }

  def cnutLinter(dispatch: Dispatcher[IO]): Extension = Codemirror.linter(dispatch, typings.codemirrorLint.mod.LintConfig().setDelay(0.0)) { view =>
    val iarray = view.state.field(CnutEditorState.cnutField).fold(IArray.empty[Diagnostic])(_.diagnostics)

    val jsArray: js.Array[codemirrorLint.Diagnostic] = js.Array()

    iarray.foreach {
      case Diagnostic(from, to, message) =>
        jsArray.push(makeInfoDiagnostic(from, to, message))
    }

    IO.pure(jsArray)
  }
}

object DiagnosticArrayCollector extends Collector[codemirrorLint.Diagnostic] {
  final type Out = js.Array[codemirrorLint.Diagnostic]

  override def newBuilder: Collector.Builder[mod.Diagnostic, js.Array[mod.Diagnostic]] = {
    val array: js.Array[mod.Diagnostic] = js.Array()
    new Collector.Builder[mod.Diagnostic, js.Array[mod.Diagnostic]] {
      override def +=(c: Chunk[mod.Diagnostic]): Unit = c.foreach(array.push(_))
      override def result: js.Array[mod.Diagnostic] = array
    }
  }
}
