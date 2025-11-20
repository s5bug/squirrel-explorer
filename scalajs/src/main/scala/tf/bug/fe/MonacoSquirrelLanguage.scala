package tf.bug.fe

import cats.*
import cats.effect.*
import org.scalablytyped.runtime.StringDictionary
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import typings.monacoEditor.esmVsEditorEditorDotapiMod as monaco

object MonacoSquirrelLanguage {
  def registerId: IO[Unit] = IO.delay {
    monaco.languages.register(monaco.languages.ILanguageExtensionPoint(id = "squirrel"))
  }

  @js.native
  @JSImport("@/squirrel_monarch.ts", JSImport.Default)
  private val lang: monaco.languages.IMonarchLanguage = js.native

  def registerTokenizer: IO[Unit] = {
    IO.delay {
      monaco.languages.setMonarchTokensProvider("squirrel", lang)
    }
  }

  def registerConfig: IO[Unit] = {
    val conf = monaco.languages.LanguageConfiguration()
    conf.setAutoClosingPairs(js.Array(
      monaco.languages.IAutoClosingPairConditional(open = "\"", close = "\""),
      monaco.languages.IAutoClosingPairConditional(open = "\'", close = "\'"),
      monaco.languages.IAutoClosingPairConditional(open = "(", close = ")"),
      monaco.languages.IAutoClosingPairConditional(open = "{", close = "}"),
      monaco.languages.IAutoClosingPairConditional(open = "[", close = "]"),
    ))
    conf.setBrackets(js.Array(
      js.Tuple2("(", ")"),
      js.Tuple2("[", "]"),
      js.Tuple2("{", "}")
    ))
    conf.setComments(
      monaco.languages.CommentRule()
        .setLineComment("//")
        .setBlockComment(js.Tuple2("/*", "*/"))
    )

    IO.delay {
      monaco.languages.setLanguageConfiguration("squirrel", conf)
    }
  }

  def register: IO[Unit] = {
    registerId >> registerTokenizer >> registerConfig
  }

}
