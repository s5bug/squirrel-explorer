package tf.bug.fe

import cats.*
import cats.effect.*
import typings.monacoEditor.mod as monaco
import monaco.languages.{CommentRule, IAutoClosingPairConditional, IExpandedMonarchLanguageAction, IExpandedMonarchLanguageRule, ILanguageExtensionPoint, IMonarchLanguage, LanguageConfiguration}
import org.scalablytyped.runtime.StringDictionary
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import typings.monacoEditor.mod.languages

object MonacoSquirrelLanguage {
  def registerId: IO[Unit] = IO.delay {
    monaco.languages.register(ILanguageExtensionPoint(id = "squirrel"))
  }

  @js.native
  @JSImport("@/squirrel_monarch.ts", JSImport.Default)
  private val lang: IMonarchLanguage = js.native

  def registerTokenizer: IO[Unit] = {
    IO.delay {
      monaco.languages.setMonarchTokensProvider("squirrel", lang)
    }
  }

  def registerConfig: IO[Unit] = {
    val conf = LanguageConfiguration()
    conf.setAutoClosingPairs(js.Array(
      IAutoClosingPairConditional(open = "\"", close = "\""),
      IAutoClosingPairConditional(open = "\'", close = "\'"),
      IAutoClosingPairConditional(open = "(", close = ")"),
      IAutoClosingPairConditional(open = "{", close = "}"),
      IAutoClosingPairConditional(open = "[", close = "]"),
    ))
    conf.setBrackets(js.Array(
      js.Tuple2("(", ")"),
      js.Tuple2("[", "]"),
      js.Tuple2("{", "}")
    ))
    conf.setComments(
      CommentRule()
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
