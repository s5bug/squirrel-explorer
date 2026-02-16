package tf.bug.fe

import cats.*
import cats.effect.*
import org.scalablytyped.runtime.StringDictionary
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import typings.lezerLr.mod as lezerLr
import typings.codemirrorLanguage.mod as codemirrorLanguage
import typings.codemirrorLanguage.anon.{Align, Except}

object LezerSquirrelLanguage {
  @js.native
  @JSImport("@/squirrel_lezer.grammar", "parser")
  object parser extends lezerLr.LRParser

  val configuredParser: lezerLr.LRParser = {
    parser.configure(
      lezerLr.ParserConfig()
        .setPropsVarargs(
          // styleTags,
          codemirrorLanguage.indentNodeProp.add(StringDictionary[js.Function1[codemirrorLanguage.TreeIndentContext, Double]](
            "ifStatement" -> codemirrorLanguage.continuedIndent(Except().setExcept(js.RegExp("""^\s*({|else\b)"""))),
            "tryStatement" -> codemirrorLanguage.continuedIndent(Except().setExcept(js.RegExp("""^\s*({|catch\b)"""))),
            "blockStatement" -> codemirrorLanguage.delimitedIndent(Align("}")),
            "Statement" -> codemirrorLanguage.continuedIndent(Except().setExcept(js.RegExp("""^\s*{"""))),
            "tableExpression" -> codemirrorLanguage.delimitedIndent(Align("}")),
          )),
        )
    )
  }

  val language: codemirrorLanguage.LRLanguage = {
    val data = typings.codemirrorLanguage.anon.LanguageData(configuredParser)
    data.setName("squirrel")
    data.setLanguageData(StringDictionary(
      "closeBrackets" -> StringDictionary("brackets" -> js.Array("(", "[", "{")),
      "commentTokens" -> StringDictionary("line" -> "//", "block" -> StringDictionary("open" -> "/*", "close" -> "*/")),
      "indentOnInput" -> js.RegExp("""^\s*(?:case |default:|\{|\}|<\/)$""")
    ))
    codemirrorLanguage.LRLanguage.define(data)
  }

  val squirrel: codemirrorLanguage.LanguageSupport =
    new codemirrorLanguage.LanguageSupport(language, js.Array(
      language.data.of(StringDictionary()),
      codemirrorLanguage.indentUnit.of("    "),
    ))
}
