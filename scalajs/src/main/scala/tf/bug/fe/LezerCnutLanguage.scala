package tf.bug.fe

import org.scalablytyped.runtime.StringDictionary
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import typings.lezerLr.mod as lezerLr
import typings.codemirrorLanguage.mod as codemirrorLanguage

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
}
