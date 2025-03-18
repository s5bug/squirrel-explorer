package tf.bug.fe

import cats.*
import cats.effect.*
import typings.monacoEditor.mod as monaco
import monaco.languages.{IExpandedMonarchLanguageAction, IExpandedMonarchLanguageRule, ILanguageExtensionPoint, IMonarchLanguage}
import org.scalablytyped.runtime.StringDictionary
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

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

  def register: IO[Unit] = {
    registerId >> registerTokenizer
  }

}
