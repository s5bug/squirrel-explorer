package tf.bug.cnut

import org.typelevel.paiges.Doc

sealed abstract class SqObject {
  def doc: Doc
}
final case class SqString(value: String) extends SqObject {
  inline def escape(inline s: String): String =
    typings.jsStringEscape.mod.^
      .asInstanceOf[scalajs.js.Dynamic].default
      .asInstanceOf[scalajs.js.Function1[scalajs.js.Any, String]]
      .apply(s)

  override def doc: Doc = Doc.char('"') + Doc.text(escape(value)) + Doc.char('"')
}
final case class SqInteger(value: Int) extends SqObject {
  override def doc: Doc = Doc.text(value.toString)
}
final case class SqFloat(value: Float) extends SqObject {
  override def doc: Doc = Doc.text(value.toString) + Doc.char('f')
}
case object SqNull extends SqObject {
  override def doc: Doc = Doc.text("null")
}
