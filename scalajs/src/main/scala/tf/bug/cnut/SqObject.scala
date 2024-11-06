package tf.bug.cnut

sealed abstract class SqObject {
  def show: String
}
final case class SqString(value: String) extends SqObject {
  inline def escape(inline s: String): String =
    typings.jsStringEscape.mod.^
      .asInstanceOf[scalajs.js.Dynamic].default
      .asInstanceOf[scalajs.js.Function1[scalajs.js.Any, String]]
      .apply(s)

  override def show: String = "\"" ++ escape(value) ++ "\""
}
final case class SqInteger(value: Int) extends SqObject {
  override def show: String = value.toString
}
final case class SqFloat(value: Float) extends SqObject {
  override def show: String = value.toString ++ "f"
}
case object SqNull extends SqObject {
  override def show: String = "null"
}
