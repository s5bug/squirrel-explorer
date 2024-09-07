package tf.bug.cnut

import org.typelevel.paiges.Doc

enum SqOuterType extends java.lang.Enum[SqOuterType] {
  case Local
  case Symbol
  case Outer
}

case class SqOuterValue(
  sqOuterType: SqOuterType,
  src: SqObject,
  name: SqObject
) {

  def doc: Doc = Doc.text("SqOuterValue(") + Doc.intercalate(Doc.char(',') + Doc.space, Vector(
    Doc.text("sqOuterType = ") + Doc.text(sqOuterType.toString),
    Doc.text("src = ") + src.doc,
    Doc.text("name = ") + name.doc
  )) + Doc.char(')')

}
