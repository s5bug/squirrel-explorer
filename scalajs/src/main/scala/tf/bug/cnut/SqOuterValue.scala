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

  def doc: Doc = Doc.intercalate(Doc.char(',') + Doc.lineOrSpace, Vector(
    Doc.text("sqOuterType = ") + Doc.text(sqOuterType.toString),
    Doc.text("src = ") + src.doc,
    Doc.text("name = ") + name.doc
  )).tightBracketBy(Doc.text("SqOuterValue("), Doc.char(')'))

}
