package tf.bug.cnut

import org.typelevel.paiges.Doc

case class SqLocalVarInfo(
  name: SqObject,
  pos: Long,
  startOp: Long,
  endOp: Long
) {

  def doc: Doc = Doc.intercalate(Doc.char(',') + Doc.lineOrSpace, Vector(
    Doc.text("name = ") + name.doc,
    Doc.text("pos = ") + Doc.text(pos.toString),
    Doc.text("startOp = ") + Doc.text(startOp.toString),
    Doc.text("endOp = ") + Doc.text(endOp.toString)
  )).tightBracketBy(Doc.text("SqLocalVarInfo("), Doc.char(')'))

}
