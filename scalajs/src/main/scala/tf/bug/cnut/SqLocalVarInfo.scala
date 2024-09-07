package tf.bug.cnut

import org.typelevel.paiges.Doc

case class SqLocalVarInfo(
  name: SqObject,
  pos: Long,
  startOp: Long,
  endOp: Long
) {

  def doc: Doc = Doc.text("SqLocalVarInfo(") + Doc.intercalate(Doc.char(',') + Doc.space, Vector(
    Doc.text("name = ") + name.doc,
    Doc.text("pos = ") + Doc.text(pos.toString),
    Doc.text("startOp = ") + Doc.text(startOp.toString),
    Doc.text("endOp = ") + Doc.text(endOp.toString)
  )) + Doc.char(')')

}
