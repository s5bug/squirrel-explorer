package tf.bug.cnut

import org.typelevel.paiges.Doc

case class SqLineInfo(
  line: Int,
  op: Int
) {

  def doc: Doc = Doc.text("SqLineInfo(") + Doc.intercalate(Doc.char(',') + Doc.space, Vector(
    Doc.text("line = ") + Doc.text(line.toString),
    Doc.text("op = ") + Doc.text(op.toString)
  )) + Doc.char(')')

}
