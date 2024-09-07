package tf.bug.cnut

import org.typelevel.paiges.Doc

case class SqLineInfo(
  line: Int,
  op: Int
) {

  def doc: Doc = Doc.intercalate(Doc.char(',') + Doc.lineOrSpace, Vector(
    Doc.text("line = ") + Doc.text(line.toString),
    Doc.text("op = ") + Doc.text(op.toString)
  )).tightBracketBy(Doc.text("SqLineInfo("), Doc.char(')'))

}
