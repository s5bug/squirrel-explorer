package tf.bug.cnut

import org.typelevel.paiges.Doc

case class SqClosure(
  sizeOfSqChar: Long,
  funcProto: SqFunctionProto
) {

  def doc: Doc =
    Doc.intercalate(Doc.char(',') + Doc.lineOrSpace, Vector(
      Doc.text("sizeOfSqChar = ") + Doc.text(sizeOfSqChar.toString),
      Doc.text("funcProto = ") + funcProto.doc
    )).tightBracketBy(Doc.text("SqClosure("), Doc.char(')'))

}
