package tf.bug.cnut

import org.typelevel.paiges.Doc

case class SqFunctionProto(
  sourceName: SqObject,
  name: SqObject,
  literals: Vector[SqObject],
  parameters: Vector[SqObject],
  outerValues: Vector[SqOuterValue],
  localVarInfos: Vector[SqLocalVarInfo],
  lineInfos: Vector[SqLineInfo],
  defaultParams: Vector[Int],
  instructions: Vector[SqInstruction],
  functions: Vector[SqFunctionProto],
  stackSize: Int,
  bgenerator: Int,
  varparams: Int
) {

  inline def renderVector[A](inline label: String, inline vector: Vector[A], inline docf: A => Doc): Doc = {
    val maximumIndex = vector.size - 1
    val maximumIndexLength = maximumIndex.toString.length

    val commentedAs: Vector[Doc] =
      vector.zipWithIndex.map { case (e, i) =>
        val iString = i.toString
        val spc = " " * (maximumIndexLength - iString.length)
        Doc.text("/* ") + Doc.text(spc) + Doc.text(iString) + Doc.text(" */ ") + docf(e)
      }

    Doc.text(label) + Doc.intercalate(Doc.char(',') + Doc.lineOrSpace, commentedAs)
      .tightBracketBy(Doc.text("Vector("), Doc.char(')'))
  }

  def doc: Doc =
    Doc.intercalate(Doc.char(',') + Doc.lineOrSpace, Vector(
      Doc.text("sourceName = ") + sourceName.doc,
      Doc.text("name = ") + name.doc,
      renderVector("literals = ", literals, _.doc),
      renderVector("parameters = ", parameters, _.doc),
      renderVector("outerValues = ", outerValues, _.doc),
      renderVector("localVarInfos = ", localVarInfos, _.doc),
      renderVector("lineInfos = ", lineInfos, _.doc),
      renderVector("defaultParams = ", defaultParams, i => Doc.text(i.toString)),
      renderVector("instructions = ", instructions, _.doc),
      renderVector("functions = ", functions, _.doc),
      Doc.text("stackSize = ") + Doc.text(stackSize.toString),
      Doc.text("bgenerator = ") + Doc.text(bgenerator.toString),
      Doc.text("varparams = ") + Doc.text(varparams.toString)
    )).tightBracketBy(Doc.text("SqFunctionProto("), Doc.char(')'))

}
