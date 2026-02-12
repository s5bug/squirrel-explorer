package tf.bug.cnut

import narr.NArray

case class SqFunctionProto(
  sourceName: SqObject,
  name: SqObject,
  literals: NArray[SqObject],
  parameters: NArray[SqObject],
  outerValues: NArray[SqOuterValue],
  localVarInfos: NArray[SqLocalVarInfo],
  lineInfos: NArray[SqLineInfo],
  defaultParams: NArray[Int],
  instructions: NArray[SqInstruction],
  functions: NArray[SqFunctionProto],
  stackSize: Int,
  bgenerator: Int,
  varparams: Int
) {

  def renderInto(renderLineInfos: Boolean, indent: Int, renderedCnut: MutableCnutRender): Unit = {
    renderedCnut.fragment("FunctionProto(")
    renderedCnut.line()
    renderedCnut.renderField("sourceName", 2 + indent, () => renderedCnut.fragment(sourceName.show))
    renderedCnut.renderField("name", 2 + indent, () => renderedCnut.fragment(name.show))
    renderedCnut.renderArrayField[SqObject]("literals", 2 + indent, literals, l => renderedCnut.fragment(l.show))
    renderedCnut.renderArrayField[SqObject]("parameters", 2 + indent, parameters, p => renderedCnut.fragment(p.show))
    renderedCnut.renderArrayField[SqOuterValue]("outerValues", 2 + indent, outerValues, _.renderInto(4 + indent, renderedCnut))
    renderedCnut.renderArrayField[SqLocalVarInfo]("localVarInfos", 2 + indent, localVarInfos, _.renderInto(4 + indent, renderedCnut))
    if renderLineInfos then {
      renderedCnut.renderArrayField[SqLineInfo]("lineInfos", 2 + indent, lineInfos, _.renderInto(4 + indent, renderedCnut))
    }
    renderedCnut.renderArrayField[Int]("defaultParams", 2 + indent, defaultParams, i => renderedCnut.fragment(i.toString))
    renderedCnut.renderArrayField[SqInstruction]("instructions", 2 + indent, instructions, i => i.renderInto(renderedCnut, this))
    renderedCnut.renderArrayField[SqFunctionProto]("functions", 2 + indent, functions, f => f.renderInto(renderLineInfos, 4 + indent, renderedCnut))
    renderedCnut.renderField("stackSize", 2 + indent, () => renderedCnut.fragment(stackSize.toString))
    renderedCnut.renderField("bgenerator", 2 + indent, () => renderedCnut.fragment(bgenerator.toString))
    renderedCnut.renderFieldLast("varparams", 2 + indent, () => renderedCnut.fragment(varparams.toString))
    renderedCnut.line()
    renderedCnut.space(indent)
    renderedCnut.fragment(")")
  }

}
