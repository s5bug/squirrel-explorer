package tf.bug.cnut

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

  def renderInto(renderLineInfos: Boolean, indent: Int, renderedCnut: MutableCnutRender): Unit = {
    renderedCnut.fragment("FunctionProto(")
    renderedCnut.line()
    renderedCnut.renderField("sourceName", 2 + indent, () => renderedCnut.fragment(sourceName.show))
    renderedCnut.renderField("name", 2 + indent, () => renderedCnut.fragment(name.show))
    renderedCnut.renderVectorField("literals", 2 + indent, literals, l => renderedCnut.fragment(l.show))
    renderedCnut.renderVectorField("parameters", 2 + indent, parameters, p => renderedCnut.fragment(p.show))
    renderedCnut.renderVectorField("outerValues", 2 + indent, outerValues, _.renderInto(4 + indent, renderedCnut))
    renderedCnut.renderVectorField("localVarInfos", 2 + indent, localVarInfos, _.renderInto(4 + indent, renderedCnut))
    if renderLineInfos then {
      renderedCnut.renderVectorField("lineInfos", 2 + indent, lineInfos, _.renderInto(4 + indent, renderedCnut))
    }
    renderedCnut.renderVectorField("defaultParams", 2 + indent, defaultParams, i => renderedCnut.fragment(i.toString))
    renderedCnut.renderVectorField("instructions", 2 + indent, instructions, i => i.renderInto(renderedCnut, this))
    renderedCnut.renderVectorField("functions", 2 + indent, functions, f => f.renderInto(renderLineInfos, 4 + indent, renderedCnut))
    renderedCnut.renderField("stackSize", 2 + indent, () => renderedCnut.fragment(stackSize.toString))
    renderedCnut.renderField("bgenerator", 2 + indent, () => renderedCnut.fragment(bgenerator.toString))
    renderedCnut.renderFieldLast("varparams", 2 + indent, () => renderedCnut.fragment(varparams.toString))
    renderedCnut.line()
    renderedCnut.space(indent)
    renderedCnut.fragment(")")
  }

}
