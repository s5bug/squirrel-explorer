package tf.bug.cnut

case class SqClosure(
  sizeOfSqChar: Long,
  funcProto: SqFunctionProto
) {

  def renderInto(lineInfos: Boolean, indent: Int, renderedCnut: MutableCnutRender): Unit = {
    renderedCnut.fragment("Closure(")
    renderedCnut.line()
    renderedCnut.space(2 + indent)
    renderedCnut.fragment("sizeOfSqChar = ")
    renderedCnut.fragment(sizeOfSqChar.toString)
    renderedCnut.fragment(",")
    renderedCnut.line()
    renderedCnut.space(2 + indent)
    renderedCnut.fragment("funcProto = ")
    funcProto.renderInto(lineInfos, 2 + indent, renderedCnut)
    renderedCnut.line()
    renderedCnut.space(indent)
    renderedCnut.fragment(")")
  }

}
