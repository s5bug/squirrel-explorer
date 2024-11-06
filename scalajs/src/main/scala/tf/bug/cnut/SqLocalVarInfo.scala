package tf.bug.cnut

case class SqLocalVarInfo(
  name: SqObject,
  pos: Long,
  startOp: Long,
  endOp: Long
) {

  def renderInto(indent: Int, renderedCnut: MutableCnutRender): Unit = {
    renderedCnut.fragment("LocalVarInfo(")
    renderedCnut.line()
    renderedCnut.space(2 + indent)
    renderedCnut.renderFieldInline("name", 2 + indent, () => renderedCnut.fragment(name.show))
    renderedCnut.renderFieldInline("pos", 2 + indent, () => renderedCnut.fragment(pos.toString))
    renderedCnut.renderFieldInline("startOp", 2 + indent, () => renderedCnut.fragment(startOp.toString))
    renderedCnut.renderFieldInlineLast("endOp", 2 + indent, () => renderedCnut.fragment(endOp.toString))
    renderedCnut.line()
    renderedCnut.space(indent)
    renderedCnut.fragment(")")
  }

}
