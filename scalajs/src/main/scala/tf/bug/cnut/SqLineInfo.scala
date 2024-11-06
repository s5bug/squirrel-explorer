package tf.bug.cnut

case class SqLineInfo(
  line: Int,
  op: Int
) {

  def renderInto(indent: Int, renderedCnut: MutableCnutRender): Unit = {
    renderedCnut.fragment("LineInfo(")
    renderedCnut.line()
    renderedCnut.renderField("line", 2 + indent, () => renderedCnut.fragment(line.toString))
    renderedCnut.renderFieldLast("op", 2 + indent, () => renderedCnut.fragment(op.toString))
    renderedCnut.line()
    renderedCnut.space(indent)
    renderedCnut.fragment(")")
  }

}
