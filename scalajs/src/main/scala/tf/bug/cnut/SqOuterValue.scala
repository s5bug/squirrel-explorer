package tf.bug.cnut

enum SqOuterType extends java.lang.Enum[SqOuterType] {
  case Local
  case Symbol
  case Outer
}

case class SqOuterValue(
  sqOuterType: SqOuterType,
  src: SqObject,
  name: SqObject
) {

  def renderInto(indent: Int, renderedCnut: MutableCnutRender): Unit = {
    renderedCnut.fragment("OuterValue(")
    renderedCnut.line()
    renderedCnut.renderField("sqOuterType", 2 + indent, () => renderedCnut.fragment(sqOuterType.toString))
    renderedCnut.renderField("src", 2 + indent, () => renderedCnut.fragment(src.show))
    renderedCnut.renderFieldLast("name", 2 + indent, () => renderedCnut.fragment(name.show))
    renderedCnut.line()
    renderedCnut.space(indent)
    renderedCnut.fragment(")")
  }

}
