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
)
