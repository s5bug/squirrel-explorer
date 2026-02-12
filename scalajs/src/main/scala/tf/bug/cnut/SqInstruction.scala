package tf.bug.cnut

import scala.annotation.switch

enum SqInstructionType extends java.lang.Enum[SqInstructionType] {
  case Line
  case Load
  case LoadInt
  case LoadFloat
  case DLoad
  case TailCall
  case Call
  case PrepCall
  case PrepCallK
  case GetK
  case Move
  case NewSlot
  case Delete
  case Set
  case Get
  case Eq
  case Ne
  case Arith
  case Bitw
  case Return
  case LoadNulls
  case LoadRootTable
  case LoadBool
  case DMove
  case Jmp
  case Jnz
  case Jz
  case LoadFreeVar
  case VArgC
  case GetVArgV
  case NewTable
  case NewArray
  case AppendArray
  case GetParent
  case CompArith
  case CompArithL
  case Inc
  case IncL
  case PInc
  case PIncL
  case Cmp
  case Exists
  case InstanceOf
  case And
  case Or
  case Neg
  case Not
  case BwNot
  case Closure
  case Yield
  case Resume
  case Foreach
  case PostForeach
  case Delegate
  case Clone
  case TypeOf
  case PushTrap
  case PopTrap
  case Throw
  case Class
  case NewSlotA
}

final case class SqInstruction(
  sqInstructionType: SqInstructionType,
  arg0: Int,
  arg1: Int,
  arg2: Int,
  arg3: Int
) {
  def renderInto(renderedCnut: MutableCnutRender, parent: SqFunctionProto): Unit = {
    renderedCnut.fragment(sqInstructionType.toString)
    renderedCnut.fragment("(")
    renderedCnut.fragment(arg0.toString)
    renderedCnut.fragment(", ")
    renderedCnut.fragment(arg1.toString)
    renderedCnut.fragment(", ")
    renderedCnut.fragment(arg2.toString)
    renderedCnut.fragment(", ")
    renderedCnut.fragment(arg3.toString)
    renderedCnut.fragment(")")
  }
}
