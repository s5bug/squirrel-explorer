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

  import tf.bug.cnut.SqInstructionType.*

  def renderInto(renderedCnut: MutableCnutRender, parent: SqFunctionProto): Unit =
    (sqInstructionType /* : @switch */) match {
      case Load =>
        renderInfo1(renderedCnut, parent.literals(arg1).show)
      case LoadFloat =>
        val bitsToFloat = java.lang.Float.intBitsToFloat(arg1)
        renderInfo1(renderedCnut, bitsToFloat.toString ++ "f")
      case DLoad =>
        renderInfo13(renderedCnut, parent.literals(arg1).show, parent.literals(arg3).show)
      case PrepCallK =>
        renderInfo1(renderedCnut, parent.literals(arg1).show)
      case GetK =>
        renderInfo1(renderedCnut, parent.literals(arg1).show)
      case Arith =>
        renderInfo3(renderedCnut, arg3.toChar.toString)
      case Bitw =>
        val op = (arg3: @switch) match {
          case 0 => "&"
          case 2 => "|"
          case 3 => "^"
          case 4 => "<<"
          case 5 => ">>"
          case 6 => ">>>"
          case _ => null
        }
        if(op != null) renderInfo3(renderedCnut, op)
        else renderRaw(renderedCnut)
      case CompArith | CompArithL =>
        renderInfo3(renderedCnut, arg3.toChar.toString)
      case Cmp =>
        val op = (arg3: @switch) match {
          case 0 => ">"
          case 2 => ">="
          case 3 => "<"
          case 4 => "<="
          case _ => null
        }
        if(op != null) renderInfo3(renderedCnut, op)
        else renderRaw(renderedCnut)
      case Closure =>
        renderInfo1(renderedCnut, parent.functions(arg1).name.show)
      case _ =>
        renderRaw(renderedCnut)
    }
  
  inline def renderRaw(inline renderedCnut: MutableCnutRender): Unit = {
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

  inline def renderInfo1(inline renderedCnut: MutableCnutRender, inline info: String): Unit = {
    renderedCnut.fragment(sqInstructionType.toString)
    renderedCnut.fragment("(")
    renderedCnut.fragment(arg0.toString)
    renderedCnut.fragment(", ")
    renderedCnut.fragmentInfo(arg1.toString, info)
    renderedCnut.fragment(", ")
    renderedCnut.fragment(arg2.toString)
    renderedCnut.fragment(", ")
    renderedCnut.fragment(arg3.toString)
    renderedCnut.fragment(")")
  }

  inline def renderInfo3(inline renderedCnut: MutableCnutRender, inline info: String): Unit = {
    renderedCnut.fragment(sqInstructionType.toString)
    renderedCnut.fragment("(")
    renderedCnut.fragment(arg0.toString)
    renderedCnut.fragment(", ")
    renderedCnut.fragment(arg1.toString)
    renderedCnut.fragment(", ")
    renderedCnut.fragment(arg2.toString)
    renderedCnut.fragment(", ")
    renderedCnut.fragmentInfo(arg3.toString, info)
    renderedCnut.fragment(")")
  }

  inline def renderInfo13(inline renderedCnut: MutableCnutRender, inline info1: String, inline info3: String): Unit = {
    renderedCnut.fragment(sqInstructionType.toString)
    renderedCnut.fragment("(")
    renderedCnut.fragment(arg0.toString)
    renderedCnut.fragment(", ")
    renderedCnut.fragmentInfo(arg1.toString, info1)
    renderedCnut.fragment(", ")
    renderedCnut.fragment(arg2.toString)
    renderedCnut.fragment(", ")
    renderedCnut.fragmentInfo(arg3.toString, info3)
    renderedCnut.fragment(")")
  }
}
