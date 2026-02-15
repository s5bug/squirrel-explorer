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
  
  def diagnostic(builder: Diagnostic.Builder, rendered: RenderedCnut.Instruction, context: SqFunctionProto, myIdx: Int): Unit = this.sqInstructionType match {
    case SqInstructionType.Load =>
      context.localVarAt(arg0, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg0, lv.name.show))
      builder.addDiagnostic(rendered.arg1, context.literals(arg1).show)
    case SqInstructionType.LoadInt =>
      context.localVarAt(arg0, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg0, lv.name.show))
    case SqInstructionType.LoadFloat =>
      context.localVarAt(arg0, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg0, lv.name.show))
      builder.addFloatDiagnostic(rendered.arg1, arg1)
    case SqInstructionType.DLoad =>
      context.localVarAt(arg0, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg0, lv.name.show))
      builder.addDiagnostic(rendered.arg1, context.literals(arg1).show)
      context.localVarAt(arg2, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg2, lv.name.show))
      builder.addDiagnostic(rendered.arg3, context.literals(arg3).show)
    case SqInstructionType.PrepCallK =>
      builder.addDiagnostic(rendered.arg1, context.literals(arg1).show)
    case SqInstructionType.GetK =>
      context.localVarAt(arg0, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg0, lv.name.show))
      builder.addDiagnostic(rendered.arg1, context.literals(arg1).show)
    case SqInstructionType.Arith =>
      context.localVarAt(arg0, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg0, lv.name.show))
      context.localVarAt(arg1, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg1, lv.name.show))
      context.localVarAt(arg2, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg2, lv.name.show))
      builder.addDiagnostic(rendered.arg3, arg3.toChar.toString)
    case SqInstructionType.Bitw =>
      context.localVarAt(arg0, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg0, lv.name.show))
      context.localVarAt(arg1, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg1, lv.name.show))
      context.localVarAt(arg2, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg2, lv.name.show))

      val op = (arg3: @switch) match {
        case 0 => "&"
        case 2 => "|"
        case 3 => "^"
        case 4 => "<<"
        case 5 => ">>"
        case 6 => ">>>"
        case 7 => null
      }
      
      builder.addDiagnostic(rendered.arg3, op)
    case SqInstructionType.CompArith =>
      context.localVarAt(arg1, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg1, lv.name.show))
      builder.addDiagnostic(rendered.arg3, arg3.toChar.toString)
    case SqInstructionType.CompArithL =>
      context.localVarAt(arg0, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg0, lv.name.show))
      context.localVarAt(arg1, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg1, lv.name.show))
      context.localVarAt(arg2, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg2, lv.name.show))
      builder.addDiagnostic(rendered.arg3, arg3.toChar.toString)
    case SqInstructionType.Cmp =>
      context.localVarAt(arg0, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg0, lv.name.show))
      context.localVarAt(arg1, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg1, lv.name.show))
      context.localVarAt(arg2, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg2, lv.name.show))

      val op = (arg3: @switch) match {
        case 0 => ">"
        case 2 => ">="
        case 3 => "<"
        case 4 => "<="
        case _ => null
      }

      builder.addDiagnostic(rendered.arg3, op)
    case SqInstructionType.Closure =>
      context.localVarAt(arg0, myIdx).foreach(lv => builder.addDiagnostic(rendered.arg0, lv.name.show))
      builder.addDiagnostic(rendered.arg1, context.functions(arg1).name.show)
    case _ => ()
  }
  
}
