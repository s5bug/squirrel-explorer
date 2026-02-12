package tf.bug.fe

import cats.effect.IO
import narr.NArray
import tf.bug.NArrayFactory
import tf.bug.cnut.SqInstructionType
import typings.lezerCommon.mod as lezerCommon

private def safeOpt[A](a: A | Null | Unit): Option[A] = {
  a match {
    case null => None
    case () => None
    case other => Some(other.asInstanceOf[A])
  }
}

abstract class LezerRender {
  def ensure(to: Int): IO[Unit]
  def progressFull: IO[Unit]
  def apply(sn: lezerCommon.SyntaxNode): IO[String]
}

extension(node: LezerFunctionProto | LezerObject | LezerOuterValue | LezerLocalVarInfo | LezerInt | LezerInstruction | LezerInstructionType) {
  def from: Int = node.from.toInt
  def to: Int = node.to.toInt
  def text(using render: LezerRender): IO[String] = render.ensure(node.to.toInt) >> render(node)
}

opaque type LezerFunctionProto <: scalajs.js.Any = lezerCommon.SyntaxNode

object LezerFunctionProto {
  inline def of(sn: lezerCommon.SyntaxNode): LezerFunctionProto = sn

  extension (lfp: LezerFunctionProto) {
    def sourceName: LezerObject =
      lfp.getChild("FunctionSourceName")
        .getChild("Object")

    def name: LezerObject =
      lfp.getChild("FunctionName")
        .getChild("Object")

    def literals: NArray[LezerObject] =
      lfp.getChild("FunctionLiterals")
        .getChild("Array")
        .getChildren("Object").to(NArrayFactory.of[lezerCommon.SyntaxNode])
    
    def parameters: NArray[LezerObject] =
      lfp.getChild("FunctionParameters")
        .getChild("Array")
        .getChildren("Object").to(NArrayFactory.of[lezerCommon.SyntaxNode])
    
    def outerValues: NArray[LezerOuterValue] =
      lfp.getChild("FunctionOuterValues")
        .getChild("Array")
        .getChildren("OuterValue").to(NArrayFactory.of[lezerCommon.SyntaxNode])
        
    def localVarInfos: NArray[LezerLocalVarInfo] =
      lfp.getChild("FunctionLocalVarInfos")
        .getChild("Array")
        .getChildren("LocalVarInfo").to(NArrayFactory.of[lezerCommon.SyntaxNode])
    
    def defaultParams: NArray[LezerInt] =
      lfp.getChild("FunctionDefaultParams")
        .getChild("Array")
        .getChildren("DefaultParams").to(NArrayFactory.of[lezerCommon.SyntaxNode])
    
    def instructions: NArray[LezerInstruction] =
      lfp.getChild("FunctionInstructions")
        .getChild("Array")
        .getChildren("Instruction").to(NArrayFactory.of[lezerCommon.SyntaxNode])
        
    def functions: NArray[LezerFunctionProto] =
      lfp.getChild("FunctionFunctions")
        .getChild("Array")
        .getChildren("FunctionProto").view.filterNot(_ == null).to(NArrayFactory.of[lezerCommon.SyntaxNode])
  }
}

opaque type LezerObject <: scalajs.js.Any = lezerCommon.SyntaxNode

object LezerObject {

}

opaque type LezerOuterValue = lezerCommon.SyntaxNode

object LezerOuterValue {
  
}

opaque type LezerLocalVarInfo <: scalajs.js.Any = lezerCommon.SyntaxNode

object LezerLocalVarInfo {
  extension (lvi: LezerLocalVarInfo) {
    def name: LezerObject =
      lvi.getChild("Object")
      
    def pos: LezerInt =
      lvi.getChildren("Integer")(0)

    def startOp: LezerInt =
      lvi.getChildren("Integer")(1)

    def endOp: LezerInt =
      lvi.getChildren("Integer")(2)
  }
}

opaque type LezerInt = lezerCommon.SyntaxNode

object LezerInt {
  extension(li: LezerInt) {
    def value(using render: LezerRender): IO[Int] = li.text.map(_.toInt)
  }
}

opaque type LezerInstruction = lezerCommon.SyntaxNode

object LezerInstruction {
  extension(li: LezerInstruction) {
    def instructionType: LezerInstructionType =
      li.getChild("InstructionType")
      
    def a0: LezerInt =
      li.getChildren("Integer")(0)

    def a1: LezerInt =
      li.getChildren("Integer")(1)

    def a2: LezerInt =
      li.getChildren("Integer")(2)

    def a3: LezerInt =
      li.getChildren("Integer")(3)
  }
}

opaque type LezerInstructionType = lezerCommon.SyntaxNode

object LezerInstructionType {
  extension (lit: LezerInstructionType) {
    def value(using render: LezerRender): IO[SqInstructionType] = lit.text.map(SqInstructionType.valueOf)
  }
}
