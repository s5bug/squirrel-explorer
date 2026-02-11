package tf.bug.fe

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

opaque type LezerRender = lezerCommon.SyntaxNode => String

object LezerRender {
  inline def of(fn: lezerCommon.SyntaxNode => String): LezerRender = fn
}

extension(node: LezerFunctionProto | LezerObject | LezerOuterValue | LezerLocalVarInfo | LezerInt | LezerInstruction | LezerInstructionType) {
  def from: Int = node.from.toInt
  def to: Int = node.to.toInt
  def text(using render: LezerRender): String = render(node)
}

opaque type LezerFunctionProto <: scalajs.js.Any = lezerCommon.SyntaxNode

object LezerFunctionProto {
  inline def of(sn: lezerCommon.SyntaxNode): LezerFunctionProto = sn

  extension (lfp: LezerFunctionProto) {
    def sourceName: Option[LezerObject] =
      safeOpt(lfp.getChild("FunctionSourceName"))
        .flatMap(fsn => safeOpt(fsn.getChild("Object")))

    def name: Option[LezerObject] =
      safeOpt(lfp.getChild("FunctionName"))
        .flatMap(fn => safeOpt(fn.getChild("Object")))

    def literals: Option[NArray[LezerObject]] =
      safeOpt(lfp.getChild("FunctionLiterals"))
        .flatMap(fl => safeOpt(fl.getChild("Array")))
        .map(a => a.getChildren("Object").view.filterNot(_ == null).to(NArrayFactory.of[lezerCommon.SyntaxNode]))
    
    def parameters: Option[NArray[LezerObject]] =
      safeOpt(lfp.getChild("FunctionParameters"))
        .flatMap(fl => safeOpt(fl.getChild("Array")))
        .map(a => a.getChildren("Object").view.filterNot(_ == null).to(NArrayFactory.of[lezerCommon.SyntaxNode]))
    
    def outerValues: Option[NArray[LezerOuterValue]] =
      safeOpt(lfp.getChild("FunctionOuterValues"))
        .flatMap(fl => safeOpt(fl.getChild("Array")))
        .map(a => a.getChildren("OuterValue").view.filterNot(_ == null).to(NArrayFactory.of[lezerCommon.SyntaxNode]))
        
    def localVarInfos: Option[NArray[LezerLocalVarInfo]] =
      safeOpt(lfp.getChild("FunctionLocalVarInfos"))
        .flatMap(fl => safeOpt(fl.getChild("Array")))
        .map(a => a.getChildren("LocalVarInfo").view.filterNot(_ == null).to(NArrayFactory.of[lezerCommon.SyntaxNode]))
    
    def defaultParams: Option[NArray[LezerInt]] =
      safeOpt(lfp.getChild("FunctionDefaultParams"))
        .flatMap(fl => safeOpt(fl.getChild("Array")))
        .map(a => a.getChildren("DefaultParams").view.filterNot(_ == null).to(NArrayFactory.of[lezerCommon.SyntaxNode]))
    
    def instructions: Option[NArray[LezerInstruction]] =
      safeOpt(lfp.getChild("FunctionInstructions"))
        .flatMap(fl => safeOpt(fl.getChild("Array")))
        .map(a => a.getChildren("Instruction").view.filterNot(_ == null).to(NArrayFactory.of[lezerCommon.SyntaxNode]))
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
    def name: Option[LezerObject] =
      safeOpt(lvi.getChild("Object"))
      
    def pos: Option[LezerInt] =
      safeOpt(lvi.getChildren("Integer")(0))

    def startOp: Option[LezerInt] =
      safeOpt(lvi.getChildren("Integer")(1))

    def endOp: Option[LezerInt] =
      safeOpt(lvi.getChildren("Integer")(2))
  }
}

opaque type LezerInt = lezerCommon.SyntaxNode

object LezerInt {
  extension(li: LezerInt) {
    def value(using render: LezerRender): Int = li.text.toInt
  }
}

opaque type LezerInstruction = lezerCommon.SyntaxNode

object LezerInstruction {
  extension(li: LezerInstruction) {
    def instructionType: Option[LezerInstructionType] =
      safeOpt(li.getChild("InstructionType"))
      
    def a0: Option[LezerInt] =
      safeOpt(li.getChildren("Integer")(0))

    def a1: Option[LezerInt] =
      safeOpt(li.getChildren("Integer")(1))

    def a2: Option[LezerInt] =
      safeOpt(li.getChildren("Integer")(2))

    def a3: Option[LezerInt] =
      safeOpt(li.getChildren("Integer")(3))
  }
}

opaque type LezerInstructionType = lezerCommon.SyntaxNode

object LezerInstructionType {
  extension (lit: LezerInstructionType) {
    def value(using render: LezerRender): SqInstructionType = SqInstructionType.valueOf(lit.text)
  }
}
