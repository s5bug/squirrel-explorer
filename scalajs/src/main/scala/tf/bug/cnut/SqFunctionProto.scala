package tf.bug.cnut

import narr.NArray

case class SqFunctionProto(
  sourceName: SqObject,
  name: SqObject,
  literals: NArray[SqObject],
  parameters: NArray[SqObject],
  outerValues: NArray[SqOuterValue],
  localVarInfos: NArray[SqLocalVarInfo],
  lineInfos: NArray[SqLineInfo],
  defaultParams: NArray[Int],
  instructions: NArray[SqInstruction],
  functions: NArray[SqFunctionProto],
  stackSize: Int,
  bgenerator: Int,
  varparams: Int
) {
  
  def localVarAt(position: Int, op: Int): Option[SqLocalVarInfo] = {
    localVarInfos.find { lv =>
      lv.pos == position && (lv.startOp - 1) <= op && op <= lv.endOp
    }
  }
  
}
