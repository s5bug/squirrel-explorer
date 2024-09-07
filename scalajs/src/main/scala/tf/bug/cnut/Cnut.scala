package tf.bug.cnut

import scodec.*
import scodec.bits.ByteVector
import scodec.codecs.*

object Cnut {

  final val sqClosureStreamPart = constant(ByteVector(0x54, 0x52, 0x41, 0x50))

  final val sqObjectTypeNull = 0x01000001
  final val sqObjectTypeInteger = 0x05000002
  final val sqObjectTypeFloat = 0x05000004
  final val sqObjectTypeString = 0x08000010
  final val sqObject: Codec[SqObject] = discriminated[SqObject].by(uint32L)
    .singleton(sqObjectTypeNull, SqNull)
    .typecase(sqObjectTypeInteger, int32L.as[SqInteger])
    .typecase(sqObjectTypeFloat, floatL.as[SqFloat])
    .typecase(sqObjectTypeString, variableSizeBytes(int32L, utf8).as[SqString])

  final val sqOuterValue: Codec[SqOuterValue] = (
    uint32L.xmap(l => SqOuterType.fromOrdinal(l.toInt), _.ordinal) ::
    sqObject ::
    sqObject
  ).as[SqOuterValue]

  final val sqLocalVarInfo: Codec[SqLocalVarInfo] = (
    sqObject ::
    uint32L ::
    uint32L ::
    uint32L
  ).as[SqLocalVarInfo]

  final val sqLineInfo: Codec[SqLineInfo] = (
    int32L ::
    int32L
  ).as[SqLineInfo]

  final val sqInstruction: Codec[SqInstruction] = (
    int32L ::
    uint8L.xmap(SqInstructionType.fromOrdinal, _.ordinal) ::
    uint8L ::
    uint8L ::
    uint8L
  ).xmap(
    (arg1, it, arg0, arg2, arg3) => SqInstruction(it, arg0, arg1, arg2, arg3),
    { case SqInstruction(it, arg0, arg1, arg2, arg3) => (arg1, it, arg0, arg2, arg3) }
  )

  private final val sqFunctionProtoCounteds: Codec[
    (Vector[SqObject], Vector[SqObject], Vector[SqOuterValue], Vector[SqLocalVarInfo],
      Vector[SqLineInfo], Vector[Int], Vector[SqInstruction], Vector[SqFunctionProto])
  ] =
    (int32L :: int32L :: int32L :: int32L :: int32L :: int32L :: int32L :: int32L).flatZip {
      case (nLiterals, nParameters, nOuterValues, nLocalVarInfos, nLineInfos, nDefaultParams, nInstructions, nFunctions) =>
        val lits = sqClosureStreamPart ~> vectorOfN(provide(nLiterals), sqObject)
        val params = sqClosureStreamPart ~> vectorOfN(provide(nParameters), sqObject)
        val outerVals = sqClosureStreamPart ~> vectorOfN(provide(nOuterValues), sqOuterValue)
        val localVars = sqClosureStreamPart ~> vectorOfN(provide(nLocalVarInfos), sqLocalVarInfo)
        val lineInfos = sqClosureStreamPart ~> vectorOfN(provide(nLineInfos), sqLineInfo)
        val defaultParams = sqClosureStreamPart ~> vectorOfN(provide(nDefaultParams), int32L)
        val instructions = sqClosureStreamPart ~> vectorOfN(provide(nInstructions), sqInstruction)
        val functions = sqClosureStreamPart ~> vectorOfN(provide(nFunctions), sqFunctionProto)
        lits :: params :: outerVals :: localVars :: lineInfos :: defaultParams :: instructions :: functions
    }.xmap(
      { case (counts, vecs) => vecs },
      vecs => (vecs.map[[X] =>> Int]([t] => (v: t) => v.asInstanceOf[Vector[?]].size).asInstanceOf[(Int, Int, Int, Int, Int, Int, Int, Int)], vecs)
    )

  final val sqFunctionProto: Codec[SqFunctionProto] = (
    sqClosureStreamPart ~>
    sqObject ::
    sqObject ::
    sqClosureStreamPart ~>
    sqFunctionProtoCounteds ++
    (int16L ::
    uint16L ::
    uint16L)
  ).as[SqFunctionProto]

  final val sqClosure: Codec[SqClosure] = (
    constant(ByteVector(0x52, 0x49, 0x51, 0x53)) :: // "RIQS"
    uint32L ::
    sqFunctionProto ::
    constant(ByteVector(0x4C, 0x49, 0x41, 0x54)) // "LIAT"
  ).as[SqClosure]

  final val cnut: Codec[SqClosure] = constant(ByteVector(0xFA, 0xFA)) ~> sqClosure

}
