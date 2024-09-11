package tf.bug.cnut

import java.nio.charset.{Charset, StandardCharsets}
import scodec.*
import scodec.bits.ByteVector
import scodec.codecs.*

object Cnut {

  final val sqClosureStreamPart = constant(ByteVector(0x54, 0x52, 0x41, 0x50))

  final val sqObjectTypeNull = 0x01000001
  final val sqObjectTypeInteger = 0x05000002
  final val sqObjectTypeFloat = 0x05000004
  final val sqObjectTypeString = 0x08000010
  def sqObject(stringCodec: Codec[String]): Codec[SqObject] = discriminated[SqObject].by(uint32L)
    .singleton(sqObjectTypeNull, SqNull)
    .typecase(sqObjectTypeInteger, int32L.as[SqInteger])
    .typecase(sqObjectTypeFloat, floatL.as[SqFloat])
    .typecase(sqObjectTypeString, variableSizeBytes(int32L, stringCodec).as[SqString])

  def sqOuterValue(encObj: Codec[SqObject]): Codec[SqOuterValue] = (
    uint32L.xmap(l => SqOuterType.fromOrdinal(l.toInt), _.ordinal) ::
    encObj ::
    encObj
  ).as[SqOuterValue]

  final def sqLocalVarInfo(encObj: Codec[SqObject]): Codec[SqLocalVarInfo] = (
    encObj ::
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

  private def sqFunctionProtoCounteds(encObj: Codec[SqObject], functionProtoCodec: Codec[SqFunctionProto]): Codec[
    (Vector[SqObject], Vector[SqObject], Vector[SqOuterValue], Vector[SqLocalVarInfo],
      Vector[SqLineInfo], Vector[Int], Vector[SqInstruction], Vector[SqFunctionProto])
  ] =
    (int32L :: int32L :: int32L :: int32L :: int32L :: int32L :: int32L :: int32L).consume {
      case (nLiterals, nParameters, nOuterValues, nLocalVarInfos, nLineInfos, nDefaultParams, nInstructions, nFunctions) =>
        val lits = sqClosureStreamPart ~> vectorOfN(provide(nLiterals), encObj)
        val params = sqClosureStreamPart ~> vectorOfN(provide(nParameters), encObj)
        val outerVals = sqClosureStreamPart ~> vectorOfN(provide(nOuterValues), sqOuterValue(encObj))
        val localVars = sqClosureStreamPart ~> vectorOfN(provide(nLocalVarInfos), sqLocalVarInfo(encObj))
        val lineInfos = sqClosureStreamPart ~> vectorOfN(provide(nLineInfos), sqLineInfo)
        val defaultParams = sqClosureStreamPart ~> vectorOfN(provide(nDefaultParams), int32L)
        val instructions = sqClosureStreamPart ~> vectorOfN(provide(nInstructions), sqInstruction)
        val functions = sqClosureStreamPart ~> vectorOfN(provide(nFunctions), functionProtoCodec)
        lits :: params :: outerVals :: localVars :: lineInfos :: defaultParams :: instructions :: functions
    } { vecs =>
      vecs.map[[X] =>> Int]([t] => (v: t) => v.asInstanceOf[Vector[?]].size).asInstanceOf[(Int, Int, Int, Int, Int, Int, Int, Int)]
    }

  def sqFunctionProto(stringCodec: Codec[String]): Codec[SqFunctionProto] = {
    val encObj = sqObject(stringCodec)
    lazy val self: Codec[SqFunctionProto] = (
      sqClosureStreamPart ~>
      encObj ::
      encObj ::
      sqClosureStreamPart ~>
      sqFunctionProtoCounteds(encObj, lazily(self)) ++
      (int16L ::
      uint16L ::
      uint16L)
    ).as[SqFunctionProto]
    self
  }

  def sqClosure(stringCodec: Codec[String]): Codec[SqClosure] = (
    constant(ByteVector(0x52, 0x49, 0x51, 0x53)) :: // "RIQS"
    uint32L ::
    sqFunctionProto(stringCodec) ::
    constant(ByteVector(0x4C, 0x49, 0x41, 0x54)) // "LIAT"
  ).as[SqClosure]

  def cnut(stringCodec: Codec[String]): Codec[SqClosure] = constant(ByteVector(0xFA, 0xFA)) ~> sqClosure(stringCodec)

  final val cnutUtf8: Codec[SqClosure] = cnut(string(StandardCharsets.UTF_8))
  final val cnutSjis: Codec[SqClosure] = cnut(ShiftJisCodec)

}
