package tf.bug.cnut

import java.nio.charset.{Charset, StandardCharsets}
import narr.*
import narr.native.NativeArrayBuilder
import scodec.*
import scodec.bits.{BitVector, ByteVector}
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
  
  final class NArrayCodec[A](private val count: Codec[Int], private val elem: Codec[A], private val initialBuilder: (count: Int) => NArrayBuilder[A]) extends Codec[NArray[A]] {
    override def decode(bits: BitVector): Attempt[DecodeResult[NArray[A]]] = count.decode(bits).match {
      case Attempt.Successful(DecodeResult(value, remainder)) =>
        val builder = initialBuilder(value)
        
        var left = value
        var rem = remainder
        while left > 0 do {
          elem.decode(rem) match {
            case Attempt.Successful(DecodeResult(nv, nr)) =>
              builder.addOne(nv)
              rem = nr
            case other => return other.asInstanceOf
          }
          
          left = left - 1
        }
        
        Attempt.Successful(DecodeResult(builder.result, rem))
      case other => other.asInstanceOf
    }

    override def encode(value: NArray[A]): Attempt[BitVector] =
      count.encode(value.length) match {
        case Attempt.Successful(bv) =>
          val bvs: Array[BitVector] = new Array(1 + value.length)
          bvs(0) = bv
          
          var i = 0
          while i < value.length do {
            elem.encode(value(i)) match {
              case Attempt.Successful(ebv) =>
                bvs(1 + i) = ebv
              case other => return other
            }
            i = i + 1
          }
          
          Attempt.Successful(BitVector.concat(bvs))
        case other => other
      }

    override def sizeBound: SizeBound = SizeBound.unknown
  }
  
  private inline def narrOfN[A](count: Codec[Int], element: Codec[A]): Codec[NArray[A]] =
    inline compiletime.erasedValue[A] match {
      case _: Byte => (new NArrayCodec[Byte](count, element.asInstanceOf, ByteArrayBuilder(_))).asInstanceOf
      case _: Short => (new NArrayCodec[Short](count, element.asInstanceOf, ShortArrayBuilder(_))).asInstanceOf
      case _: Int => (new NArrayCodec[Int](count, element.asInstanceOf, IntArrayBuilder(_))).asInstanceOf
      case _: Float => (new NArrayCodec[Float](count, element.asInstanceOf, FloatArrayBuilder(_))).asInstanceOf
      case _: Double => (new NArrayCodec[Double](count, element.asInstanceOf, DoubleArrayBuilder(_))).asInstanceOf
      case _ => (new NArrayCodec[AnyRef](count, element.asInstanceOf, NativeArrayBuilder[AnyRef](_))).asInstanceOf
    }

  private def sqFunctionProtoCounteds(encObj: Codec[SqObject], functionProtoCodec: Codec[SqFunctionProto]): Codec[
    (NArray[SqObject], NArray[SqObject], NArray[SqOuterValue], NArray[SqLocalVarInfo],
      NArray[SqLineInfo], NArray[Int], NArray[SqInstruction], NArray[SqFunctionProto])
  ] =
    (int32L :: int32L :: int32L :: int32L :: int32L :: int32L :: int32L :: int32L).consume {
      case (nLiterals, nParameters, nOuterValues, nLocalVarInfos, nLineInfos, nDefaultParams, nInstructions, nFunctions) =>
        val lits = sqClosureStreamPart ~> narrOfN(provide(nLiterals), encObj)
        val params = sqClosureStreamPart ~> narrOfN(provide(nParameters), encObj)
        val outerVals = sqClosureStreamPart ~> narrOfN(provide(nOuterValues), sqOuterValue(encObj))
        val localVars = sqClosureStreamPart ~> narrOfN(provide(nLocalVarInfos), sqLocalVarInfo(encObj))
        val lineInfos = sqClosureStreamPart ~> narrOfN(provide(nLineInfos), sqLineInfo)
        val defaultParams = sqClosureStreamPart ~> narrOfN(provide(nDefaultParams), int32L)
        val instructions = sqClosureStreamPart ~> narrOfN(provide(nInstructions), sqInstruction)
        val functions = sqClosureStreamPart ~> narrOfN(provide(nFunctions), functionProtoCodec)
        lits :: params :: outerVals :: localVars :: lineInfos :: defaultParams :: instructions :: functions
    } {
      case (aLits, aParams, aOuterVals, aLocalVars, aLineInfos, aDefaultParams, aInstructions, aFunctions) =>
        (aLits.length, aParams.length, aOuterVals.length, aLocalVars.length, aLineInfos.length, aDefaultParams.length, aInstructions.length, aFunctions.length)
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
