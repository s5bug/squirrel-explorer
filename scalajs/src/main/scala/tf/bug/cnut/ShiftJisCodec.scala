package tf.bug.cnut

import scodec.Attempt.Failure
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import typings.std.TextDecodeOptions
import typings.std.global.TextDecoder

object ShiftJisCodec extends Codec[String] {

  override def decode(bits: BitVector): Attempt[DecodeResult[String]] = {
    val decoder = new TextDecoder("shift-jis")
    
    val textDecodeOptions = new TextDecodeOptions {}
    textDecodeOptions.stream = false
    
    val result = decoder.decode(bits.bytes.toUint8Array, textDecodeOptions)
    Attempt.successful(DecodeResult.apply(result, BitVector.empty))
  }

  override def encode(value: String): Attempt[BitVector] = {
    Failure(scodec.Err("Encoding as Shift_JIS not supported"))
  }

  override def sizeBound: SizeBound = SizeBound.unknown

}
