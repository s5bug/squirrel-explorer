package tf.bug

import cats.effect.{IO, Resource}
import scodec.bits.ByteVector
import tf.bug.Sq.{ByteBuffer, OutBuffer, SqVm, Utf8Buffer}

abstract class Sq {

  def nullBuffer: Utf8Buffer

  def utf8ToString(buf: Utf8Buffer): String
  def lengthBytesUtf8(str: String): Int
  def stringToUtf8(str: String): Resource[IO, Utf8Buffer]

  def bytesToByteVector(ptr: ByteBuffer, length: Int): IO[ByteVector]

  def sqVm: Resource[IO, SqVm]

  def outBuffer: Resource[IO, OutBuffer]
  def content(outBuffer: OutBuffer): IO[ByteBuffer]

  def compileAndSerializeBuffer(vm: SqVm, buffer: Utf8Buffer, bufferSize: Int, sourceName: Utf8Buffer, outBuffer: OutBuffer): IO[Int]

  def compileAndSerialize(vm: SqVm, input: String, sourceName: Option[String], outBuffer: OutBuffer): IO[ByteVector] =
    stringToUtf8(input).use { inBuf =>
      sourceName.fold(Resource.pure(nullBuffer))(stringToUtf8).use { sourceNameBuf =>
        compileAndSerializeBuffer(vm, inBuf, lengthBytesUtf8(input), sourceNameBuf, outBuffer).flatMap { length =>
          content(outBuffer).flatMap(c => bytesToByteVector(c, length))
        }
      }
    }

}

object Sq {

  opaque type ByteBuffer = Int
  opaque type Utf8Buffer = Int
  opaque type SqVm = Int
  opaque type OutBuffer = Int

  def of(wasm: WasmApi): Sq =
    new SqWasm(wasm)

  private final class SqWasm(private val wasm: WasmApi) extends Sq {

    override def nullBuffer: Utf8Buffer = 0

    override def utf8ToString(buf: Utf8Buffer): String = wasm.utf8ToString(buf)
    override def lengthBytesUtf8(str: String): Int = wasm.lengthBytesUtf8(str)
    override def stringToUtf8(str: String): Resource[IO, Utf8Buffer] =
      Resource.make(IO.delay(wasm.stringToNewUtf8(str)))(ptr => IO.delay(wasm.free(ptr)))

    override def bytesToByteVector(ptr: ByteBuffer, length: Int): IO[ByteVector] =
      IO.delay(ByteVector.fromUint8Array(wasm.heapU8.slice(ptr, ptr + length)))

    override def sqVm: Resource[IO, SqVm] =
      Resource.make(IO.delay(wasm.sqOpen()))(ptr => IO.delay(wasm.sqClose(ptr)))

    override def outBuffer: Resource[IO, OutBuffer] =
      Resource.make(IO.delay(wasm.outBufferInit()))(ptr => IO.delay(wasm.outBufferClose(ptr)))

    override def content(outBuffer: OutBuffer): IO[ByteBuffer] =
      IO.delay(wasm.outBufferContent(outBuffer))

    override def compileAndSerializeBuffer(vm: SqVm, buffer: Utf8Buffer, bufferSize: SqVm, sourceName: Utf8Buffer, outBuffer: OutBuffer): IO[SqVm] =
      IO.delay(wasm.compileAndSerializeBuffer(vm, buffer, bufferSize, sourceName, outBuffer))

  }
}
