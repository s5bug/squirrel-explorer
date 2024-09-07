package tf.bug

import cats.effect.IO
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, JSName}
import typings.std.Uint8Array

@js.native
trait WasmApi extends js.Object {

  @JSName("HEAPU8")
  val heapU8: Uint8Array = js.native
  
  @JSName("UTF8ToString")
  def utf8ToString(ptr: Int, maxBytesToRead: js.UndefOr[Int] = ()): String =
    js.native
  
  @JSName("lengthBytesUTF8")
  def lengthBytesUtf8(str: String): Int =
    js.native

  @JSName("stringToNewUTF8")
  def stringToNewUtf8(str: String): Int =
    js.native

  @JSName("_free")
  def free(ptr: Int): Unit =
    js.native

  @JSName("_sq_open")
  def sqOpen(): Int =
    js.native
    
  @JSName("_sq_close")
  def sqClose(vm: Int): Unit =
    js.native

  @JSName("_out_buffer_init")
  def outBufferInit(): Int =
    js.native
  
  @JSName("_out_buffer_close")
  def outBufferClose(ptr: Int): Unit =
    js.native

  @JSName("_out_buffer_content")
  def outBufferContent(ptr: Int): Int =
    js.native

  @JSName("_compile_and_serialize_buffer")
  def compileAndSerializeBuffer(vm: Int, buffer: Int, bufferSize: Int, sourceName: Int, outBuffer: Int): Int =
    js.native

}

object WasmApi {

  @js.native
  @JSImport("/wasm/api.js", JSImport.Default)
  private val wasmModule: js.Function1[js.Dictionary[js.Dynamic], js.Promise[WasmApi]] = js.native

  def get: IO[WasmApi] =
    IO.fromPromise(IO.delay {
      wasmModule.apply(js.Dictionary())
    })

}
