package tf.bug.worker

import scala.scalajs.js
import scala.scalajs.js.Promise
import scala.scalajs.js.annotation.{JSImport, JSName}
import scala.scalajs.js.typedarray.{Float32Array, Int32Array, Uint32Array}

@js.native
trait DragonboxApi extends js.Object {

  @JSName("HEAP32")
  val heapI32: Int32Array = js.native

  @JSName("HEAPU32")
  val heapU32: Uint32Array = js.native
  
  @JSName("HEAPF32")
  val heapF32: Float32Array = js.native

  @JSName("UTF8ToString")
  def utf8ToString(ptr: Int, maxBytesToRead: js.UndefOr[Int] = ()): String =
    js.native

  @JSName("_realloc")
  def realloc(ptr: Int, size: Int): Int =
    js.native

  @JSName("_free")
  def free(ptr: Int): Unit =
    js.native

  @JSName("_bulk_dragonbox")
  def bulkDragonbox(intsPtr: Int, count: Int): Int =
    js.native

}

object DragonboxApi {

  @js.native
  @JSImport("/wasm/buildDir/dragonbox.js", JSImport.Default)
  private val wasmModule: js.Function1[js.Dictionary[js.Dynamic], js.Promise[DragonboxApi]] = js.native

  def get: Promise[DragonboxApi] =
    wasmModule.apply(js.Dictionary())

}

