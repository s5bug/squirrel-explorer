package tf.bug.worker

import com.github.plokhotnyuk.jsoniter_scala
import org.scalajs.dom
import org.scalajs.dom.MessageEvent
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scodec.{Attempt, DecodeResult}
import scodec.bits.{BitVector, ByteVector}
import tf.bug.cnut.Cnut

object SquirrelCompilerWorker {

  def main(args: Array[String]): Unit = {
    val accumulator: js.Array[String] = js.Array()
    js.Dynamic.global.onmessage =
      (e => accumulator.push(e.data.asInstanceOf[String])): js.Function1[MessageEvent, Unit]

    WasmApi.get.`then` { (wasm: WasmApi) =>
      val outBuf = wasm.outBufferInit()
      val vm = wasm.sqOpen()
      val emptyString = wasm.stringToNewUtf8("")

      js.Dynamic.global.onmessage =
        (e => returnMessage(onMessage(wasm, vm, emptyString, outBuf, e.data.asInstanceOf[String]))): js.Function1[MessageEvent, Unit]

      if(accumulator.nonEmpty) {
        val recent = accumulator.last
        returnMessage(onMessage(wasm, vm, emptyString, outBuf, recent))
      }
    }
  }

  def returnMessage(content: String | Uint8Array): Unit =
    js.Dynamic.global.postMessage(content.asInstanceOf[js.Any])

  def onMessage(wasm: WasmApi, vm: Int, sourceName: Int, outBuf: Int, content: String): String | Uint8Array = {
    val allocateContent = wasm.stringToNewUtf8(content)
    val contentLength = wasm.lengthBytesUtf8(content)
    val compiledLength = wasm.compileAndSerializeBuffer(vm, allocateContent, contentLength, sourceName, outBuf)
    wasm.free(allocateContent)
    
    if (compiledLength != 0) {
      val contentPtr = wasm.outBufferContent(outBuf)
      wasm.heapU8.slice(contentPtr, contentPtr + compiledLength)
    } else "[error] Compilation failed"
  }

}
