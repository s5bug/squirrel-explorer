package tf.bug.worker

import org.scalajs.dom
import org.scalajs.dom.MessageEvent
import scala.scalajs.js
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

      js.Dynamic.global.onmessage =
        (e => returnMessage(onMessage(wasm, vm, outBuf, e.data.asInstanceOf[String]))): js.Function1[MessageEvent, Unit]

      if(accumulator.nonEmpty) {
        val recent = accumulator.last
        returnMessage(onMessage(wasm, vm, outBuf, recent))
      }
    }
  }

  def returnMessage(content: String): Unit =
    js.Dynamic.global.postMessage(content)

  def onMessage(wasm: WasmApi, vm: Int, outBuf: Int, content: String): String = {
    val allocateContent = wasm.stringToNewUtf8(content)
    val contentLength = wasm.lengthBytesUtf8(content)
    val compiledLength = wasm.compileAndSerializeBuffer(vm, allocateContent, contentLength, 0, outBuf)
    wasm.free(allocateContent)

    if (compiledLength != 0) {
      val contentPtr = wasm.outBufferContent(outBuf)
      val bytes = ByteVector.fromUint8Array(wasm.heapU8.slice(contentPtr, contentPtr + compiledLength))

      val cnut = Cnut.cnutUtf8.decode(bytes.bits)
      cnut match {
        case Attempt.Successful(DecodeResult(value, _)) =>
          value.doc.renderTrim(0)
        case Attempt.Failure(err) =>
          "[error] " + err.toString
      }
    } else "[error] Compilation failed"
  }

}
