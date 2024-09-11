package tf.bug.worker

import java.nio.charset.{Charset, StandardCharsets}
import org.scalajs.dom.MessageEvent
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scodec.{Attempt, DecodeResult}
import scodec.bits.ByteVector
import tf.bug.cnut.Cnut

object SquirrelExplorerWorker {

  def main(args: Array[String]): Unit = {
    js.Dynamic.global.onmessage =
      (e => onMessage(e.data.asInstanceOf[Uint8Array])): js.Function1[MessageEvent, Unit]
  }

  def returnMessage(content: String): Unit =
    js.Dynamic.global.postMessage(content)

  def onMessage(content: Uint8Array): Unit = {
    val bytes = ByteVector.fromUint8Array(content)

    val cnut = Cnut.cnutSjis.decode(bytes.bits)
    cnut match {
      case Attempt.Successful(DecodeResult(value, _)) =>
        val response = value.doc.renderTrim(0)
        returnMessage(response)
      case Attempt.Failure(err) => throw new RuntimeException(err.toString)
    }
  }

}
