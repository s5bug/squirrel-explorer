package tf.bug.worker

import com.github.plokhotnyuk.jsoniter_scala
import org.scalajs.dom.MessageEvent
import scala.compiletime.uninitialized
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scodec.{Attempt, DecodeResult}
import scodec.bits.{BitVector, ByteVector}
import tf.bug.cnut.{Cnut, MutableCnutRender, SqClosure}

object SquirrelRendererWorker {

  private var renderLineInfos: Boolean = false
  private var encodingSjis: Boolean = false
  
  def main(args: Array[String]): Unit = {
    val accumulator: js.Array[String | Uint8Array] = js.Array()
    js.Dynamic.global.onmessage =
      (e => accumulator.push(e.data.asInstanceOf[String | Uint8Array])): js.Function1[MessageEvent, Unit]

    DragonboxApi.get.`then` { (dboxApi: DragonboxApi) =>
      val render = new MutableCnutRender(dboxApi)

      js.Dynamic.global.onmessage =
        (e => onMessage(render, e.data.asInstanceOf[String | Uint8Array])): js.Function1[MessageEvent, Unit]

      var i = 0
      while i < accumulator.length do {
        onMessage(render, accumulator(i))
        i += 1
      }
    }
  }

  def returnMessage(content: String): Unit =
    js.Dynamic.global.postMessage(content)
  
  def onMessage(render: MutableCnutRender, cnutBytesOrCommandJson: String | Uint8Array): Unit = {
    cnutBytesOrCommandJson match {
      case commandStr: String =>
        val command = jsoniter_scala.core.readFromString[RenderCommand](commandStr)
        command match {
          case RenderLineInfos(value) => this.renderLineInfos = value
          case RenderEncodingSjis(value) => this.encodingSjis = value
        }
      case cnutBytes: Uint8Array =>
        val bv = ByteVector.fromUint8Array(cnutBytes)
        val parsed =
          if this.encodingSjis then Cnut.cnutSjis.decode(bv.bits)
          else Cnut.cnutUtf8.decode(bv.bits)
        
        parsed match {
          case Attempt.Successful(DecodeResult(value, _)) =>
            render.reset()
            value.renderInto(this.renderLineInfos, 0, render)
            
            returnMessage(render.rawText())
            returnMessage(render.hintsJson())
            returnMessage(render.markersJson())
          case Attempt.Failure(cause) => returnMessage(s"[error] $cause")
        }
    }
    
  }
  
}
