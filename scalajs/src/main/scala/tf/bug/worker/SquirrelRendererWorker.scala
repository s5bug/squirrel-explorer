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
    js.Dynamic.global.onmessage =
      (e => onMessage(e.data.asInstanceOf[String | Uint8Array])): js.Function1[MessageEvent, Unit]
  }

  def returnMessage(content: String): Unit =
    js.Dynamic.global.postMessage(content)
  
  def onMessage(cnutBytesOrCommandJson: String | Uint8Array): Unit = {
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
          if(this.encodingSjis) Cnut.cnutSjis.decode(bv.bits)
          else Cnut.cnutUtf8.decode(bv.bits)
        
        parsed match {
          case Attempt.Successful(DecodeResult(value, _)) =>
            val renderer = new MutableCnutRender
            value.renderInto(this.renderLineInfos, 0, renderer)
            
            returnMessage(renderer.rawText())
            returnMessage(renderer.hintsJson())
            returnMessage(renderer.markersJson())
          case Attempt.Failure(cause) => returnMessage(s"[error] $cause")
        }
    }
    
  }
  
}
