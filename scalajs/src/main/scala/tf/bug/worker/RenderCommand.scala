package tf.bug.worker

import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.github.plokhotnyuk.jsoniter_scala.core.*

sealed trait RenderCommand
final case class RenderLineInfos(value: Boolean) extends RenderCommand
final case class RenderEncodingSjis(value: Boolean) extends RenderCommand

object RenderCommand {
  given renderCommandJsonEncoder: JsonValueCodec[RenderCommand] = JsonCodecMaker.make
}
