package tf.bug.fe

case class RenderResult(
  rawText: String,
)

object RenderResult {
  final val empty: RenderResult =
    RenderResult("")
}
