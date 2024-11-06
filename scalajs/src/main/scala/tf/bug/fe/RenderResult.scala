package tf.bug.fe

case class RenderResult(
  rawText: String,
  hints: scalajs.js.Array[typings.monacoEditor.mod.languages.InlayHint],
  markers: scalajs.js.Array[typings.monacoEditor.mod.editor.IMarkerData]
)

object RenderResult {
  final val empty: RenderResult =
    RenderResult("", scalajs.js.Array(), scalajs.js.Array())
}
