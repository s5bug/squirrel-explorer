package tf.bug.fe

import typings.monacoEditor.esmVsEditorEditorDotapiMod as monaco

case class RenderResult(
  rawText: String,
  hints: scalajs.js.Array[monaco.languages.InlayHint],
  markers: scalajs.js.Array[monaco.editor.IMarkerData]
)

object RenderResult {
  final val empty: RenderResult =
    RenderResult("", scalajs.js.Array(), scalajs.js.Array())
}
