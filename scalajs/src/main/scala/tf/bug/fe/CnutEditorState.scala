package tf.bug.fe

import tf.bug.cnut.RenderedCnut
import typings.codemirrorState.mod as codemirrorState

object CnutEditorState {

  val cnutAnnotationType: codemirrorState.AnnotationType[Option[RenderedCnut]] =
    codemirrorState.Annotation.define[Option[RenderedCnut]]()

  val cnutField: codemirrorState.StateField[Option[RenderedCnut]] = codemirrorState.StateField.define(new codemirrorState.StateFieldSpec[Option[RenderedCnut]] {
    override def create(state: codemirrorState.EditorState): Option[RenderedCnut] = None
    override def update(current: Option[RenderedCnut], transaction: codemirrorState.Transaction): Option[RenderedCnut] = {
      transaction.annotation(cnutAnnotationType) match {
        case next: Option[?] => next
        case _ => current
      }
    }
  })

}
