package tf.bug.cnut

import scala.annotation.tailrec
import scala.scalajs.js
import scala.scalajs.js.typedarray.Int32Array
import tf.bug.worker.DragonboxApi

final class MutableCnutRender(dboxApi: DragonboxApi) { self =>

  private var lineNumber: Int = 1
  private var column: Int = 1

  private val fragments: scalajs.js.Array[String] = scalajs.js.Array()
  private val hints: scalajs.js.Array[typings.monacoEditor.mod.languages.InlayHint] = scalajs.js.Array()
  private val markers: scalajs.js.Array[typings.monacoEditor.mod.editor.IMarkerData] = scalajs.js.Array()
  private val floatMarkerData: scalajs.js.Array[MutableCnutRender.FloatMarker] = scalajs.js.Array()
  private var floatMarkerBitsOffset: Int = dboxApi.realloc(0, 1024)
  private var floatMarkersBitsCapacity: Int = 1024

  def reset(): Unit = {
    lineNumber = 1
    column = 1
    fragments.length = 0
    hints.length = 0
    markers.length = 0
    floatMarkerData.length = 0
  }

  private inline def ensureFloatsBitsSpace(): Unit = {
    if((4 * (1 + floatMarkerData.length)) >= floatMarkersBitsCapacity) {
      floatMarkersBitsCapacity <<= 1
      floatMarkerBitsOffset = dboxApi.realloc(floatMarkerBitsOffset, floatMarkersBitsCapacity)
    }
  }

  private def pushFloatBits(floatMarker: MutableCnutRender.FloatMarker, intBits: Int): Unit = {
    ensureFloatsBitsSpace()
    val intIndex = floatMarkerBitsOffset >> 2
    dboxApi.heapI32.set(intIndex + floatMarkerData.length, intBits)
    floatMarkerData.push(floatMarker)
  }

  def line(): Unit = {
    fragments.push("\n")
    this.lineNumber = this.lineNumber + 1
    this.column = 1
  }

  @tailrec def space(length: Int): Unit = {
    if(length > 256) {
      this.fragment(MutableCnutRender.twoFiveSixSpaces)
      space(length - 256)
    } else {
      this.fragment(MutableCnutRender.twoFiveSixSpaces.substring(256 - length))
    }
  }

  def fragment(text: String): Unit = {
    fragments.push(text)
    column += text.length
  }
  
  def fragmentInfo(text: String, info: String): Unit = {
    val start = this.column
    this.fragment(text)
    val end = this.column

    val marker = typings.monacoEditor.mod.editor.IMarkerData(
      message = info,
      startLineNumber = self.lineNumber,
      endLineNumber = self.lineNumber,
      startColumn = start,
      endColumn = end,
      // we can't directly use MarkerSeverity.Info because the "monaco" import references DOM which pulls vite HMR into
      // a web worker, which fails because web workers don't have DOM
      severity = 2.asInstanceOf[typings.monacoEditor.mod.MarkerSeverity]
    )
    this.markers.push(marker)
  }

  def fragmentInfoFloat(text: String, infoBits: Int): Unit = {
    val start = this.column
    this.fragment(text)
    val end = this.column

    val floatMarkerData = MutableCnutRender.FloatMarker(self.lineNumber, start, end)
    pushFloatBits(floatMarkerData, infoBits)
  }

  def renderVector[A](
    indent: Int,
    vector: Vector[A],
    renderElem: A => Unit
  ): Unit = {
    this.fragment("Vector(")
    this.line()

    val maxIdx = vector.length
    val maxIdxStr = maxIdx.toString
    val maxIdxLen = maxIdxStr.length

    var idx = 0
    val iter = vector.iterator
    while (iter.hasNext) {
      this.space(2 + indent)

      val idxStr = idx.toString
      val spaceStr = MutableCnutRender.twoFiveSixSpaces.substring(256 - (maxIdxLen - idxStr.length))
      this.indexHint(spaceStr ++ idxStr)

      val next = iter.next()
      renderElem(next)

      this.fragment(",")
      this.line()

      idx = idx + 1
    }

    this.space(indent)
    this.fragment(")")
  }

  inline def renderField[A](
    inline name: String,
    inline indent: Int,
    inline renderF: () => Unit
  ): Unit = {
    this.space(indent)
    this.fragment(name)
    this.fragment(" = ")
    renderF()
    this.fragment(",")
    this.line()
  }

  inline def renderFieldLast[A](
    inline name: String,
    inline indent: Int,
    inline renderF: () => Unit
  ): Unit = {
    this.space(indent)
    this.fragment(name)
    this.fragment(" = ")
    renderF()
  }

  inline def renderFieldInline[A](
    inline name: String,
    inline indent: Int,
    inline renderF: () => Unit
  ): Unit = {
    this.fragment(name)
    this.fragment(" = ")
    renderF()
    this.fragment(", ")
  }

  inline def renderFieldInlineLast[A](
    inline name: String,
    inline indent: Int,
    inline renderF: () => Unit
  ): Unit = {
    this.fragment(name)
    this.fragment(" = ")
    renderF()
  }

  inline def renderVectorField[A](
    inline name: String,
    inline indent: Int,
    inline vec: Vector[A],
    inline renderElem: A => Unit
  ): Unit = {
    this.renderField[A](name, indent, () => this.renderVector(indent, vec, renderElem))
  }

  def indexHint(idx: String): Unit = {
    val p = typings.monacoEditor.mod.IPosition(self.column, self.lineNumber)
    val h = typings.monacoEditor.mod.languages.InlayHint(idx, p)
    h.paddingRight = true
    this.hints.push(h)
  }

  def rawText(): String = {
    fragments.join("")
  }

  def hintsJson(): String = {
    scalajs.js.JSON.stringify(this.hints)
  }
  
  def markersJson(): String = {
    val wasmIntArray = dboxApi.bulkDragonbox(floatMarkerBitsOffset, floatMarkerData.length)
    val i32idx = wasmIntArray >> 2

    var i = 0
    while(i < floatMarkerData.length) {
      val significandAddress = i32idx + (i * 3)
      val exponentAddress = significandAddress + 1
      val negativeAddress = exponentAddress + 1

      val significandU32: Double = dboxApi.heapU32.get(significandAddress)

      val asPreciseDouble: Double = if(significandU32 != 0) {
        val exponentI32: Int = dboxApi.heapI32.get(exponentAddress)
        val negativeB: Int = dboxApi.heapI32.get(negativeAddress)

        js.Dynamic.global.Number.parseFloat(
          MutableCnutRender.signStrings(negativeB) ++
            significandU32.toString ++
            "e" ++
            exponentI32.toString
        ).asInstanceOf[Double]
      } else {
        val f32bits: Double = dboxApi.heapF32.get(exponentAddress)
        f32bits
      }

      val fakeMarker = floatMarkerData(i)
      val realMarker = typings.monacoEditor.mod.editor.IMarkerData(
        message = asPreciseDouble.toString ++ "f",
        startLineNumber = fakeMarker.lineNo,
        endLineNumber = fakeMarker.lineNo,
        startColumn = fakeMarker.start,
        endColumn = fakeMarker.end,
        // we can't directly use MarkerSeverity.Info because the "monaco" import references DOM which pulls vite HMR into
        // a web worker, which fails because web workers don't have DOM
        severity = 2.asInstanceOf[typings.monacoEditor.mod.MarkerSeverity]
      )
      this.markers.push(realMarker)

      i += 1
    }

    scalajs.js.JSON.stringify(this.markers)
  }

}

object MutableCnutRender {

  case class FloatMarker(lineNo: Int, start: Int, end: Int)
  
  private final val twoFiveSixSpaces =
    "                                                                                                                                                                                                                                                                "

  private final val signStrings: js.Array[String] =
    js.Array[String]("+", "-")

}
