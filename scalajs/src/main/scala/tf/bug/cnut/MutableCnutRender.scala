package tf.bug.cnut

import narr.*
import scala.annotation.tailrec
import scala.scalajs.js
import scala.scalajs.js.typedarray.Int32Array
import tf.bug.worker.DragonboxApi

final class MutableCnutRender(dboxApi: DragonboxApi) { self =>

  private var lineNumber: Int = 1
  private var column: Int = 1

  private val fragments: scalajs.js.Array[String] = scalajs.js.Array()

  def reset(): Unit = {
    lineNumber = 1
    column = 1
    fragments.length = 0
  }

  def line(): Unit = {
    fragments.push("\n")
    this.lineNumber = this.lineNumber + 1
    this.column = 1
  }

  @tailrec def space(length: Int): Unit = {
    if length > 256 then {
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

  def renderArray[A](
    indent: Int,
    array: NArray[A],
    renderElem: A => Unit
  ): Unit = {
    this.fragment("[")
    this.line()

    val maxIdx = array.length
    val maxIdxStr = maxIdx.toString
    val maxIdxLen = maxIdxStr.length

    var idx = 0
    while idx < array.length do {
      this.space(2 + indent)

      renderElem(array(idx))

      this.fragment(",")
      this.line()

      idx = idx + 1
    }

    this.space(indent)
    this.fragment("]")
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

  inline def renderArrayField[A](
    inline name: String,
    inline indent: Int,
    inline arr: NArray[A],
    inline renderElem: A => Unit
  ): Unit = {
    this.renderField[A](name, indent, () => this.renderArray(indent, arr, renderElem))
  }

  def rawText(): String = {
    fragments.join("")
  }

}

object MutableCnutRender {

  case class FloatMarker(lineNo: Int, start: Int, end: Int)
  
  private final val twoFiveSixSpaces =
    "                                                                                                                                                                                                                                                                "

  private final val signStrings: js.Array[String] =
    js.Array[String]("+", "-")

}
