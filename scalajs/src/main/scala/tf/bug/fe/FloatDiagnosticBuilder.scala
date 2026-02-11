package tf.bug.fe

import scalajs.js
import tf.bug.worker.DragonboxApi
import typings.codemirrorLint.mod as codemirrorLint

final class FloatDiagnosticBuilder private (dbox: DragonboxApi, private var intsPtr: Int, private var capacity: Int) {
  private val records: js.Array[FloatDiagnosticBuilder.FloatDiagnosticRecord] = js.Array()

  private inline def floatsCapacity: Int = capacity >> 2
  private inline def floatsCapacity_=(i: Int): Unit = { capacity = (i << 2) }
  private inline def intsPtrI32: Int = intsPtr >> 2

  def addFloatDiagnostic(from: Int, to: Int, bits: Int): Unit = {
    val offset = records.length
    dbox.heapI32(intsPtrI32 + offset) = bits
    records.push(FloatDiagnosticBuilder.FloatDiagnosticRecord(from, to))
    if records.length > floatsCapacity then {
      floatsCapacity = floatsCapacity * 2
      intsPtr = dbox.realloc(intsPtr, capacity)
    }
  }

  def buildAndClean(diagnostics: js.Array[codemirrorLint.Diagnostic]): Unit = {
    val tripleIntsPtr = dbox.bulkDragonbox(intsPtr, records.length)
    val tripleIntsPtrU32 = tripleIntsPtr >> 2

    var i = 0
    while i < records.length do {
      val significandAddress = tripleIntsPtrU32 + (i * 3)
      val exponentAddress = significandAddress + 1
      val negativeAddress = exponentAddress + 1

      val significandU32: Double = dbox.heapU32.get(significandAddress)

      val asPreciseDouble: Double = if significandU32 != 0 then {
        val exponentI32: Int = dbox.heapI32.get(exponentAddress)
        val negativeB: Int = dbox.heapI32.get(negativeAddress)

        val signString = if negativeB == 1 then "-" else ""
        js.Dynamic.global.Number.parseFloat(
          signString ++
            significandU32.toString ++
            "e" ++
            exponentI32.toString
        ).asInstanceOf[Double]
      } else {
        val f32bits: Double = dbox.heapF32.get(exponentAddress)
        f32bits
      }

      val asString = asPreciseDouble.toString
      val insertDecimal = if asString.contains('.') || asString.contains('e') then asString else s"${asString}.0"

      diagnostics.push(LezerCnutLanguage.makeInfoDiagnostic(
        this.records(i).from,
        this.records(i).to,
        insertDecimal
      ))

      i += 1
    }

    dbox.free(intsPtr)
  }
}

object FloatDiagnosticBuilder {
  private final case class FloatDiagnosticRecord(from: Int, to: Int)

  def of(dbox: DragonboxApi): FloatDiagnosticBuilder = {
    val initialFloatCapacity = 64
    val initialPtr = dbox.realloc(0, 4 * initialFloatCapacity)
    new FloatDiagnosticBuilder(dbox, initialPtr, initialFloatCapacity)
  }
}
