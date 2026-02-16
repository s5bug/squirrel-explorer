package tf.bug.cnut

import scala.scalajs.js
import tf.bug.worker.DragonboxApi

final case class Diagnostic(
  from: Int,
  to: Int,
  message: String,
)

object Diagnostic {

  private final case class FloatRecord(from: Int, to: Int)

  final class Builder private (private val dragonbox: DragonboxApi, private var intsPtr: Int, private var capacity: Int) {

    def addDiagnostic(from: Int, to: Int, msg: String): Unit = {
      accumulator.push(Diagnostic(from, to, msg))
    }

    def addDiagnostic(node: RenderedCnut.Node, msg: String): Unit = {
      this.addDiagnostic(node.from, node.to, msg)
    }

    def visit(raw: SqInstruction, node: RenderedCnut.Instruction, context: SqFunctionProto, opIdx: Int): Unit = {
      raw.diagnostic(this, node, context, opIdx)
    }

    private val accumulator: js.Array[Diagnostic] = js.Array()
    private val floatAccumulator: js.Array[FloatRecord] = js.Array()

    def reset(): Unit = {
      accumulator.length = 0
      floatAccumulator.length = 0
    }

    private inline def floatsCapacity: Int = capacity >> 2
    private inline def floatsCapacity_=(i: Int): Unit = { capacity = (i << 2) }
    private inline def intsPtrI32: Int = intsPtr >> 2

    def addFloatDiagnostic(from: Int, to: Int, bits: Int): Unit = {
      val offset = floatAccumulator.length
      dragonbox.heapI32(intsPtrI32 + offset) = bits
      floatAccumulator.push(FloatRecord(from, to))
      if floatAccumulator.length > floatsCapacity then {
        floatsCapacity = floatsCapacity * 2
        intsPtr = dragonbox.realloc(intsPtr, capacity)
      }
    }

    def addFloatDiagnostic(node: RenderedCnut.Node, bits: Int): Unit = {
      this.addFloatDiagnostic(node.from, node.to, bits)
    }

    private def moveFloatsToAccumulator(): Unit = {
      val tripleIntsPtr = dragonbox.bulkDragonbox(intsPtr, floatAccumulator.length)
      val tripleIntsPtrU32 = tripleIntsPtr >> 2

      var i = 0
      while i < floatAccumulator.length do {
        val significandAddress = tripleIntsPtrU32 + (i * 3)
        val exponentAddress = significandAddress + 1
        val negativeAddress = exponentAddress + 1

        val significandU32: Double = dragonbox.heapU32.get(significandAddress)

        val asPreciseDouble: Double = if significandU32 != 0 then {
          val exponentI32: Int = dragonbox.heapI32.get(exponentAddress)
          val negativeB: Int = dragonbox.heapI32.get(negativeAddress)

          val signString = if negativeB == 1 then "-" else ""
          js.Dynamic.global.Number.parseFloat(
            signString ++
              significandU32.toString ++
              "e" ++
              exponentI32.toString
          ).asInstanceOf[Double]
        } else {
          val f32bits: Double = dragonbox.heapF32.get(exponentAddress)
          f32bits
        }

        val asString = asPreciseDouble.toString
        val insertDecimal = if asString.contains('.') || asString.contains('e') then asString else s"${asString}.0"

        accumulator.push(Diagnostic(floatAccumulator(i).from, floatAccumulator(i).to, insertDecimal))

        i += 1
      }

      floatAccumulator.length = 0
    }

    def build: IArray[Diagnostic] = {
      moveFloatsToAccumulator()

      val result = IArray.unsafeFromArray(accumulator.toArray)
      
      accumulator.length = 0
      
      result
    }

  }

  object Builder {
    def apply(dragonbox: DragonboxApi): Builder = {
      val initialFloatCapacity = 64
      val initialPtr = dragonbox.realloc(0, initialFloatCapacity << 2)
      new Builder(dragonbox, initialPtr, initialFloatCapacity)
    }
  }

}
