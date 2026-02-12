package tf.bug.fe

import cats.effect.*
import fs2.Stream
import scalajs.js
import tf.bug.worker.DragonboxApi
import typings.codemirrorLint.mod as codemirrorLint

final class FloatDiagnosticBuilder private (private val dbox: DragonboxApi, private var intsPtr: Int, private var capacity: Int) {
  private val records: js.Array[FloatDiagnosticBuilder.FloatDiagnosticRecord] = js.Array()

  private inline def floatsCapacity: Int = capacity >> 2
  private inline def floatsCapacity_=(i: Int): Unit = { capacity = (i << 2) }
  private inline def intsPtrI32: Int = intsPtr >> 2

  def addFloatDiagnostic(from: Int, to: Int, bits: Int): IO[Unit] = IO {
    val offset = records.length
    dbox.heapI32(intsPtrI32 + offset) = bits
    records.push(FloatDiagnosticBuilder.FloatDiagnosticRecord(from, to))
    if records.length > floatsCapacity then {
      floatsCapacity = floatsCapacity * 2
      intsPtr = dbox.realloc(intsPtr, capacity)
    }
  }

  def build: Stream[IO, codemirrorLint.Diagnostic] = Stream.force(
    IO(dbox.bulkDragonbox(intsPtr, records.length)).flatMap(tripleIntsPtr => IO {
      val tripleIntsPtrU32 = tripleIntsPtr >> 2
      Stream.range(0, records.length).evalMap { i =>
        val significandAddress = tripleIntsPtrU32 + (i * 3)
        val exponentAddress = significandAddress + 1
        val negativeAddress = exponentAddress + 1

        IO {
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

          LezerCnutLanguage.makeInfoDiagnostic(
            this.records(i).from,
            this.records(i).to,
            insertDecimal
          )
        }
      }
    })
  )
}

object FloatDiagnosticBuilder {
  private final case class FloatDiagnosticRecord(from: Int, to: Int)

  def of(): Resource[IO, FloatDiagnosticBuilder] = {
    val loadApi = IO.fromPromise(IO { js.dynamicImport(() => DragonboxApi.get) })
    val getApi = loadApi.flatMap(f => IO.fromPromise(IO(f())))
    val acquire = getApi.flatMap { dbox =>
      val initialFloatCapacity = 64
      IO(dbox.realloc(0, 4 * initialFloatCapacity)).map { initialPtr =>
        new FloatDiagnosticBuilder(dbox, initialPtr, initialFloatCapacity)
      }
    }
    val release = (fdb: FloatDiagnosticBuilder) => IO {
      fdb.dbox.free(fdb.intsPtr)
    }

    Resource.make(acquire)(release)
  }
}
