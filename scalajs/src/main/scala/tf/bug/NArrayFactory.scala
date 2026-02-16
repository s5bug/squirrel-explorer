package tf.bug

import narr.native.NativeArrayBuilder
import narr.{ByteArrayBuilder, DoubleArrayBuilder, FloatArrayBuilder, IntArrayBuilder, NArray, NArrayBuilder, ShortArrayBuilder}
import scala.collection.{Factory, mutable}

final case class NArrayFactory[T](builder: Int => NArrayBuilder[T]) extends Factory[T, NArray[T]] {
  override def fromSpecific(it: IterableOnce[T]): NArray[T] = {
    val b = newBuilder
    b.sizeHint(it.knownSize)
    b.addAll(it)
    b.result()
  }

  override def newBuilder: mutable.Builder[T, NArray[T]] = {
    new mutable.Builder[T, NArray[T]] {
      var nativeBuilder: NArrayBuilder[T] = builder(NArrayBuilder.DefaultInitialSize)

      override def sizeHint(size: Int): Unit = {
        if nativeBuilder.size == 0 && size > 0 then {
          nativeBuilder = builder(size)
        }
      }
      
      override def clear(): Unit = {
        nativeBuilder = builder(NArrayBuilder.DefaultInitialSize)
      }
      override def result(): NArray[T] = nativeBuilder.result
      override def addOne(elem: T): this.type = {
        nativeBuilder.addOne(elem)
        this
      }
    }
  }
}

object NArrayFactory {
  private val byteFn: Int => ByteArrayBuilder = initCapacity => ByteArrayBuilder(initCapacity)
  private val shortFn: Int => ShortArrayBuilder = initCapacity => ShortArrayBuilder(initCapacity)
  private val intFn: Int => IntArrayBuilder = initCapacity => IntArrayBuilder(initCapacity)
  private val floatFn: Int => FloatArrayBuilder = initCapacity => FloatArrayBuilder(initCapacity)
  private val doubleFn: Int => DoubleArrayBuilder = initCapacity => DoubleArrayBuilder(initCapacity)
  private val anyRefFn: Int => NativeArrayBuilder[AnyRef] = initCapacity => NativeArrayBuilder[AnyRef](initCapacity)

  inline def of[T]: NArrayFactory[T] = inline compiletime.erasedValue[T] match {
    case _: Byte => NArrayFactory[Byte](byteFn).asInstanceOf
    case _: Short => NArrayFactory[Short](shortFn).asInstanceOf
    case _: Int => NArrayFactory[Int](intFn).asInstanceOf
    case _: Float => NArrayFactory[Float](floatFn).asInstanceOf
    case _: Double => NArrayFactory[Double](doubleFn).asInstanceOf
    case _ => NArrayFactory[AnyRef](anyRefFn).asInstanceOf
  }
}
