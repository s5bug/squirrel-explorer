package tf.bug

import narr.native.NativeArrayBuilder
import narr.{ByteArrayBuilder, DoubleArrayBuilder, FloatArrayBuilder, IntArrayBuilder, NArray, NArrayBuilder, ShortArrayBuilder}
import scala.collection.{Factory, mutable}

final case class NArrayFactory[T](builder: () => NArrayBuilder[T]) extends Factory[T, NArray[T]] {
  override def fromSpecific(it: IterableOnce[T]): NArray[T] = {
    val b = newBuilder
    b.addAll(it)
    b.result()
  }

  override def newBuilder: mutable.Builder[T, NArray[T]] = {
    new mutable.Builder[T, NArray[T]] {
      var nativeBuilder: NArrayBuilder[T] = builder()

      override def clear(): Unit = {
        nativeBuilder = builder()
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
  private val byteFn: () => ByteArrayBuilder = () => ByteArrayBuilder()
  private val shortFn: () => ShortArrayBuilder = () => ShortArrayBuilder()
  private val intFn: () => IntArrayBuilder = () => IntArrayBuilder()
  private val floatFn: () => FloatArrayBuilder = () => FloatArrayBuilder()
  private val doubleFn: () => DoubleArrayBuilder = () => DoubleArrayBuilder()
  private val anyRefFn: () => NativeArrayBuilder[AnyRef] = () => NativeArrayBuilder[AnyRef]()

  inline def of[T]: NArrayFactory[T] = inline compiletime.erasedValue[T] match {
    case _: Byte => NArrayFactory[Byte](byteFn).asInstanceOf
    case _: Short => NArrayFactory[Short](shortFn).asInstanceOf
    case _: Int => NArrayFactory[Int](intFn).asInstanceOf
    case _: Float => NArrayFactory[Float](floatFn).asInstanceOf
    case _: Double => NArrayFactory[Double](doubleFn).asInstanceOf
    case _ => NArrayFactory[AnyRef](anyRefFn).asInstanceOf
  }
}
