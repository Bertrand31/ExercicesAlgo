object PerfUtils {

  import scala.collection.mutable.HashMap

  def memoizeUnary[A, B](fn: A => B): A => B = {
    val store = HashMap[A, B]()
    (arg: A) =>
      store.getOrElse(arg, {
        val result = fn(arg)
        store(arg) = result
        result
      })
  }
}

object ArrayUtils {

  import scala.reflect.ClassTag

  implicit class AugmentedArray[A: ClassTag](val arr: Array[A]) {

    def minAndMaxBy[T](fn: A => T)(implicit n: Numeric[T]): (T, T) = {
      import n._
      arr.foldLeft((zero, zero))((acc, item) => {
        val (currentMin, currentMax) = acc
        val currentValue = fn(item)
        (currentMin min currentValue, currentMax max currentValue)
      })
    }
  }
}
