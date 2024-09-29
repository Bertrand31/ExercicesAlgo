import annotation.tailrec

object BinarySearch:

  @tailrec
  private def binarySearchInternal[T](arr: Array[T], compare: T => Int, left: Int, right: Int): Option[Int] =
    if left > right then None
    else
      val pivot = left + ((right - left ) / 2)
      val pivotValue = arr(pivot)
      compare(pivotValue) match
        case 0  => Some(pivot)
        case -1 => binarySearchInternal(arr, compare, pivot + 1, right)
        case 1  => binarySearchInternal(arr, compare, left, pivot - 1)

  def binarySearch[T](arr: Array[T], target: T)(using ord: Ordering[T]): Option[Int] =
    binarySearchInternal(arr, ord.compare(_, target), 0, arr.size - 1)

object BinarySearchApp extends App:
        
  val arr = Array(1, 2, 3, 4, 5, 6, 7)
               // 0  1  2  3  4  5  6
  println(BinarySearch.binarySearch[Int](arr, 5))
  println(BinarySearch.binarySearch[Int](arr, 0))
  println(BinarySearch.binarySearch[Int](arr, 8))
  println(BinarySearch.binarySearch[Int](Array(), 8))
  println(BinarySearch.binarySearch[Int](Array(1), 8))
  println(BinarySearch.binarySearch[Int](Array(1), 1))
  println(BinarySearch.binarySearch[Int](Array(1, 2), 1))
  println(BinarySearch.binarySearch[Int](Array(1, 2), 2))
