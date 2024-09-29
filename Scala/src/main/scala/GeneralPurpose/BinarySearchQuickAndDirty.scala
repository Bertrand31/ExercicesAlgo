import annotation.tailrec

object BinarySearchQuickAndDirty:

  @tailrec
  private def binarySearchInternal(arr: Array[Int], target: Int, lowerBound: Int, upperBound: Int): Int =
    if lowerBound > upperBound then -1
    else
      val pivot = lowerBound + (upperBound - lowerBound) / 2
      val pivotValue = arr(pivot)
      if target < pivotValue then
        binarySearchInternal(arr, target, lowerBound, pivot - 1)
      else if target > pivotValue then
        binarySearchInternal(arr, target, pivot + 1, upperBound)
      else
        pivot

  def binarySearch(arr: Array[Int], target: Int): Int =
    binarySearchInternal(arr, target, 0, arr.size - 1)

object BinarySearchQuickAndDirtyApp extends App:

  val arr = Array(1, 2, 3, 4, 5, 6, 7)
               // 0  1  2  3  4  5  6
  println(BinarySearchQuickAndDirty.binarySearch(arr, 5))
  println(BinarySearchQuickAndDirty.binarySearch(arr, 0))
  println(BinarySearchQuickAndDirty.binarySearch(arr, 8))
  println(BinarySearchQuickAndDirty.binarySearch(Array(), 8))
  println(BinarySearchQuickAndDirty.binarySearch(Array(1), 8))
  println(BinarySearchQuickAndDirty.binarySearch(Array(1), 1))
  println(BinarySearchQuickAndDirty.binarySearch(Array(1, 2), 1))
  println(BinarySearchQuickAndDirty.binarySearch(Array(1, 2), 2))
