object MedianOfTwoSortedArrays {

  private def findMedianInternal(
    arr1: Array[Int], arr1LeftBound: Int, arr1RightBound: Int,
    arr2: Array[Int], arr2LeftBound: Int, arr2RightBound: Int,
    target: Int,
    isMedianAnAverage: Boolean,
  ): Int =
    println(s"${arr1.toList}, leftBound: $arr1LeftBound, rightBound: $arr1RightBound")
    println(s"${arr2.toList}, leftBound: $arr2LeftBound, rightBound: $arr2RightBound")
    println(s"target: $target, isMedianAnAverage: $isMedianAnAverage")
    val arr1BoundedLength = arr1RightBound - arr1LeftBound + 1
    val arr2BoundedLength = arr2RightBound - arr2LeftBound + 1
    if arr1BoundedLength + arr2BoundedLength < target then
      throw Error(s"Target $target is out of bounds")
    else if arr1BoundedLength + arr2BoundedLength == target then
      // TODO: handle medianIsAverage
      math.max(arr1(arr1RightBound), arr2(arr2RightBound))
    else if arr1LeftBound > arr2RightBound then
      arr2(arr2RightBound)
    else if arr2LeftBound > arr2RightBound then
      arr1(arr1RightBound)
    else if target == 0 then
      // TODO: handle medianIsAverage
      math.min(arr1(arr1LeftBound), arr2(arr2LeftBound))
    else
      val arr1Pivot = arr1LeftBound + arr1BoundedLength / 2
      val arr2Pivot = arr2LeftBound + arr2BoundedLength / 2
      val arr1PivotValue = arr1(arr1Pivot)
      val arr2PivotValue = arr2(arr2Pivot)
      println(s"arr1pivot: $arr1PivotValue, arr2pivot: $arr2PivotValue")
      println("=============================================")
      if arr1PivotValue <= arr2PivotValue then
        findMedianInternal(
          arr1, arr1LeftBound, arr1RightBound,
          arr2, arr2LeftBound, arr2RightBound - arr2BoundedLength / 2,
          target,
          isMedianAnAverage,
        )
      else
        findMedianInternal(
          arr1, arr1LeftBound, arr1RightBound - arr1BoundedLength / 2,
          arr2, arr2LeftBound, arr2RightBound,
          target,
          isMedianAnAverage,
        )

  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double =
    findMedianInternal(
      nums1, 0, nums1.size - 1,
      nums2, 0, nums2.size - 1,
      (nums1.size + nums2.size) / 2,
      (nums1.size + nums2.size) % 2 == 0,
    )
}

object MedianOfTwoSortedArraysApp extends App {

  val left = Array(1, 3, 5, 7, 9)
  val right = Array(6, 8, 10, 12, 14)
  val res = MedianOfTwoSortedArrays.findMedianSortedArrays(left, right)
  println(res)
}
