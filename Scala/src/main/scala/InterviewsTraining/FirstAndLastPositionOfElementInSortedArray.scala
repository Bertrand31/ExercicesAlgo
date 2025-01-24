object FirstAndLastPositionOfElementInSortedArray:

    @annotation.tailrec
    private def seekLeftMost(nums: Array[Int], target: Int, left: Int, right: Int): Option[Int] =
      if right < left then None
      else
        val pivot = left + (right - left) / 2 
        val pivotValue = nums(pivot)
        if pivotValue < target then
          seekLeftMost(nums, target, pivot + 1, right)
        else if pivotValue > target then
          throw new Error("Canont happen")
        else if nums.lift(pivot - 1).fold(true)(_ < target) then
          Some(pivot)
        else // pivotValue == target
          seekLeftMost(nums, target, left, pivot - 1)

    @annotation.tailrec
    private def seekRightMost(nums: Array[Int], target: Int, left: Int, right: Int): Option[Int] =
      if right < left then None
      else
        val pivot = left + (right - left) / 2 
        val pivotValue = nums(pivot)
        if pivotValue > target then
          seekRightMost(nums, target, left, pivot - 1)
        else if pivotValue < target then
          throw new Error("Canont happen")
        else if nums.lift(pivot + 1).fold(true)(_ > target) then
          Some(pivot)
        else // pivotValue == target
          seekRightMost(nums, target, pivot + 1, right)

    @annotation.tailrec
    private def searchRangeInternal(nums: Array[Int], target: Int, left: Int, right: Int): Array[Int] =
      if right < left then
        Array(-1, -1)
      else
        val pivot = left + (right - left ) / 2
        val pivotValue = nums(pivot)
        if pivotValue == target then
          val leftMost = seekLeftMost(nums, target, left, pivot - 1).getOrElse(pivot)
          val rightMost = seekRightMost(nums, target, pivot + 1, right).getOrElse(pivot)
          Array(leftMost, rightMost)
        else if pivotValue > target then
          searchRangeInternal(nums, target, left, pivot - 1)
        else // pivotValue < target
          searchRangeInternal(nums, target, pivot + 1, right)

    def searchRange(nums: Array[Int], target: Int): Array[Int] =
      searchRangeInternal(nums, target, 0, nums.size - 1)

object FirstAndLastPositionOfElementInSortedArrayApp extends App:

  val arr = Array(5, 7, 7, 8, 8, 10)
  assert(FirstAndLastPositionOfElementInSortedArray.searchRange(arr, 8).toList == List(3, 4))
  assert(FirstAndLastPositionOfElementInSortedArray.searchRange(arr, 6).toList == List(-1, -1))
  assert(FirstAndLastPositionOfElementInSortedArray.searchRange(Array(), 0).toList == List(-1, -1))
  assert(FirstAndLastPositionOfElementInSortedArray.searchRange(Array(1), 1).toList == List(0, 0))
  assert(FirstAndLastPositionOfElementInSortedArray.searchRange(Array(2, 2), 2).toList == List(0, 1))
  val arr2 = Array(0, 0, 0, 0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 8, 8, 9, 9, 10, 10, 11, 11)
  assert(FirstAndLastPositionOfElementInSortedArray.searchRange(arr2, 0).toList == List(0, 3))
