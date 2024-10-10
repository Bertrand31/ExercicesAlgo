// https://leetcode.com/problems/maximum-width-ramp/

import scala.collection.mutable.PriorityQueue

object MaximumWidthRamp:

  private def seekFirstGeq(arr: Array[Int], target: Int, until: Int, idx: Int): Option[(Int, Int)] =
    if idx < until then None
    else
      val currentValue = arr(idx)
      if currentValue >= target then Some((currentValue, idx))
      else seekFirstGeq(arr, target, until, idx - 1)

  def maxWidthRamp(nums: Array[Int]): Int =

    def testOneByOneIfRelevant(
      nums: Array[Int],
      current: Int,
      maxLeftValueSoFar: Option[Int],
      bestResult: Int,
    ): Int =
      if nums.length - 1 - current <= bestResult then bestResult
      else if current >= nums.length then bestResult
      else if nums(current) >= maxLeftValueSoFar.getOrElse(Int.MaxValue) then 
        testOneByOneIfRelevant(nums, current + 1, maxLeftValueSoFar, bestResult)
      else
        seekFirstGeq(nums, nums(current), current + 1, nums.length - 1) match
          case None => 
            testOneByOneIfRelevant(
              nums,
              current + 1,
              maxLeftValueSoFar.map(math.max(nums(current), _)),
              bestResult,
            )
          case Some((value, i)) =>
            testOneByOneIfRelevant(
              nums,
              current + 1,
              maxLeftValueSoFar.map(math.max(value, _)),
              math.max(bestResult, i - current)
            )
    testOneByOneIfRelevant(nums, 0, None, 0)

object MaximumWidthRampApp extends App:

  {
    val arr = Array(6, 0, 8, 2, 1, 5)
    val res = MaximumWidthRamp.maxWidthRamp(arr)
    println(res)
  }
  {
    val arr = Array(9, 8, 1, 0, 1, 9, 4, 0, 4, 1)
    val res = MaximumWidthRamp.maxWidthRamp(arr)
    println(res)
  }
  {
    val arr = Array(0, 1)
    val res = MaximumWidthRamp.maxWidthRamp(arr)
    println(res)
  }
