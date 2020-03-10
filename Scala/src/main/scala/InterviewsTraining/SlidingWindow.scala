// https://leetcode.com/problems/sliding-window-maximum/

object SlidingWindow {

  def maxSlidingWindow(nums: Array[Int], k: Int): Seq[Int] =
    if (nums.isEmpty || k < 1) Seq.empty
    else nums.sliding(k).map(_.max).toSeq
}

object SlidingWindowApp extends App {

  val arr = Array(1,3,-1,-3,5,3,6,7)
  assert(SlidingWindow.maxSlidingWindow(arr, 3) == Seq(3,3,5,5,6,7))
  assert(SlidingWindow.maxSlidingWindow(Array(), 0) == Seq())
}
