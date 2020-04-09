import scala.annotation.tailrec

// Kadane's algorithm
object MaxSubArray {

  @tailrec
  private def findMaxSum(seq: Seq[Int], currentSum: Int, maxSum: Int): Int =
    if (seq.isEmpty) maxSum
    else {
      val newCurrentSum = 0 max (currentSum + seq.head)
      val newMaxSum = maxSum max newCurrentSum
      findMaxSum(seq.tail, newCurrentSum, newMaxSum)
    }

  def findMaxSum: Seq[Int] => Int = findMaxSum(_, 0, Int.MinValue)
}

object MaxSubArrayApp extends App {

  val seq = IndexedSeq(2, -4, 2, -1, 3, -3, 10, -1, -11, -100, 8, -1)
  println(MaxSubArray.findMaxSum(seq))
}
