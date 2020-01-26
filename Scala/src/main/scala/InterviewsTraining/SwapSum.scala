// Is there two integers x and y such that arrayA.sum and arrayB.sum
// are equal if we swap arrayA(x) and arrayB(y) ?

import scala.collection.immutable.HashSet

object SwapSum {

  def solution(arrayA: Array[Int], arrayB: Array[Int]): Boolean = {
    val arrayBFootprint = HashSet(arrayB.toIndexedSeq:_*)
    val halfDifference = Math.abs(arrayA.sum - arrayB.sum) / 2
    arrayA.exists(value =>
      arrayBFootprint.contains(halfDifference + value)
    )
  }
}

object SwapSumApp extends App {
  println(SwapSum.solution(Array(1, 7, 3), Array(1, 1, 1)))
  println(SwapSum.solution(Array(1, 7, 3), Array(1, 5, 1)))
}
