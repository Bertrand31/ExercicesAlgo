// https://www.hackerrank.com/challenges/diagonal-difference/problem

import scala.annotation.tailrec

object DiagonalDifference extends App {

  @tailrec
  private def getLeftRightDiagonal(arr: Array[Array[Int]], soFar: Int = 0): Int =
    arr match {
      case Array(Array(x: Int)) => soFar + x
      case _ => {
        val offsetArr = arr.drop(1).map(_.drop(1))
        getLeftRightDiagonal(offsetArr, soFar + arr(0)(0))
      }
    }

  @tailrec
  private def getRightLeftDiagonal(arr: Array[Array[Int]], soFar: Int = 0): Int =
    arr match {
      case Array(Array(x: Int)) => soFar + x
      case _ => {
        val offsetArr = arr.drop(1).map(_.take(arr.length - 1))
        getRightLeftDiagonal(offsetArr, soFar + arr(0)(arr.length - 1))
      }
    }

  def get(arr: Array[Array[Int]]): Int =
    Math.abs(getLeftRightDiagonal(arr) - getRightLeftDiagonal(arr))

  val sample = Array(
    Array(11, 2, 4),
    Array(4, 5, 6),
    Array(10, 8, -12),
  )
  assert(get(sample) == 15)
}
