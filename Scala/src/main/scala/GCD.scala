// https://www.hackerrank.com/challenges/functional-programming-warmups-in-recursion---gcd/problem

import scala.annotation.tailrec

object GCDCalculator extends App {

  @tailrec
  private def getGCD(biggest: Long, smallest: Long): Long = {
    val rest = biggest % smallest
    if (rest == 0) smallest
    else getGCD(smallest, rest)
  }

  def get(a: Long, b: Long): Long =
    if (a == b) a
    else getGCD(Math.max(a, b), Math.min(a, b))

  assert(get(45, 18) == 9)
  assert(get(123, 40) == 1)
  assert(get(128093810, 312312) == 26)
  assert(get(4798, 1123) == 1)
  assert(get(123456789, 123456789) == 123456789)
}
