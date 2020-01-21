// How many times within a given range is there a number
// which contains at least 3 occurences of the same digit.

import scala.annotation.tailrec

object ZalandoOne {

  @tailrec
  private def walkThroughChars(chars: List[Char], soFar: Map[Char, Int] = Map()): Boolean =
    if (chars.isEmpty) true
    else {
      val head +: tail = chars
      val newCount = soFar.getOrElse(head, 0) + 1
      if (newCount >= 3) false
      else walkThroughChars(tail, soFar + (head -> newCount))
    }

  private def isLovely(n: Int): Boolean = walkThroughChars(n.toString.toList)

  def solution(a: Int, b: Int): Int = (a to b) count isLovely
}

object ZalandoOneApp extends App {

  println(ZalandoOne.solution(1, 111))
}

