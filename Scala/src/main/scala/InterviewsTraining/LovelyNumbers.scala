// How many times within a given range is there a number
// which contains at least 3 occurences of the same digit.

import scala.annotation.tailrec

object LovelyNumbers {

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

  def countLovelyNumbers(a: Int, b: Int): Int = (a to b) count isLovely
}

object LovelyNumbersApp extends App {

  println(LovelyNumbers.countLovelyNumbers(1, 111))
}

