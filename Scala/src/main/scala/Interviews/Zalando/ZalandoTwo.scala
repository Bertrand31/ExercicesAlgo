// Truncate the input string after the given number of characters, or less.
// The output string must not finish with a space, of contain a truncated word.

import scala.annotation.tailrec

object ZalandoTwo {

  def solution(message: String, k: Int): String = {

    @tailrec
    def walkThroughChars(chars: List[Char], wordIsOpen: Boolean = false): List[Char] =
      if ((chars.length <= k && !wordIsOpen) || chars.isEmpty) chars
      else {
        val head +: tail = chars
        if (head == ' ') walkThroughChars(tail, false)
        else walkThroughChars(tail, true)
      }

    val reversedList = message.reverse.toList
    walkThroughChars(reversedList).reverse.mkString
  }
}

object ZalandoTwoApp extends App {

  println("|" + ZalandoTwo.solution("Codility We test coders", 14) + "|") // "Codility We"
  println("|" + ZalandoTwo.solution("Codility", 14) + "|") // "Codility"
  println("|" + ZalandoTwo.solution("Codility", 5) + "|") // ""
}

