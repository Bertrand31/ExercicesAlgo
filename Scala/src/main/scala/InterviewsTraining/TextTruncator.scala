// Truncate the input string after the given number of characters, or less.
// The output string must not finish with a space, of contain a truncated word.

import scala.annotation.tailrec

object TextTruncator {

  def truncate(message: String, k: Int): String = {

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

object TextTruncatorApp extends App {

  println("|" + TextTruncator.truncate("Codility We test coders", 14) + "|") // "Codility We"
  println("|" + TextTruncator.truncate("Codility", 14) + "|") // "Codility"
  println("|" + TextTruncator.truncate("Codility", 5) + "|") // ""
}

