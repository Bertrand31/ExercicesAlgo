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

  println("|" + ZalandoTwo.solution("Codility We test coders", 14) + "|")
  println("|" + ZalandoTwo.solution("Codility", 14) + "|")
  println("|" + ZalandoTwo.solution("Codility", 5) + "|")
}

