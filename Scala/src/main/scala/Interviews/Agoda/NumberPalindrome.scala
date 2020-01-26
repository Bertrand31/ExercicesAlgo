import scala.annotation.tailrec

object NumberPalindrome {

  @tailrec
  private def numberToDigits(number: Int, soFar: List[Int] = List()): List[Int] =
    if (number == 0) soFar
    else {
      val newElem = number % 10
      val rest = number / 10
      numberToDigits(rest, newElem +: soFar)
    }

  def solution(number: Int): Boolean = {
    val digits = numberToDigits(number)
    digits == digits.reverse
  }
}

object NumberPalindromeApp extends App {

  NumberPalindrome.solution(14541) // true
  NumberPalindrome.solution(13541) // false
  NumberPalindrome.solution(1441) // true
  NumberPalindrome.solution(1341) // false
}

