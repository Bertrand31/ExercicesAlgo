// https://leetcode.com/problems/zigzag-conversion/

object ZigzagConversion extends App {

  private def takeEveryOr(s: String, n1: Int, n2: Int, current: Int): List[Char] =
    s
      .lift(current - 1)
      .fold(List.empty)(_ :: takeEveryOr(s, n2, n1, current + n1))

  def convert(s: String, numRows: Int): String =
    (1 to numRows).flatMap(i =>
        if i == 1 || i == numRows then
          takeEveryOr(s, (numRows - 1) * 2, (numRows - 1) * 2, i)
        else
          takeEveryOr(s, (numRows - i) * 2, (i - 1) * 2, i)
    ).mkString

  println(convert("PAYPALISHIRING", 3))
  println(convert("PAYPALISHIRING", 4))
  println(convert("PAYPALISHIRING", 5))
}
