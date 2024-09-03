// https://leetcode.com/problems/zigzag-conversion/
import scala.annotation.tailrec

object ZigzagConversion extends App {

  @tailrec
  private def takeEveryOr(s: String, n1: Int, n2: Int, current: Int, soFar: StringBuilder): StringBuilder =
    if s.sizeIs <= current - 1 then soFar
    else
      val step = math.max(1, n1)
      takeEveryOr(s, n2, step, current + step, soFar += s(current - 1))

  @tailrec
  private def concatLines(current: Int, limit: Int, s: String, soFar: StringBuilder): StringBuilder =
    if current > limit then soFar
    else
      val newSoFar =
        if current == 1 || current == limit then
          takeEveryOr(s, (limit - 1) * 2, (limit - 1) * 2, current, soFar)
        else
          takeEveryOr(s, (limit - current) * 2, (current - 1) * 2, current, soFar)
      concatLines(current + 1, limit, s, newSoFar)

  def convert(s: String, numRows: Int): String =
    concatLines(1, numRows, s, StringBuilder(s.size)).toString

  println(convert("A", 1))
  println(convert("AB", 1))
  println(convert("PAYPALISHIRING", 3))
  println(convert("PAYPALISHIRING", 4))
  println(convert("PAYPALISHIRING", 5))
}
