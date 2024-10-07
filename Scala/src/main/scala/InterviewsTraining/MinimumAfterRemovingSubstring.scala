// https://leetcode.com/problems/minimum-string-length-after-removing-substrings/
import scala.collection.mutable

object MinimumStringLengthAfterRemovingSubstrings:

  def minLength(s: String): Int =
    val stack = new mutable.Stack[Char](s.length)
    s.foreach(char =>
      if !stack.isEmpty && (char == 'B' && stack.top == 'A' || char == 'D' && stack.top == 'C') then
        stack.pop()
      else
        stack.push(char)
    )
    stack.size

object MinimumStringLengthAfterRemovingSubstringsApp extends App:

  {
    val input = "ABFCACDB"
              // 01234567
              // 01  4567
    val res = MinimumStringLengthAfterRemovingSubstrings.minLength(input)
    assert(res == 2)
  }
  {
    val input = "ACBBD"
    val res = MinimumStringLengthAfterRemovingSubstrings.minLength(input)
    assert(res == 5)
  }
  {
    val input = "CCDDACDB"
    val res = MinimumStringLengthAfterRemovingSubstrings.minLength(input)
    assert(res == 0)
  }
  {
    val input = "CCCCABDDDD"
    val res = MinimumStringLengthAfterRemovingSubstrings.minLength(input)
    assert(res == 0)
  }
  {
    val input = "CCDDACDB"
    val res = MinimumStringLengthAfterRemovingSubstrings.minLength(input)
    assert(res == 0)
  }
  {
    val input = "CCDAABBDCD"
    val res = MinimumStringLengthAfterRemovingSubstrings.minLength(input)
    assert(res == 0)
  }
