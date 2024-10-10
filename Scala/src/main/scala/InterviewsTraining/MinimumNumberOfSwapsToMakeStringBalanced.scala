// https://leetcode.com/problems/minimum-number-of-swaps-to-make-the-string-balanced/

object MinimumNumberOfSwapsToMakeStringBalanced:
    
  private def countUnclosed(s: String, i: Int, unmatchedOpen: Int, unmatchedClosed: Int): Int =
    if i >= s.size || s.isEmpty then unmatchedClosed
    else
      s(i) match
        case '[' => countUnclosed(s, i + 1, unmatchedOpen + 1, unmatchedClosed)
        case ']' if unmatchedOpen == 0 => countUnclosed(s, i + 1, 0, unmatchedClosed + 1)
        case ']' => countUnclosed(s, i + 1, unmatchedOpen - 1, unmatchedClosed)
        case ___ => countUnclosed(s, i + 1, unmatchedOpen, unmatchedClosed)

  def minSwaps(s: String): Int =
    val unclosed = countUnclosed(s, 0, 0, 0)
    unclosed / 2 + unclosed % 2


object MinimumNumberOfSwapsToMakeStringBalancedApp extends App:

  Seq(
    ("][][", 1),
    ("]]][[[", 2),
    ("[[[]]]][][]][[]]][[[", 2),
  ).foreach({
    case (input, expected) =>
    assert(MinimumNumberOfSwapsToMakeStringBalanced.minSwaps(input) == expected)
  })
