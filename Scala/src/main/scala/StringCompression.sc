// https://www.hackerrank.com/challenges/string-compression/problem

import scala.annotation.tailrec

object StringCompressor {

  // As an optimization, we don't transform patterns when the result would not be shorter
  private def patternToString(char: Char, count: Int): String =
    if (count > 2) s"$char$count"
    else char.toString * count

  @tailrec
  private def compressRepeating(soFar: String, char: Char, count: Int, rest: List[Char]): String =
    rest match {
      case head +: Nil => soFar + patternToString(char, count) + patternToString(head, 1)
      case head +: tail if head == char => compressRepeating(soFar, char, count + 1, tail)
      case head +: tail => compressRepeating(soFar + patternToString(char, count), head, 1, tail)
    }

  def compress(str: String): String =
    compressRepeating("", str.head, 1, str.tail.toCharArray.toList)

  @tailrec
  private def decompressRepeating(chars: List[Char], soFar: String = ""): String =
    chars match {
      case head +: Nil => soFar + head
      case head +: second +: Nil if Character.isDigit(second) =>
        soFar + (head.toString * second.asDigit)
      case head +: second +: Nil => soFar + head + second
      case head +: second +: tail if Character.isDigit(second) =>
        decompressRepeating(tail, soFar + (head.toString * second.asDigit))
      case head +: second +: tail => decompressRepeating(second +: tail, soFar + head)
    }

  def decompress(str: String): String = decompressRepeating(str.toCharArray.toList)
}

val sampleString = "aaabccdeeef"
println(sampleString)
val compressed = StringCompressor.compress(sampleString)
println(compressed)
assert(compressed == "a3bccde3f")
val decompressed = StringCompressor.decompress(compressed)
println(decompressed)
assert(decompressed == sampleString)
