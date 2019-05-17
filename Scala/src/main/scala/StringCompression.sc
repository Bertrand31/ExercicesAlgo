// https://www.hackerrank.com/challenges/string-compression/problem

import scala.annotation.tailrec

object StringCompressor {

  def run(string: String): String = {
    @tailrec
    def compressRepeating(soFar: String, currentChar: Char, currentCount: Int, rest: List[Char]): String =
      rest match {
        case head +: Nil => soFar + s"$currentChar$currentCount" + s"$head${1}"
        case head +: tail if head == currentChar => compressRepeating(soFar, currentChar, currentCount + 1, tail)
        case head +: tail => compressRepeating(soFar + s"$currentChar$currentCount", head, 1, tail)
      }

    compressRepeating("", string.head, 1, string.tail.toCharArray.toList)
  }
}

val sampleString = "aaabccdeeef"
println(sampleString)
val compressed = StringCompressor.run(sampleString)
println(compressed)
assert(compressed == "a3b1c2d1e3f1")
