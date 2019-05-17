// https://www.hackerrank.com/challenges/string-compression/problem

import scala.annotation.tailrec

object StringCompressor {

  def run(string: String): String = {
    @tailrec
    def compressRepeating(soFar: String, char: Char, count: Int, rest: List[Char]): String =
      rest match {
        case head +: Nil => soFar + s"$char$count" + s"$head${1}"
        case head +: tail if head == char => compressRepeating(soFar, char, count + 1, tail)
        case head +: tail => compressRepeating(soFar + s"$char$count", head, 1, tail)
      }

    compressRepeating("", string.head, 1, string.tail.toCharArray.toList)
  }
}

val sampleString = "aaabccdeeef"
println(sampleString)
val compressed = StringCompressor.run(sampleString)
println(compressed)
assert(compressed == "a3b1c2d1e3f1")
