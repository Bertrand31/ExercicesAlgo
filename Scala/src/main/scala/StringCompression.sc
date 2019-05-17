// https://www.hackerrank.com/challenges/string-compression/problem

object StringCompressor {

  type CompressedChar = (Char, Int)

  def run(string: String): String = {
    def makeTuple(currentChar: Char, currentCount: Int, rest: List[Char]): List[CompressedChar] = {
      rest match {
        case head +: Nil => List((currentChar, currentCount), (head, 1))
        case head +: tail if head == currentChar => makeTuple(currentChar, currentCount + 1, tail)
        case head +: tail => (currentChar, currentCount) +: makeTuple(head, 1, tail)
      }
    }
    makeTuple(string.head, 1, string.tail.toCharArray.toList)
      .map(t => t._1 + t._2.toString)
      .mkString
  }
}

val sampleString = "aaabccdeeef"
println(sampleString)
val compressed = StringCompressor.run(sampleString)
println(compressed)
assert(compressed == "a3b1c2d1e3f1")
