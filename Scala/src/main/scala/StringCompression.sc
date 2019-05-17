// https://www.hackerrank.com/challenges/string-compression/problem

// Using a case class makes the code slightly cleaner and simpler than using a tuple because of the
// ability to override `toString`. However, this also has a runtime cost so in a real-life,
// performance-sensitive environment we'd probably go with representing CompressedChar with a
// simple (String, Int) tuple type alias.
case class CompressedChar(char: Char, count: Int) {
  override def toString(): String = char + count.toString
}

object StringCompressor {

  def run(string: String): String = {
    def makeTuple(currentChar: Char, currentCount: Int, rest: List[Char]): List[CompressedChar] =
      rest match {
        case head +: Nil => List(CompressedChar(currentChar, currentCount), CompressedChar(head, 1))
        case head +: tail if head == currentChar => makeTuple(currentChar, currentCount + 1, tail)
        case head +: tail => CompressedChar(currentChar, currentCount) +: makeTuple(head, 1, tail)
      }

    makeTuple(string.head, 1, string.tail.toCharArray.toList).mkString
  }
}

val sampleString = "aaabccdeeef"
println(sampleString)
val compressed = StringCompressor.run(sampleString)
println(compressed)
assert(compressed == "a3b1c2d1e3f1")
