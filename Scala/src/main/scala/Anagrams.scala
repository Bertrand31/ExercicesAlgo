// https://www.hackerrank.com/challenges/sherlock-and-anagrams/problem

object Solution extends App {

    type Anagram = (String, String)

    private def makeFootPrint: String => Map[Char, Int] =
      _.foldLeft(Map[Char, Int]())((acc, char) => {
        acc.get(char) match {
          case Some(count) => acc + (char -> (count + 1))
          case None => acc + (char -> 1)
        }
      })

    private def compareWords(s1: String, s2: String): Boolean =
      makeFootPrint(s1) == makeFootPrint(s2)

    def sherlockAndAnagrams(s: String, step: Int = 0, soFar: List[Anagram] = List()): Int = {
      val stringPivot = s.length - 2
      if (step > stringPivot) soFar.length
      else {
        val newAnagrams = (1 to stringPivot + 1).foldLeft(Vector[Anagram]())((acc, len) => {
          val pattern = s.slice(step, step + len)
          val rest = s.drop(step + 1)
          val anagrams =
            rest
              .toSeq
              .sliding(len)
              .map(_.unwrap)
              .withFilter(compareWords(_, pattern))
              .map((pattern, _))
          acc ++ anagrams
        })
        sherlockAndAnagrams(s, step + 1, soFar ++ newAnagrams)
      }
    }

  println(sherlockAndAnagrams("ifailuhkqq"))
}
