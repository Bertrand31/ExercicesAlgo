// Given a dictionary (a list of strings) find all anagrams for a given word.
// Anagrams are words that contain the same letters, only in a different order.

import cats.implicits._
import cats.effect._

object DictionaryAnagrams:

  private def makeFootprint(word: String): Map[Char, Int] =
    word.groupMapReduce(identity)(_ => 1)(_ + _)

  def findAnagrams(dictionary: Array[String], word: String): Array[String] =
    val wordFootprint = makeFootprint(word)
    dictionary.filter(w =>
      w.size === word.size && makeFootprint(w) === wordFootprint && w =!= word
    )

object DictionaryAnagramsApp extends IOApp:

  private val Dictionary = Array(
    "friend",
    "redfin",
    "abcsde",
    "refind",
    "friende",
    "asdasdsa",
    "asdgadgad",
  )

  private val ExpectedAnagrams = Array("redfin", "refind")

  def run(args: List[String]): IO[ExitCode] =
    val results = DictionaryAnagrams.findAnagrams(Dictionary, "friend")
    IO
      .fromEither(Either.catchOnly[AssertionError](results sameElements ExpectedAnagrams))
      .productR(IO.print(s"Success! Found ${results.mkString("\"", ", ", "\"")}.\n"))
      .as(ExitCode.Success)
