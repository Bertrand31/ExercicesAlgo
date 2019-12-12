import scala.reflect.ClassTag
import scala.annotation.tailrec
import cats.implicits._
import cats.effect._

// Knuth’s 64-bit linear congruential generator
final case class RandomSeed(long: Long) {
  def next = RandomSeed(long * 6364136223846793005L + 1442695040888963407L)
}

object ArrayShuffling {

  @tailrec
  private def fisherYates[A: ClassTag](array: Array[A], seed: RandomSeed, currentIndex: Int): Array[A] =
    if (currentIndex >= array.length) array
    else {
      val randIndex = (Math.abs(seed.long) % (array.length - currentIndex)).toInt + currentIndex
      val a = array(currentIndex)
      val b = array(randIndex)
      val swappedArray = array.updated(currentIndex, b).updated(randIndex, a)
      fisherYates(swappedArray, seed.next, currentIndex + 1)
    }

  def fisherYates[A: ClassTag](array: Array[A], randomSeed: RandomSeed): Array[A] =
    fisherYates(array, randomSeed, 0)
}

object SecretSanta2Pure {

  type Person = String
  type Pairings = Map[Person, Person]

  @tailrec
  private def makePairs(people: Array[Person], currentIndex: Int, soFar: Pairings): Pairings =
    if (currentIndex == people.length - 1) soFar + (people(currentIndex) -> people.head)
    else {
      val newMap = soFar + (people(currentIndex) -> people(currentIndex + 1))
      makePairs(people, currentIndex + 1, newMap)
    }

  def makePairs(people: Array[Person], randNumber: IO[Long]): IO[Pairings] =
    randNumber.map(RandomSeed).map(seed => {
      val shuffled = ArrayShuffling.fisherYates(people, seed)
      makePairs(shuffled, 0, Map())
    })
}

object SecretSanta2PureApp extends IOApp {

  import scala.util.Random

  private val names = Array(
    "Foo",
    "Bar",
    "Baz",
    "Test",
    "Bertrand",
  )

  private def print(str: String): IO[Unit] = IO { println(str) }

  def run(args: List[String]): IO[ExitCode] = {
    val randomSeed = IO { Random.nextLong }
    SecretSanta2Pure.makePairs(names, randomSeed)
      .flatMap(
        _
          .map(pair => {
            val (giver, receiver) = pair
            print(giver + " fait un cadeau à " + receiver)
          })
          .toList
          .parSequence
      )
      .as(ExitCode.Success)
  }
}
