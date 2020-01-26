import scala.annotation.tailrec
import cats.implicits._
import cats.effect._

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
    randNumber.map(Seed).map(seed => {
      val shuffled = FisherYates.shuffle(people, seed)
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
