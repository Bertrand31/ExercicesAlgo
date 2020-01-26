import scala.annotation.tailrec
import cats.implicits._
import cats.effect._

object SecretSantaPure {

  type Person = String
  type Pairings = Map[Person, Person]

  @tailrec
  private def pickReceiver(people: Array[Person], pairings: Pairings, indexToPair: Int, seed: Seed): Person = {
    val randIndex = (Math.abs(seed.long) % people.length).toInt
    if (randIndex === indexToPair || pairings.contains(people(randIndex)))
      pickReceiver(people, pairings, indexToPair, seed.next)
    else
      people(randIndex)
  }

  @tailrec
  private def makePairs(people: Array[Person], current: Int, soFar: Pairings, seed: Seed): Pairings =
    if (current >= people.length) soFar.map(_.swap)
    else {
      val giver = people(current)
      val receiver = pickReceiver(people, soFar, current, seed)
      makePairs(people, current + 1, soFar + (receiver -> giver), seed)
    }

  def makePairs(people: Array[Person], randSeed: IO[Long]): IO[Pairings] =
    randSeed.map(Seed).map(makePairs(people, 0, Map(), _))
}

object SecretSantaPureApp extends IOApp {

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
    SecretSantaPure
      .makePairs(names, randomSeed)
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
