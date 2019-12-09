import scala.annotation.tailrec

// Knuth’s 64-bit linear congruential generator
final case class Seed(long: Long) {
  def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
}

object SecretSantaPure {

  type Person = String
  type Pairings = Map[Person, Person]

  @tailrec
  private def pickReceiver(people: Array[Person], pairings: Pairings, indexToPair: Int, seed: Seed): Person = {
    val randIndex = (seed.long % people.length).toInt
    if (randIndex == indexToPair || pairings.contains(people(randIndex)))
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
      makePairs(people, current + 1, soFar + (receiver -> giver), seed.next)
    }

  // Could use optional arguments with default values instead of this proxy `makePairs` method,
  // but that would mean leaking those default arguments out and making the API unclean.
  def makePairs(people: Array[Person], randSeed: Long): Pairings =
    makePairs(people, 0, Map(), Seed(Math.abs(randSeed)))
}

object SecretSantaPureApp extends App {

  import scala.util.Random

  val names = Array(
    "Foo",
    "Bar",
    "Baz",
    "Test",
    "Bertrand",
  )

  val rand = Random.nextLong

  SecretSantaPure.makePairs(names, rand).foreach(pair => {
    val (giver, receiver) = pair
    println(giver + " fait un cadeau à " + receiver)
  })
}
