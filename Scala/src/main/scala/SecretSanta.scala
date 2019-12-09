import scala.util.Random
import scala.annotation.tailrec

object SecretSanta {

  type Person = String
  type Pairings = Map[Person, Person]

  @tailrec
  private def pickReceiver(people: Array[Person], pairings: Pairings, indexToPair: Int): Person = {
    val randIndex = Random.nextInt(people.length)
    if (randIndex == indexToPair || pairings.contains(people(randIndex)))
      pickReceiver(people, pairings, indexToPair)
    else
      people(randIndex)
  }

  @tailrec
  private def makePairs(people: Array[Person], current: Int, soFar: Pairings): Pairings =
    if (current >= people.length) soFar.map(_.swap)
    else {
      val giver = people(current)
      val receiver = pickReceiver(people, soFar, current)
      makePairs(people, current + 1, soFar + (receiver -> giver))
    }

  // Could use optional arguments with default values instead of this proxy `makePairs` method,
  // but that would mean leaking those default arguments out and making the API unclean.
  def makePairs(people: Array[Person]): Pairings = makePairs(people, 0, Map())
}

object SecretSantaApp extends App {

  val names = Array(
    "Foo",
    "Bar",
    "Baz",
    "Test",
    "Bertrand",
  )

  SecretSanta.makePairs(names).foreach(pair => {
    val (giver, receiver) = pair
    println(giver + " fait un cadeau à " + receiver)
  })
}
