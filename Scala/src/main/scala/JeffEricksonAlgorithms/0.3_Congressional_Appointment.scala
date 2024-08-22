import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec
import scala.util.Random

object CongressionalAppointment extends App {

  final case class State(id: String, population: Int, appointed: Int)

  private def appointMembers(populations: Array[Int], seats: Int): Seq[State] =
    val states = populations.zipWithIndex.map({
      case (pop, i) => State(id = i.toString, population = pop, appointed = 1)
    })
    val pq = PriorityQueue[State](states*)(Ordering.by(state => 
        state.population / math.sqrt(state.appointed * (state.appointed + 1))
    ))

    @tailrec
    def fillSeats(remaining: Int): Unit =
      if remaining == 0 then ()
      else
        val currentState = pq.dequeue()
        val updatedState = currentState.copy(appointed = currentState.appointed + 1)
        pq.enqueue(updatedState)
        fillSeats(remaining - 1)

    fillSeats(seats - states.size)
    pq.dequeueAll

  val statesNumber = 50
  val seats = 435
  val fakePopulations = Array.fill(statesNumber)(Random().between(1_000, 10_000_000))
  assert(appointMembers(fakePopulations, seats).map(_.appointed).sum == seats)
}
