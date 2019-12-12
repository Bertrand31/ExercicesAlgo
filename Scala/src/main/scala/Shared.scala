import scala.reflect.ClassTag
import scala.annotation.tailrec

// Knuth’s 64-bit linear congruential generator
final case class Seed(long: Long) {
  def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
}

object ArrayShuffling {

  @tailrec
  private def fisherYates[A: ClassTag](array: Array[A], seed: Seed, currentIndex: Int): Array[A] =
    if (currentIndex >= array.length) array
    else {
      val randIndex = (Math.abs(seed.long) % (array.length - currentIndex)).toInt + currentIndex
      val a = array(currentIndex)
      val b = array(randIndex)
      val swappedArray = array.updated(currentIndex, b).updated(randIndex, a)
      fisherYates(swappedArray, seed.next, currentIndex + 1)
    }

  def fisherYates[A: ClassTag](array: Array[A], randomSeed: Seed): Array[A] =
    fisherYates(array, randomSeed, 0)
}
