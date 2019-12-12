import scala.reflect.ClassTag
import scala.annotation.tailrec
import cats.implicits._

object FisherYates {

  @tailrec
  private def shuffle[A: ClassTag](array: Array[A], seed: Seed, currentIndex: Int): Array[A] =
    if (currentIndex === 0) array
    else {
      val randIndex = (Math.abs(seed.long) % currentIndex).toInt
      val a = array(currentIndex)
      val b = array(randIndex)
      val swappedArray = array.updated(currentIndex, b).updated(randIndex, a)
      shuffle(swappedArray, seed.next, currentIndex - 1)
    }

  def shuffle[A: ClassTag](array: Array[A], randomSeed: Seed): Array[A] =
    shuffle(array, randomSeed, array.length - 1)
}

