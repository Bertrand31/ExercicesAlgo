// Given an array of distinct values, return all the possible subsets of this array

import cats.implicits._


/** Where n is the array length, there are 2^n possible subsets we can generate from that array.
  * This is because every item of the array can either taken or not (2 possible states),
  * over n items. Hence the 2^n possible combinations.
  * So we generate all numbers from 0 to 2^n - 1, then use their binary representations (which are
  * going to be n bits long) to decide which index we keep from the array.
  *
  * These two solutions take O(n) time, but use very different techniques. The first one converts
  * each number to a string of binary digits, while the second uses bit shifting to test where
  * the 1s can be found within the binary representation of a number that stays in the decimal base.
  */
object AllSubsets {

  def get[A](arr: Array[A]): IndexedSeq[IndexedSeq[A]] =
    (0 until Math.pow(2, arr.size).toInt)
      .map(
        _
          .toBinaryString
          .reverse
          .zip(Iterator from 0)
          .collect({ case ('1', index) => arr(index) })
      )

  def getWithBitShifting[A](arr: Array[A]): IndexedSeq[IndexedSeq[A]] =
    (0 until Math.pow(2, arr.size).toInt)
      .map(nb =>
        (0 until arr.size).collect({
          case shift if (nb & ~(1 << shift)) =!= nb => arr(shift)
        })
      )
}

object AllSubsetsApp extends App {

  import scala.util.Random

  val lists = (0 to 20).map(_ => (0 to 5).map(_ => Random.between(0, 1000)).toArray)
  lists.foreach(list => {
    val a = AllSubsets.get(list)
    val b = AllSubsets.getWithBitShifting(list)
    assert(a == b)
  })
}
