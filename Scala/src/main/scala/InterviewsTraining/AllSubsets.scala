// Given an array of numbers, return all the possible subsets of this array

object AllSubsets {

  // Where n is the array length, there are 2^n possible subsets we can generate from that array.
  // This is because every item of the array can either taken or not (2 possible states),
  // over n items. Hence the 2^n possible combinations.
  // So we generate all numbers from 0 to 2^n - 1, then use their binary representations (which are
  // going to be n bits long) to decide which index we keep from the array.
  // This solution takes O(n) time.
  def get(arr: Array[Int]): IndexedSeq[IndexedSeq[Int]] =
    (0 until Math.pow(2, arr.size).toInt)
      .map(
        _
          .toBinaryString
          .reverse
          .zip(Iterator from 0)
          .collect({ case ('1', index) => arr(index) })
      )
}

object AllSubsetsApp extends App {

  println(AllSubsets.get(Array(2, 4, 8)))
}
