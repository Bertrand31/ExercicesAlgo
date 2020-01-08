// https://codility.com/media/train/13-CaterpillarMethod.pdf
// Is there in a given array a sub-sequence which elements sum up to a given integer

object Caterpillar {

  private def caterpillar(
    arr: Array[Int], target: Int, lowerBound: Int, upperBound: Int, currentSum: Int
  ): Boolean =
    if (currentSum == target) true
    else if (lowerBound == arr.length - 1) false
    else if (upperBound == arr.length - 1 && currentSum < target) false
    else if (currentSum > target) {
      val newLowerBound = lowerBound + 1
      val newSum = currentSum - arr(newLowerBound)
      caterpillar(arr, target, newLowerBound, upperBound, newSum)
    }
    else { // currentSum < target
      val newUpperBound = upperBound + 1
      val newSum = currentSum + arr(newUpperBound)
      caterpillar(arr, target, lowerBound, newUpperBound, newSum)
    }

  def hasSubSequence(arr: Array[Int], target: Int): Boolean =
    caterpillar(arr, target, 0, 0, arr.head)
}

object CaterpillarApp extends App {

  println(Caterpillar.hasSubSequence(Array(6, 2, 7, 4, 1, 3, 6), 12))
  println(Caterpillar.hasSubSequence(Array(6, 7), 12))
  println(Caterpillar.hasSubSequence(Array(6), 12))
  println(Caterpillar.hasSubSequence(Array(1, 9, 11, 3, 4, 2, 0, 8), 29))
}
