// Let A be an array containing both positive and negative integers.
// Count the number of subsets of A that sum up to a target integer.

object SubsetSums {

  def numberOfSubsets(arr: Array[Int], target: Int): Int = {
    val runningSums = arr.foldLeft(Array.empty[Int])((acc, item) =>
      acc :+ (acc.lastOption.getOrElse(0) + item)
    )
    val mapOfSums = runningSums.groupMapReduce(identity)(_ => 1)(_ + _)
    runningSums.foldLeft(0)((acc, item) => acc + mapOfSums.getOrElse(item - target, 0))
  }
}

object SubsetSumsApp extends App {

  val arr = Array(10, 5, 1, 2, -1, -1, 7, 1, 2)
  assert(SubsetSums.numberOfSubsets(arr, 8) == 5)
}
