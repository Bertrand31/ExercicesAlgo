object TreeSums {

  def numberOfSubsets(arr: Array[Int], target: Int): Int = {
    val runningSums = arr.foldLeft(Vector[Int]())((acc, item) =>
      acc :+ (acc.lastOption.getOrElse(0) + item)
    )
    val mapOfSums = runningSums.foldLeft(Map[Int, Int]())((acc, item) => {
      val newCount = acc.getOrElse(item, 0) + 1
      acc + (item -> newCount)
    })
    runningSums.map(item => {
      val difference = item - target
      mapOfSums.getOrElse(difference, 0)
    }).sum
  }
}

object TreeSumsApp extends App {

  val arr = Array(10, 5, 1, 2, -1, -1, 7, 1, 2)
  println(TreeSums.numberOfSubsets(arr, 8))
}
