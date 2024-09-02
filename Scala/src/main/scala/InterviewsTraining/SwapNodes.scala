object SwapNodes extends App {

  final case class Tree(value: Int, left: Option[Tree], right: Option[Tree])

  private def unmarshallTree(indexes: Array[Array[Int]]): Tree =
    ???
    // val head = indexes.head
    // val leftValue = indexes(0) 
    // val rightValue = indexes(1)
    // val leftTree = 
    // Tree(value = 1, left = leftTree, right = rightTree)

  private def marshallTree(tree: Tree): Array[Array[Int]] =
    ???

  def swapNodes(indexes: Array[Array[Int]], queries: Array[Int]): Array[Array[Int]] =
    ???


  val sampleIndexes = Array(Array(2, 3), Array(-1, -1), Array(-1, -1))
  println(swapNodes(sampleIndexes, Array(1)))
}
