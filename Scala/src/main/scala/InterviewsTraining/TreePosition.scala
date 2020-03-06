/** Given the following three:
  *
  *       a
  *      / \
  *     b   c
  *        / \
  *       d   e
  *      /
  *     f
  *    /
  *   g
  *
  * Print the following: g bf ad c e.
  * The orders of the letters within a group do not matter,
  * so the following would also be valid: g fb da c e.
  *
  * The tree structure is to be implemented, and both the construction of the tree and the
  * generation of the output have to be performed in O(n) time and O(n) space.
  *
  */

object TreePosition {

  final case class Node(value: Char, left: Option[Node] = None, right: Option[Node] = None)

  private def makePositionsArray(tree: Option[Node], position: Int = 0): Array[(Int, Char)] =
    tree.fold(Array.empty[(Int, Char)])(node => {
      val left = makePositionsArray(node.left, position - 1)
      val right = makePositionsArray(node.right, position + 1)
      (position -> node.value) +: (left ++ right)
    })

  private def makePositionMap(node: Node): (Map[Int, Array[Char]], Int, Int) = {
    val positionsArray = makePositionsArray(Some(node))
    val baseAccumulator = (Map.empty[Int, Array[Char]], 0, 0)
    positionsArray.foldLeft(baseAccumulator)((acc, tuple) => {
      val (currentMap, minPosition, maxPosition) = acc
      val (position, char) = tuple
      val newPair = currentMap.get(position) match {
        case Some(values) => (position -> (values :+ char))
        case None => (position -> Array(char))
      }
      val newMin = minPosition min position
      val newMax = maxPosition max position
      (currentMap + newPair, newMin, newMax)
    })
  }

  def groupCharsByTreeColumn(node: Node): String = {
    val (positionMap, minPosition, maxPosition) = makePositionMap(node)
    (minPosition to maxPosition)
      .map(positionMap(_).mkString)
      .mkString(" ")
  }
}

object TreePositionApp extends App {

  import TreePosition.Node

  val sampleTree = Node(
    value='a',
    left=Some(Node(value='b')),
    right=Some(
      Node(
        value='c',
        left=Some(
          Node(
            value='d',
            left=Some(
              Node(
                value='f',
                left=Some(Node(value='g')),
              ),
            ),
          )
        ),
        right=Some(Node(value='e')),
      ),
    ),
  )

  val result = TreePosition.groupCharsByTreeColumn(sampleTree)
  println(result)
}
