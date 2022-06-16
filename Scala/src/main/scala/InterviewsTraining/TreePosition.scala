/** Given the following tree:
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
  * Print the following: "g bf ad c e".
  * The orders of the letters within a group do not matter,
  * so the following would also be valid: "g fb da c e" or even "g fb ad c e".
  *
  * The tree structure is to be implemented, and both the construction of the tree and the
  * generation of the output have to be performed in O(n) time and O(n) space.
  */

object TreePosition {

  import ArrayUtils._

  final case class Node(value: Char, left: Option[Node] = None, right: Option[Node] = None)

  private def getNodesPositions(tree: Option[Node], position: Int = 0): Array[(Int, Char)] =
    tree.fold(Array.empty[(Int, Char)])(node => {
      val left = getNodesPositions(node.left, position - 1)
      val right = getNodesPositions(node.right, position + 1)
      left ++ right :+ (position -> node.value)
    })

  private def makePositionsArray(node: Node): Array[List[Char]] = {
    val positionsArray = getNodesPositions(Some(node))
    val (leftMost, rightMost) = positionsArray.minAndMaxBy(_._1) // Method from ArrayUtils
    val offset = -leftMost
    val breadth = rightMost + offset + 1
    positionsArray.foldLeft(new Array[List[Char]](breadth))((acc, tuple) => {
      val (position, char) = tuple
      val targetIndex = position + offset
      val newIndexValues = Option(acc(targetIndex)).fold(List(char))(char +: _)
      acc.updated(targetIndex, newIndexValues)
    })
  }

  val groupCharsByTreeColumn: Node => String =
    makePositionsArray(_).map(_.mkString).mkString(" ")
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
  val acceptable = Seq("g bf ad c e", "g fb da c e", "g fb ad c e")
  assert(acceptable contains result)
}
