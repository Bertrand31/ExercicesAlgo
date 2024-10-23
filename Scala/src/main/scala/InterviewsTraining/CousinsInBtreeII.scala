// https://leetcode.com/problems/cousins-in-binary-tree-ii/
import scala.collection.mutable.ArrayBuffer

/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */

object CousinsInBtreeII:

  def replaceValueInTree(root: TreeNode): TreeNode =
    @annotation.tailrec
    def bfs(currentNodes: ArrayBuffer[TreeNode]): Unit =
      if currentNodes.isEmpty then ()
      else
        val nodesBelow = new ArrayBuffer[TreeNode](currentNodes.size * 2)
        var currentLevelSum = 0
        currentNodes.foreach(n =>
          if n != null then
            currentLevelSum += n.value
            nodesBelow += n.left
            nodesBelow += n.right
        )
        def walkThroughPairs(currentIndex: Int): Unit =
          if currentIndex > currentNodes.length - 1 then ()
          else
            val left = Option(currentNodes(currentIndex))
            val right = currentNodes.lift(currentIndex + 1).flatMap(Option(_))
            val cousinsSum = currentLevelSum - (left.fold(0)(_.value) + right.fold(0)(_.value))
            left.foreach(_.value = cousinsSum)
            right.foreach(_.value = cousinsSum)
            walkThroughPairs(currentIndex + 2)

        walkThroughPairs(0)
        bfs(nodesBelow)
    bfs(ArrayBuffer(root))
    root

object CousinsInBtreeIIApp extends App:

  {
    val tree = TreeNode(
      5,
      TreeNode(
        4,
        TreeNode(1),
        TreeNode(10),
      ),
      TreeNode(
        9,
        null,
        TreeNode(7),
      )
    )
    val res = CousinsInBtreeII.replaceValueInTree(tree)
    assert(res.value == 0)
    assert(res.left.value == 0)
    assert(res.left.left.value == 7)
    assert(res.left.right.value == 7)
    assert(res.right.value == 0)
    assert(res.right.right.value == 11)
  }
