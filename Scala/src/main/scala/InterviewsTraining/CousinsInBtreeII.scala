// https://leetcode.com/problems/cousins-in-binary-tree-ii/

import scala.collection.mutable.Queue

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
    val nodesQueue = Queue(root)
    while nodesQueue.nonEmpty do
      var currentLevelSum = 0
      nodesQueue.foreach(n =>
        if n != null then currentLevelSum += n.value
      )
      (0 to math.max(0, (nodesQueue.length / 2) - 1)).foreach(_ =>
        val left = Option(nodesQueue.dequeue())
        val right = if !nodesQueue.isEmpty then Option(nodesQueue.dequeue()) else None
        val cousinsSum = currentLevelSum - (left.fold(0)(_.value) + right.fold(0)(_.value))
        left.foreach(l =>
          l.value = cousinsSum
          nodesQueue.enqueue(l.left, l.right)
        )
        right.foreach(r =>
          r.value = cousinsSum
          nodesQueue.enqueue(r.left, r.right)
        )
      )
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
