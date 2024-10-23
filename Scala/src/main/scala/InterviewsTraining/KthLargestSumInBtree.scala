// https://leetcode.com/problems/kth-largest-sum-in-a-binary-tree/

import scala.collection.mutable.{ArrayBuffer, PriorityQueue}

// Class below is provided by leetcode
class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object KthLargestSumInBinaryTree:

  def kthLargestLevelSum(root: TreeNode, k: Int): Long =
    @annotation.tailrec
    def bfs(nodes: ArrayBuffer[TreeNode], heap: PriorityQueue[Long]): Long =
      if nodes.isEmpty then
        if heap.size == k then heap.head
        else -1
      else
        val nodesBelow = new ArrayBuffer[TreeNode](nodes.size * 2)
        var currentSum = 0l
        nodes.foreach(n =>
          currentSum += n.value
          if n.left != null then nodesBelow += n.left
          if n.right != null then nodesBelow += n.right
        )
        if heap.size < k then
          heap.enqueue(currentSum)
        else if heap.size == k && heap.head < currentSum then
          heap.dequeue()
          heap.enqueue(currentSum)
        bfs(nodesBelow, heap)
    bfs(ArrayBuffer(root), PriorityQueue[Long]()(Ordering[Long].reverse))

object KthLargestSumInBinaryTreeApp extends App:

  {
    val tree = TreeNode(
      5,
      TreeNode(
        8,
        TreeNode(
          2,
          TreeNode(4),
          TreeNode(6),
        ),
        TreeNode(1),
      ),
      TreeNode(
        9,
        TreeNode(3),
        TreeNode(7),
      )
    )
    val res = KthLargestSumInBinaryTree.kthLargestLevelSum(tree, 2)
    assert(res == 13)
  }
  {
    val tree = TreeNode(
      1,
      TreeNode(
        2,
        TreeNode(3),
      ),
    )
    val res = KthLargestSumInBinaryTree.kthLargestLevelSum(tree, 1)
    assert(res == 3)
  }
