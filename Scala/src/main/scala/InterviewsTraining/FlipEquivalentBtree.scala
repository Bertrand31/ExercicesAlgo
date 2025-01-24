// https://leetcode.com/problems/flip-equivalent-binary-trees/

import scala.collection.mutable.Queue
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */

object FlipEquivalentBtree:

  import Ordering.Implicits.*

  private def descend(
    leftLevel: ArrayBuffer[TreeNode],
    rightLevel: ArrayBuffer[TreeNode],
  ): Boolean =
    if leftLevel.isEmpty && rightLevel.isEmpty then true
    else if leftLevel.isEmpty && rightLevel.nonEmpty then false
    else if leftLevel.nonEmpty && rightLevel.isEmpty then false
    else
      val leftValues = new ArrayBuffer[Int](leftLevel.length)
      val nextLeftLevel = new ArrayBuffer[TreeNode](leftLevel.length * 2)
      leftLevel.foreach(t =>
        leftValues += (if t == null then -1 else t.value)
        if t != null then
          nextLeftLevel.append(t.left)
          nextLeftLevel.append(t.right)
      )
      val leftPairs = leftValues.sliding(2, 2).map(_.sorted).toList

      val rightValues = new ArrayBuffer[Int](rightLevel.length)
      val nextRightLevel = new ArrayBuffer[TreeNode](rightLevel.length * 2)
      rightLevel.foreach(t =>
        rightValues += (if t == null then -1 else t.value)
        if t != null then
          nextRightLevel.append(t.left)
          nextRightLevel.append(t.right)
      )
      val rightPairs = rightValues.sliding(2, 2).map(_.sorted).toList
      println(leftPairs)
      println(rightPairs)
      println("==========")
      if leftPairs != rightPairs then false
      else descend(nextLeftLevel, nextRightLevel)

  def flipEquiv(root1: TreeNode, root2: TreeNode): Boolean =
    descend(ArrayBuffer(root1), ArrayBuffer(root2))

object FlipEquivalentBtreeApp extends App:

  {
    val tree = TreeNode(
      1,
      TreeNode(
        2,
        TreeNode(4),
        TreeNode(
          5,
          TreeNode(7),
          TreeNode(8),
        ),
      ),
      TreeNode(3, TreeNode(6))
    )
    val tree2 = TreeNode(
      1,
      TreeNode(
        3,
        TreeNode(6)
      ),
      TreeNode(
        2,
        TreeNode(4),
        TreeNode(
          5,
          TreeNode(8),
          TreeNode(7),
        ),
      )
    )
    val res = FlipEquivalentBtree.flipEquiv(tree, tree2)
    assert(res)
  }
  {
    val tree = TreeNode(
      0,
      TreeNode(3),
      TreeNode(
        1,
        null,
        TreeNode(2),
      )
    )
    val tree2 = TreeNode(
      0,
      TreeNode(
        3,
        TreeNode(2),
      ),
      TreeNode(1)
    )
    val res = FlipEquivalentBtree.flipEquiv(tree, tree2)
    assert(!res)
  }
