// https://leetcode.com/problems/height-of-binary-tree-after-subtree-removal-queries/

import scala.collection.mutable.{HashSet, PriorityQueue}

object HeightOfBtreeAfterRemovalQueries:

  private def buildHeightsHeap(
    toVisit: List[TreeNode],
    currentLevel: Int,
    set: HashSet[Int],
    heap: PriorityQueue[(Int, Int)],
  ): (HashSet[Int], PriorityQueue[(Int, Int)]) =
    if toVisit.isEmpty then (set, heap)
    else
      val newToVisit = toVisit.flatMap(tree =>
        if tree == null then List()
        else
          set += tree.value
          heap += (tree.value -> currentLevel)
          List(tree.left, tree.right)
      )
      buildHeightsHeap(newToVisit, currentLevel + 1, set, heap)

  /* for each query, take number out of set,
   * and pop top element of heap if is the number from the query OR if it's missing from the set
   * return remaining height at the top of the heap
   */
  private def applyQuery(query: Int, set: HashSet[Int], heap: PriorityQueue[(Int, Int)]): Int =
    set -= query
    heap.dropWhile(tpl => tpl._1 == query || !set.contains(tpl._1))
    heap.head._2

  def treeQueries(root: TreeNode, queries: Array[Int]): Array[Int] =
    val baseHeap = PriorityQueue[(Int, Int)]()(Ordering.by[(Int, Int), Int](_._1))
    val (set, heap) = buildHeightsHeap(List(root), 1, HashSet.empty, baseHeap)
    println(set)
    println(heap)
    queries.map(applyQuery(_, set, heap))
        

object HeightOfBtreeAfterRemovalQueriesApp extends App:
  {
    val tree = TreeNode(
      1,
      TreeNode(
        3,
        TreeNode(2),
      ),
      TreeNode(
        4,
        TreeNode(6),
        TreeNode(
          5,
          null,
          TreeNode(7),
        ),
      )
    )
    val queries = Array(4)
    val res = HeightOfBtreeAfterRemovalQueries.treeQueries(tree, queries)
    val expected = List(2)
    println(res.toList)
    // assert(res.toList == expected)
  }
