/*

Suppose we have some input data describing a graph of relationships between parents and children over multiple generations. The data is formatted as a list of (parent, child) pairs, where each individual is assigned a unique integer identifier.

For example, in this diagram, the earliest ancestor of 6 is 14, and the earliest ancestor of 15 is 2.

         14
         |
  2      4
  |    / | \
  3   5  8  9
 / \ / \     \
15  6   7    11

Write a function that, for a given individual in our dataset, returns their earliest known ancestor -- the one at the farthest distance from the input individual. If there is more than one ancestor tied for "earliest", return any one of them. If the input individual has no parents, the function should return null (or -1).

Sample input for the above diagram:

parentChildPairs = [
    (2, 3), (3, 15), (3, 6), (5, 6), (5, 7),
    (4, 5), (4, 8), (4, 9), (9, 11), (14, 4),
]

*/

object EarliestAncestor {

  private def makeMap(pairs: List[(Int, Int)], soFar: Map[Int, Set[Int]] = Map()): Map[Int, Set[Int]] =
    if (pairs.isEmpty) soFar
    else {
      val head +: tail = pairs
      val (parent, child) = head
      val currentParents = soFar.getOrElse(child, Set())
      val newMap = soFar + (child -> (currentParents + parent))
      makeMap(tail, newMap)
    }

  private def getHighestAncestors(map: Map[Int, Set[Int]], child: Int, currentSteps: Int = 0): Set[(Int, Int)] =
    map.get(child) match {
      case None => Set((child,  currentSteps))
      case Some(ancestors) => ancestors.flatMap(getHighestAncestors(map, _, currentSteps + 1))
    }

  def findEarliestAncestor(pairs: List[(Int, Int)], nb: Int): Int =
    getHighestAncestors(makeMap(pairs), nb).maxBy(_._2) match {
      case (_, 0) => -1
      case (nb, _) => nb
    }
}

object EarliestAncestorApp extends App {

  val parentChildPairs = List((2, 3), (3, 15), (3, 6), (5, 6), (5, 7), (4, 5), (4, 8), (4, 9), (9, 11), (14, 4))
  assert(findEarliestAncestor(parentChildPairs, 8) === 14)
  assert(findEarliestAncestor(parentChildPairs, 7) === 14)
  assert(findEarliestAncestor(parentChildPairs, 6) === 14)
  assert(findEarliestAncestor(parentChildPairs, 15) === 2)
  assert(findEarliestAncestor(parentChildPairs, 14) === -1)
  assert(findEarliestAncestor(parentChildPairs, 11) === 14)
}
