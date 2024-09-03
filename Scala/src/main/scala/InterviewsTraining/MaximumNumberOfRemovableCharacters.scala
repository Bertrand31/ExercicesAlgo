import scala.annotation.tailrec

object MaximumNumberOfRemovableCharacters extends App {

  private def getIndices(str: String, subStr: String): Map[Char, IndexedSeq[Int]] =
    val subStrSet = subStr.toSet
    str
      .zipWithIndex
      .filter(tpl => subStrSet.contains(tpl._1))
      .groupMap(_._1)(_._2)

  @tailrec
  private def getRuns(
    remaining: List[Char],
    indexes: Map[Char, IndexedSeq[Int]],
    soFar: List[List[Int]] = List.empty,
  ): List[List[Int]] =
    if remaining.isEmpty then soFar
    else
      val head :: tail = remaining: @unchecked
      val currIndexes = indexes(head)
      val newSoFar = soFar.flatMap(run => currIndexes.flatMap(i =>
        if i > run.head then Some(i :: run)
        else None
      ))
      getRuns(tail, indexes, newSoFar)
      

  @tailrec
  private def getMaximumRemovals(removals: Vector[Int], runs: List[Set[Int]]): Int =
    if runs.exists(run => !removals.exists(run.contains)) then
      removals.size
    else
      getMaximumRemovals(removals.init, runs)


  def maximumRemovals(s: String, p: String, removable: Array[Int]): Int =
    val indices = getIndices(s, p)
    val runs = getRuns(p.tail.toList, indices, indices(p.head).map(List(_)).toList)
    getMaximumRemovals(removable.toVector, runs.map(_.toSet))

  assert(maximumRemovals("abcacb", "ab", Array(3, 1, 0)) == 2)
  assert(maximumRemovals("abcbddddd", "abcd", Array(3, 2, 1, 4, 5, 6)) == 1)
  assert(maximumRemovals("abcab", "abc", Array(0, 1, 2, 3, 4)) == 0)
}
