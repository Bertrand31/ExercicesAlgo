import scala.annotation.tailrec
import scala.collection.mutable.{HashMap, HashSet}

object MaximumNumberOfRemovableCharacters extends App {

  type Indexes = HashMap[Char, HashSet[Int]]

  private def getIndices(str: String, subStr: String): Indexes =
    val subStrSet = subStr.toSet
    str
      .zipWithIndex
      .filter(tpl => subStrSet.contains(tpl._1))
      .groupMap(_._1)(_._2)
      .view.mapValues(_.to(HashSet)).to(HashMap)

  // @tailrec
  private def runExists(
    str: String,
    i: Int,
    indexes: Indexes,
    forbidden: HashSet[Int],
    lastIndex: Int = -1,
  ): Boolean =
    if str.sizeIs <= i then true
    else
      val char = str(i)
      val allowedIndexes = indexes(char).subtractAll(forbidden).filter(_ > lastIndex)
      allowedIndexes.exists(runExists(str, i + 1, indexes, forbidden, _))

  def maximumRemovals(s: String, p: String, removable: Array[Int]): Int =
    val indices = getIndices(s, p)
    (1 to removable.size).findLast(size =>
      runExists(p, 0, indices, removable.take(size).to(HashSet))
    ).getOrElse(0)

  // println(maximumRemovals("abcacb", "ab", Array(3, 1, 0)))
  assert(maximumRemovals("abcacb", "ab", Array(3, 1, 0)) == 2)
  assert(maximumRemovals("abcbddddd", "abcd", Array(3, 2, 1, 4, 5, 6)) == 1)
  assert(maximumRemovals("abcab", "abc", Array(0, 1, 2, 3, 4)) == 0)
}
