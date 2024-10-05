// https://leetcode.com/problems/permutation-in-string
import scala.collection.mutable.HashMap

object PermutationInString {

  // faster, ad hoc version of `.groupMapReduce(identity)(_ => 1)(_ + _)`
  private def makeMap(s: String): HashMap[Char, Int] =
    val map = new HashMap[Char, Int]()
    s.foreach(char =>
      val oldValue = map.getOrElse(char, 0)
      map.update(char, oldValue + 1)
    )
    map

  @annotation.tailrec
  private def slideThrough(
    targetMap: HashMap[Char, Int],
    currentMap: HashMap[Char, Int],
    s2: String,
    leftBound: Int,
    rightBound: Int,
  ): Boolean =
    if targetMap == currentMap then true
    else if rightBound >= s2.size - 1 then false
    else
      val leftBoundChar = s2(leftBound)
      currentMap.get(leftBoundChar) match
        case None => throw new Error("Cannot happen")
        case Some(1) => currentMap.remove(leftBoundChar)
        case Some(n) => currentMap.update(leftBoundChar, n - 1)
      val newRightBound = rightBound + 1
      val newRightBoundChar = s2(newRightBound)
      val oldValueForNewRightBound = currentMap.getOrElse(newRightBoundChar, 0)
      currentMap.update(newRightBoundChar, oldValueForNewRightBound + 1)
      slideThrough(targetMap, currentMap, s2, leftBound + 1, newRightBound) 
      
  def checkInclusion(s1: String, s2: String): Boolean =
    val s1Map = makeMap(s1)
    val (firstChunk, rest) = s2.splitAt(s1.size)
    val s2BaseMap = makeMap(firstChunk)
    slideThrough(s1Map, s2BaseMap, s2, 0, s1.size - 1)
}

object PermutationInStringApp extends App {

  {
    val s1 = "ab"
    val s2 = "eidbaooo"
    val res = PermutationInString.checkInclusion(s1, s2)
    println(res)
  }
  {
    val s1 = "ab"
    val s2 = "eidboaoo"
    val res = PermutationInString.checkInclusion(s1, s2)
    println(res)
  }
}
