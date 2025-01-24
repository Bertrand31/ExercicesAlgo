// https://leetcode.com/problems/the-number-of-the-smallest-unoccupied-chair/
import scala.collection.mutable.HashSet
import scala.collection.mutable.PriorityQueue

object NumberOfSmallestUnoccupiedChair:

  def smallestChair(times: Array[Array[Int]],  targetFriend: Int): Int =
    val Array(targetArrival, _) = times(targetFriend)
    val sortedByArrival = times.sortBy(_.head)
    val minHeap = PriorityQueue()(Ordering[(Int, Int)].reverse)

    def traverse(current: Int): Int =
      if current == targetFriend then
        minHeap.size
      else
        val Array(arrival, departure) = times(current)
        if minHeap.nonEmpty && minHeap.head._1 < arrival then
          minHeap.dequeue()
        minHeap.enqueue((departure, current))
        traverse(current + 1)

    traverse(0)

object NumberOfSmallestUnoccupiedChairApp extends App:

  {
    val times = Array(
      Array(1, 4),
      Array(2, 3),
      Array(4, 6),
    )
    val res = NumberOfSmallestUnoccupiedChair.smallestChair(times,  1)
    println(res)
  }

  {
    val times = Array(
      Array(33889, 98676), // 1
      Array(80071, 89737), // 1
      Array(44118, 52565), // 0
      Array(52992, 84310), // 0
      Array(78492, 88209), // 1
      Array(21695, 67063), // 0
      Array(84622, 95452), // target
      Array(98048, 98856),
      Array(98411, 99433),
      Array(55333, 56548),
      Array(65375, 88566),
      Array(55011, 62821),
      Array(48548, 48656),
      Array(87396, 94825),
      Array(55273, 81868),
      Array(75629, 91467),
    )
    val res = NumberOfSmallestUnoccupiedChair.smallestChair(times,  6)
    println(res) // should be 2
  }

