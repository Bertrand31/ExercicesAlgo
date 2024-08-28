import scala.annotation.tailrec

object QuickSort {

  @tailrec
  private def merge[A](
    greaterThanOrEqual: (A, A) => Boolean,
    left: IndexedSeq[A],
    right: IndexedSeq[A],
    soFar: IndexedSeq[A] = IndexedSeq(),
  ): IndexedSeq[A] =
    if left.isEmpty then soFar ++ right
    else if right.isEmpty then soFar ++ left
    else if greaterThanOrEqual(left.head, right.head) then
      merge(greaterThanOrEqual, left, right.tail, soFar :+ right.head)
    else
      merge(greaterThanOrEqual, left.tail, right, soFar :+ left.head)

  def sort[A](seq: IndexedSeq[A])(using ord: Ordering[A]): IndexedSeq[A] =
    if seq.length < 2 then
      seq
    else if seq.length == 2 then
      val head = seq.head
      val last = seq.last
      if ord.lt(head, last) then
        seq
      else
        IndexedSeq[A](last, head)
    else
      val pivot = seq.head
      val (left, right) = seq.tail.partition(ord.gteq(pivot, _))
      merge(ord.gteq, sort(left), pivot +: sort(right))
}

object QuickSortTests extends App {

  {
    val seq = IndexedSeq(2, 4, 9, 1, 3, 2, 7, 0)
    val sorted = QuickSort.sort[Int](seq)
    assert(sorted == seq.sorted)
  }

  {
    given Ordering[(Long, Long)] = Ordering.by[(Long, Long), Long](_._1)
    val seq = IndexedSeq((1L, 2L), (0L, 2L), (-4L, 9L), (9L, 0L))
    val sorted = QuickSort.sort[(Long, Long)](seq)
    assert(sorted == seq.sortBy(_._1))
  }
}
