import scala.annotation.tailrec

object Merge_Sort {

  @tailrec
  private def merge[A](
    predicate: (A, A) => Boolean,
    left: IndexedSeq[A],
    right: IndexedSeq[A],
    soFar: IndexedSeq[A] = IndexedSeq(),
  ): IndexedSeq[A] =
    if left.isEmpty then
      soFar ++ right
    else if right.isEmpty then
      soFar ++ left
    else
      val headLeft +: tailLeft = left: @unchecked
      val headRight +: tailRight = right: @unchecked
      if predicate(headLeft, headRight) then
        merge(predicate, tailLeft, right, soFar :+ headLeft)
      else
        merge(predicate, left, tailRight, soFar :+ headRight)

  def sort[A](seq: IndexedSeq[A])(using ord: Ordering[A]): IndexedSeq[A] =
    if (seq.length < 2) seq
    else
      val pivot = seq.length / 2
      val (left, right) = seq.splitAt(pivot)
      merge(ord.lt, sort(left), sort(right))
}

object Merge_Sort_Tests extends App {

  {
    val seq = IndexedSeq(2, 4, 9, 1, 3, 2, 7, 0)
    val sorted = Merge_Sort.sort[Int](seq)
    assert(sorted == Seq(0, 1, 2, 2, 3, 4, 7, 9))
  }

  {
    given Ordering[(Long, Long)] = Ordering.by[(Long, Long), Long](_._1)
    val seq = IndexedSeq((1L, 2L), (0L, 2L), (-4L, 9L), (9L, 0L))
    val sorted = Merge_Sort.sort[(Long, Long)](seq)
    assert(sorted == IndexedSeq((-4,9), (0,2), (1,2), (9,0)))
  }
}
