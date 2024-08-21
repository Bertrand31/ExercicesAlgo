import scala.annotation.tailrec

object MergeSort {

  @tailrec
  private def merge[A](
    predicate: (A, A) => Boolean,
    left: IndexedSeq[A], right: IndexedSeq[A],
    soFar: IndexedSeq[A] = IndexedSeq(),
  ): IndexedSeq[A] =
    if (left.isEmpty) soFar ++ right
    else if (right.isEmpty) soFar ++ left
    else
      val headLeft +: tailLeft = left: @unchecked
      val headRight +: tailRight = right: @unchecked
      if (predicate(headLeft, headRight)) merge(predicate, tailLeft, right, soFar :+ headLeft)
      else merge(predicate, left, tailRight, soFar :+ headRight)

  def sort[A](seq: IndexedSeq[A])(implicit comparisonPredicate: (A, A) => Boolean): IndexedSeq[A] =
    if (seq.length < 2) seq
    else
      val pivot = seq.length / 2
      merge(
        comparisonPredicate,
        sort(seq take pivot),
        sort(seq drop pivot),
      )
}

object MergeSortTests extends App {

  {
    val seq = IndexedSeq(2, 4, 9, 1, 3, 2, 7, 0)
    implicit val predicate = (a: Int, b: Int) => a < b
    val sorted = MergeSort.sort[Int](seq)
    assert(sorted == Seq(0, 1, 2, 2, 3, 4, 7, 9))
  }

  {
    val seq = IndexedSeq((1L, 2L), (0L, 2L), (-4L, 9L), (9L, 0L))
    implicit val predicate = (t1: (Long, Long), t2: (Long, Long)) => t1._1 < t2._1
    val sorted = MergeSort.sort[(Long, Long)](seq)
    assert(sorted == IndexedSeq((-4,9), (0,2), (1,2), (9,0)))
  }
}
