import scala.annotation.tailrec

object CountingSort {

  private def createArray(arr: IndexedSeq[Int], min: Int): IndexedSeq[Int] = {
    val delta = arr.max - min
    val emptyArray = IndexedSeq.fill(delta + 1)(0)
    arr.foldLeft(emptyArray)((acc, item) => {
      val index = item - min
      acc.updated(index, acc(index) + 1)
    })
  }

  @tailrec
  private def recoverContinuousArray(arr: Seq[Int], min: Int, soFar: Seq[Int] = Seq(), index: Int = 0): Seq[Int] = {
    if (arr.isEmpty) soFar
    else {
      val head +: tail = arr
      val newContinuousArr =
        if (head == 0) soFar
        else soFar ++ Seq.fill(head)(index + min)
      recoverContinuousArray(tail, min, newContinuousArr, index + 1)
    }
  }

  def sort(arr: IndexedSeq[Int]): Seq[Int] = {
    val min = arr.min
    val arrayWithHoles = createArray(arr, min)
    recoverContinuousArray(arrayWithHoles, min)
  }
}

object CountingSortApp extends App {

  val sample = IndexedSeq(5, 4, 1, 4, 9, 0)
  assert(CountingSort.sort(sample) == sample.sorted)
}
