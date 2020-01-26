object IndexedSeqUtils {

  implicit class IndexedSeqImprovements[A](val seq: IndexedSeq[A]) {

    def updateAt(index: Int, fn: A => A): IndexedSeq[A] =
      seq.updated(index, fn(seq(index)))
  }
}
