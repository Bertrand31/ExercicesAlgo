import scala.annotation.tailrec

object MiraGriesSummary {

  @tailrec
  private def getFrequencies(seq: Iterator[Int], maxKeys: Int, soFar: Map[Int, Int]): Map[Int, Int] =
    if (!seq.hasNext) soFar
    else
      val head = seq.next
      val newMap = soFar.get(head) match
        case Some(count) => soFar.updated(head, count + 1)
        case None if soFar.size < maxKeys - 1 => soFar.updated(head, 1)
        case _ => soFar.collect({ case (k, v) if v > 1 => (k -> (v - 1)) })

      getFrequencies(seq, maxKeys, newMap)

  def getFrequencies(s: IterableOnce[Int], k: Int): Map[Int, Int] =
    getFrequencies(s.iterator, k, Map())
}

object MisraGriesSummaryApp extends App {

  val iterable = (0 to 100).map(_ => scala.util.Random.between(0, 10))
  val frequencies = MiraGriesSummary.getFrequencies(iterable, 2)
  println(iterable.groupBy(identity).view.mapValues(_.size).toSeq.sortBy(_._2))
  println(frequencies)
}
