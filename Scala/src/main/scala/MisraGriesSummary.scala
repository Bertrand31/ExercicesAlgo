import scala.annotation.tailrec

object MiraGriesSummry {

  @tailrec
  private def getFrequencies(seq: Iterator[Int], maxKeys: Int, soFar: Map[Int, Int]): Map[Int, Int] =
    if (!seq.hasNext) soFar
    else {
      val head = seq.next
      val newMap = soFar.get(head) match {
        case Some(count) => soFar + (head -> (count + 1))
        case None if soFar.size < maxKeys - 1 => soFar + (head -> 1)
        case _ => soFar.collect({ case (k, v) if v > 1 => (k -> (v - 1)) })
      }
      getFrequencies(seq, maxKeys, newMap)
    }

  def getFrequencies(s: IterableOnce[Int], k: Int): Map[Int, Int] =
    getFrequencies(s.iterator, k, Map())
}

object MisraGriesSummaryApp extends App {

  val iterable = (0 to 100).map(_ => scala.util.Random.between(0, 10))
  val frequencies = MiraGriesSummry.getFrequencies(iterable, 2)
  println(frequencies)
}
