import scala.annotation.tailrec

object LatticeMultiplication extends App {

  private val SingleDigitsMultiplications: Map[(Int, Int), Int] =
    (0 to 9).flatMap(i =>
      (0 to 9).map(j => ((i, j), i * j))
    ).toMap

  private def getDigits(n: Int): List[Int] =
    def getDigitsInternal(n: Int, digits: List[Int]): List[Int] =
      if n < 10 then n :: digits
      else getDigitsInternal(n / 10, (n % 10) :: digits)
    getDigitsInternal(n, Nil)

  private def pairsAddingUpTo(n: Int): List[(Int, Int)] =
    (0 to n).map(i => (i, n - i)).toList

  private def fibonacciMultiply(a: Int, b: Int): Int =
    val x = getDigits(a).reverse
    val y = getDigits(b).reverse
    val steps = x.size + y.size
    val z = new Array[Int](steps)
    var hold = 0
    for (k <- (0 to (steps - 1))) {
      pairsAddingUpTo(k).foreach({
        case (i, j) if i < x.size && j < y.size =>
          hold += SingleDigitsMultiplications((x(i), y(j)))
        case _ =>
      })
      z.update(k, hold % 10)
      hold /= 10
    }
    z.zipWithIndex.map({
      case (digit, i) => digit * math.pow(10, i).toInt
    }).sum

  assert(fibonacciMultiply(8, 74) == 592)
  assert(fibonacciMultiply(28, 14) == 392)
  assert(fibonacciMultiply(934, 314) == 293276)
}

object PeasantMultiplication extends App {

  @tailrec
  def peasantMultiply(x: Int, y: Int, sum: Int = 0): Int =
    if x <= 0 then sum
    else if x % 2 == 1 then peasantMultiply(x / 2, y + y, sum + y)
    else peasantMultiply(x / 2, y + y, sum)

  assert(peasantMultiply(8, 74) == 592)
  assert(peasantMultiply(28, 14) == 392)
  assert(peasantMultiply(934, 314) == 293276)
}
