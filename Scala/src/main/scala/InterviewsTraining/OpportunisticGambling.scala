// A gambler can only either add one chip of go all in. If he wins, he gets back twice what he bet.
// Yesterday, he won `n` chips, by going all in `k` times. What is the minimum number of steps this
// gambler could possibly have played to attain this number of chips, starting with only 1 chip?

import scala.annotation.tailrec

object OpportunisticGambling {

  @tailrec
  private def countStepsDown(chips: Int, remainingAllIn: Int, steps: Int = 0): Int =
    if (chips == 1)
      steps
    else if (remainingAllIn == 0)
      steps + chips - 1
    else if (chips % 2 == 0)
      countStepsDown(chips / 2, remainingAllIn - 1, steps + 1)
    else
      countStepsDown(chips - 1, remainingAllIn, steps +  1)

  def getNumberOfSteps(n: Int, k: Int): Int = countStepsDown(n, k)
}

object OpportunisticGamblingApp extends App {

  assert(OpportunisticGambling.getNumberOfSteps(8, 0) == 7)
  assert(OpportunisticGambling.getNumberOfSteps(19, 2) == 7)
  assert(OpportunisticGambling.getNumberOfSteps(10, 10) == 4)
}

