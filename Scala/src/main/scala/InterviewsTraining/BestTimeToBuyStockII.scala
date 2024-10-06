// https://leetcode.com/problems/best-time-to-buy-and-sell-stock-ii

object BestTimeToBuyStockII:

  def maxProfit(prices: Array[Int]): Int =
    @annotation.tailrec
    def walkThrough(currentDay: Int, currentProfit: Int): Int =
      if prices.sizeIs <= currentDay + 1 then currentProfit
      else
        val currentDayPrice = prices(currentDay)
        val nextDayPrice = prices(currentDay + 1)
        val potentialProfit = nextDayPrice - currentDayPrice
        if potentialProfit > 0 then walkThrough(currentDay + 1, currentProfit + potentialProfit)
        else walkThrough(currentDay + 1, currentProfit)

    walkThrough(0, 0)

object BestTimeToBuyStockIIApp extends App:

  {
    val prices = Array(7, 1, 5, 3, 6, 4)
    val res = BestTimeToBuyStockII.maxProfit(prices)
    assert(res == 7)
  }
  {
    val prices = Array(1, 2, 3, 4, 5)
    val res = BestTimeToBuyStockII.maxProfit(prices)
    assert(res == 4)
  }
  {
    val prices = Array(7, 6, 4, 3, 1)
    val res = BestTimeToBuyStockII.maxProfit(prices)
    assert(res == 0)
  }
