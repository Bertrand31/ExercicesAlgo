// https://leetcode.com/problems/best-time-to-buy-and-sell-stock-ii

object Candy:

  def candy(ratings: Array[Int]): Int =

    def walkThrough(idx: Int, leftCandies: Int, soFar: Int): Int =
      println(s"idx: $idx, leftCandies: $leftCandies, soFar: $soFar")
      if idx == ratings.size - 1 then // last item
        if ratings(idx - 1) < ratings(idx) then soFar + leftCandies + 1
        else if ratings(idx - 1) == ratings(idx) then soFar + leftCandies
        else soFar + 1
      else if idx == 0 then // first item
        if ratings(idx + 1) > ratings(idx) then
          walkThrough(idx + 1, 1, soFar + 1)
        else if ratings(idx + 1) == ratings(idx) then ???
        else
          val kek =
            (idx until ratings.size).takeWhile(i =>
              ratings(i) > ratings(i + 1)
            ).size
          val rangeUntilNextAscending = (idx to kek).toList
          println(rangeUntilNextAscending)
          walkThrough(idx + rangeUntilNextAscending.size, 1, soFar + rangeUntilNextAscending.map(_ + 1).sum)
      else // item in the middle
        val localRating = ratings(idx)
        if localRating < ratings(idx + 1) && localRating < ratings(idx - 1) then
          walkThrough(idx + 1, 1, soFar + 1)
        else if localRating == leftCandies then // potential problem here
          walkThrough(idx + 1, leftCandies, soFar + leftCandies)
        else if localRating < ratings(idx - 1) && localRating < ratings(idx + 1) then
          val kek =
            (idx until ratings.size).takeWhile(i =>
              ratings(i) > ratings(i + 1)
            ).size
          val rangeUntilNextAscending = (idx to kek).toList
          val fixedRange =
            math.max(leftCandies, rangeUntilNextAscending.head) :: rangeUntilNextAscending.tail
          walkThrough(idx + fixedRange.size, 1, soFar + fixedRange.sum)
        else
          ???

    walkThrough(0, 0, 0)

object CandyApp extends App:

  // {
  //   val ratings = Array(1, 0, 2)
  //   val res = Candy.candy(ratings)
  //   assert(res == 5)
  // }
  {
    val ratings = Array(1, 2, 2)
    val res = Candy.candy(ratings)
    println(res)
    // assert(res == 4)
  }
