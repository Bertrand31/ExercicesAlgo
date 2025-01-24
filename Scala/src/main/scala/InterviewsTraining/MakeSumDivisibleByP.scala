object MakeSumDivisibleByP:

  private def findMultiplesUpTo(p: Int, limit: Int, soFar: List[Int]): List[Int] =
    soFar match
      case Nil if p <= limit =>  findMultiplesUpTo(p, limit, p :: Nil)
      case Nil => Nil
      case head :: next =>
        val nextMultiple = head * 2
        if nextMultiple > limit then soFar
        else findMultiplesUpTo(p, limit, nextMultiple :: soFar)

  private def findSubarray(increasingSums: List[Int], decreasingSumsMap: Map[Int, Int], multiples: List[Int]): Int =
    increasingSums.zipWithIndex.map({
      case (sum: Int, idx: Int) =>
        multiples.collect({
          case multiple if decreasingSumsMap.contains(multiple - sum) =>
            math.abs(idx - decreasingSumsMap.get(multiple - sum).get).toInt
        }).min
    }).min

  def minSubarray(nums: Array[Int], p: Int): Int =
    val increasingSums = nums.reverse.foldLeft(List.empty[Int])((acc, item) =>
      (item + acc.headOption.getOrElse(0)) :: acc
    ).reverse
    println(increasingSums)
    val decreasingSums = nums.foldLeft(List.empty[Int])((acc, item) =>
      (item + acc.headOption.getOrElse(0)) :: acc
    )
    val totalSum = decreasingSums.headOption.getOrElse(0)
    val decreasingSumsMap = decreasingSums.zipWithIndex.toMap
    val multiples = findMultiplesUpTo(p, totalSum, Nil)
    findSubarray(increasingSums, decreasingSumsMap, multiples)

object MakeSumDivisibleByPApp extends App:

  {
    val nums = Array(2, 6, 3, 5, 2)
    val res = MakeSumDivisibleByP.minSubarray(nums, 9)
    println(res)
    // assert(res == 1)
  }
  // {
  //   val nums = Array(3, 1, 4, 2)
  //   val res = MakeSumDivisibleByP.minSubarray(nums, 6)
  //   assert(res == 1)
  // }
  // {
  //   val nums = Array(6, 3, 5, 2)
  //   val res = MakeSumDivisibleByP.minSubarray(nums, 9)
  //   assert(res == 2)
  // }
  // {
  //   val nums = Array(1, 2, 3)
  //   val res = MakeSumDivisibleByP.minSubarray(nums, 3)
  //   assert(res == 0)
  // }
  // {
  //   val nums = Array(1, 2, 3)
  //   val res = MakeSumDivisibleByP.minSubarray(nums, 29)
  //   assert(res == -1)
  // }
