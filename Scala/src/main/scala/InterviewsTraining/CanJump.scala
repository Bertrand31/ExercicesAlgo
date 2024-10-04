// https://leetcode.com/problems/jump-game/

object CanJump {

  @annotation.tailrec
  private def hasNeeded(nums: Array[Int], currentIndex: Int, needed: Int): Boolean =
    nums.lift(currentIndex) match
      case None => false
      case Some(value) if value >= needed => true
      case Some(value) => hasNeeded(nums, currentIndex - 1, needed + 1)
    
  private def walkBack(nums: Array[Int], currentIndex: Int): Boolean =
    nums.lift(currentIndex) match
      case None => true
      case Some(0) =>
        walkBack(nums, currentIndex - 1) && hasNeeded(nums, currentIndex - 1, 2)
      case Some(_) =>
        walkBack(nums, currentIndex - 1)
    

  def canJump(nums: Array[Int]): Boolean =
    if nums.isEmpty then false
    else walkBack(nums, nums.size - 2)
}

object CanJumpApp extends App {

  {
    val sample = Array(2, 3, 1, 1, 4)
    val res = CanJump.canJump(sample)
    println(res)
  }
  {
    val sample = Array(3, 2, 1, 0, 4)
    val res = CanJump.canJump(sample)
    println(res)
  }
}
