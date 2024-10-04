// https://leetcode.com/problems/divide-players-into-teams-of-equal-skill

object DividePlayers {

  def dividePlayers(skill: Array[Int]): Long =
    if skill.isEmpty || skill.size % 2 != 0 then -1
    else
      val targetSum = skill.min + skill.max
      val targetSumHalf = targetSum / 2
      if targetSum % 2 == 0 then
        val halves = skill.count(_ == targetSumHalf)
        val base = (targetSumHalf * targetSumHalf) * (halves / 2).toLong
        val (below, above) = skill.filter(_ != targetSumHalf).partition(_ > targetSumHalf)
        if below.size != above.size then -1
        else
          val belowMap = below.groupMapReduce(identity)(_ => 1)(_ + _)
          val aboveMap = above.groupMapReduce(identity)(_ => 1)(_ + _)
          if !belowMap.forall(tpl => aboveMap.get(targetSum - tpl._1).fold(false)(_ == tpl._2)) then
            -1
          else
            base + below.foldLeft(0l)((acc, s) => acc + s.toLong * (targetSum - s).toLong)
      else
        val (below, above) = skill.partition(_ > targetSumHalf)
        if below.size != above.size then -1
        else
          val belowMap = below.groupMapReduce(identity)(_ => 1)(_ + _)
          val aboveMap = above.groupMapReduce(identity)(_ => 1)(_ + _)
          if !belowMap.forall(tpl => aboveMap.get(targetSum - tpl._1).fold(false)(_ == tpl._2)) then
            -1
          else
            below.foldLeft(0l)((acc, s) => acc + s.toLong * (targetSum - s).toLong)
}

object DividePlayersApp extends App {

  {
    val sample = Array(3, 2, 5, 1, 3, 4)
    val res = DividePlayers.dividePlayers(sample)
    assert(res == 22)
  }
  {
    val sample = Array(3, 4)
    val res = DividePlayers.dividePlayers(sample)
    assert(res == 12)
  }
  {
    val sample = Array(1, 1, 2, 3)
    val res = DividePlayers.dividePlayers(sample)
    assert(res == -1)
  }
  {
    val sample = Array.fill(50000)(1000)
    val res = DividePlayers.dividePlayers(sample)
    println(res)
    assert(res == 50000000000l)
  }
}
