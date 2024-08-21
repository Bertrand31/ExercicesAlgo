import scala.annotation.tailrec

object AllPermutations extends App {

  private def permute(nums: List[Int]): List[List[Int]] =
    if (nums.size == 0) List(List.empty)
    else if (nums.size == 1) List(List(nums.head))
    else {
      val head :: tail = nums: @unchecked
      permute(tail).map(head :: _) ++ (0 until tail.size).flatMap(i => {
        permute(tail.updated(i, head)).map(tail(i) :: _)
      }).toList
    }

  println(permute(List(1, 2, 3)))
}
