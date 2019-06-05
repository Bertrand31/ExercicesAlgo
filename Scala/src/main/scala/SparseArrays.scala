// https://www.hackerrank.com/challenges/sparse-arrays/problem

// import scala.annotation.tailrec

object SparseArrays extends App {

  def matchingStrings(strings: Array[String], queries: Array[String]): Array[Int] =
    queries.map(q => strings.count(_ == q))


  val strs = Array("abcde", "sdaklfj", "asdjf", "na", "basdn", "sdaklfj", "asdjf", "na", "asdjf", "na", "basdn", "sdaklfj", "asdjf")
  println(matchingStrings(strs, Array("abcde", "sdaklfj", "asdjf", "na", "basdn")).toList)

}
