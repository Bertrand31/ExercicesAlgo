/** You are choreographing a circus show with various animals. For one act, you are given two
  * kangaroos on a number line ready to jump in the positive direction (i.e, toward positive
  * infinity). The first kangaroo starts at location x1 and moves at a rate of v1 meters per jump.
  * The second kangaroo starts at location x2 and moves at a rate of v2 meters per jump.
  * You have to figure out a way to get both kangaroos at the same location at the same time as
  * part of the show. If it is possible, return YES, otherwise return NO.
  * A maximum of 10000 hops is permitted.
  * https://www.hackerrank.com/challenges/kangaroo/problem
  */

object KangarooCircus {

  def willKangaroosMeet(x1: Int, v1: Int, x2: Int, v2: Int): Boolean =
    if ((x1 > x2 && v1 >= v2) || (x2 > x1 && v2 >= v1)) false
    else (0 to 10000).exists(hops => hops * v1 + x1 == hops * v2 + x2)
}

object KangarooCircusApp extends App {

  assert(KangarooCircus.willKangaroosMeet(0, 3, 4, 2) == true)
  assert(KangarooCircus.willKangaroosMeet(0, 2, 5, 3) == false)
}
