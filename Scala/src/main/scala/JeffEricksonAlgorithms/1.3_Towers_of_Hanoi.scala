import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object TowerOfHanoi extends App {

  final case class Disk(width: Int)

  opaque type Pegs = ArrayBuffer[ArrayBuffer[Disk]]

  extension (pegs: Pegs)
    def show(): String =
      s"""
        Left: ${pegs(0).toString}
        Middle: ${pegs(1).toString}
        Right: ${pegs(2).toString}
      """

  private def move(n: Int, pegs: Pegs, from: Int, to: Int, tmp: Int): Pegs =
    if n == 0 then pegs
    else
      move(n - 1, pegs, from, tmp, to)
      val disk = pegs(from).head
      pegs(from) = pegs(from).tail
      pegs(to).prepend(disk)
      move(n - 1, pegs, tmp, to, from)

  val disks = (1 to 5).map(Disk(_)).to(ArrayBuffer)
  val pegs = ArrayBuffer[ArrayBuffer[Disk]](disks, ArrayBuffer.empty, ArrayBuffer.empty)
  val result = move(disks.size, pegs, 0, 2, 1)
  assert(result(0).isEmpty)
  assert(result(1).isEmpty)
  assert(result(2) == disks)
}
