import cats.Applicative
import cats.implicits._
import PerfUtils.memoizeUnary

object Minesweeper extends App {

  type Board = Array[String]
  type Coordinates = (Int, Int)

  private def getCell(board: Board, coordinates: Coordinates): Char =
    board(coordinates._1)(coordinates._2)

  private val Mine = '*'

  private def isMine(board: Board): Coordinates => Boolean =
    _ match {
      case (x, y) if x < 0 || x >= board.length || y < 0 || y >= board.head.length => false
      case coordinates => getCell(board, coordinates) === Mine
    }

  private val CoordinatesFnsPairs: List[(Int => Int, Int => Int)] = List(
    (_ - 1,    _ - 1),
    (_ - 1,    identity),
    (_ - 1,    _ + 1),
    (identity, _ + 1),
    (_ + 1,    _ + 1),
    (_ + 1,    identity),
    (_ + 1,    _ - 1),
    (identity, _ - 1),
  )

  private def getSurroundingCoordinates: Coordinates => List[Coordinates] =
    CoordinatesFnsPairs map {
      case (fx, fy) => (xy: Coordinates) => xy.bimap(fx, fy)
    } ap Applicative[List].pure(_)

  // The maximum number of mines that can surround a cell being 8, once converted
  // to a string the number of neighbouring mines can only be 1 character long.
  // Hence why we use `head` to turn the String into a Char.
  private def handleCell(board: Board, coordinates: Coordinates, isMineFn: Coordinates => Boolean): Char =
    if (getCell(board, coordinates) === Mine) Mine
    else
      getSurroundingCoordinates(coordinates)
        .count(isMineFn)
        .toString
        .head

  private def walkThroughRow(board: Board, currentRow: Int, currentChar: Int, isMineFn: Coordinates => Boolean): List[Char] = {
    val updatedCell = handleCell(board, (currentRow, currentChar), isMineFn)
    if (board(currentRow).length - 1 === currentChar) List(updatedCell)
    else updatedCell +: walkThroughRow(board, currentRow, currentChar + 1, isMineFn)
  }

  private def processRow(board: Board, currentRow: Int, isMineFn: Coordinates => Boolean): Board = {
    val processedRow = walkThroughRow(board, currentRow, 0, isMineFn).mkString
    if (currentRow === board.length - 1) Array(processedRow)
    else processedRow +: processRow(board, currentRow + 1, isMineFn)
  }

  def markMines(board: Board): Board = {
    require(!board.isEmpty, "The board must be at least one row high")
    val Array(head, tail@_*) = board.map(_.toList)
    require(head.length > 0, "The board must be at least one column wide")
    require(tail.forall(_.length === head.length), "All columns must have the same breadth")
    val isMineFn = memoizeUnary(isMine(board))
    processRow(board, 0, isMineFn)
  }

  def showBoard: Board => Unit = _ foreach println

  val input: Board = Array(
    "       ",
    "       ",
    "       ",
    "       ",
    "       ",
    " *     ",
    "    *  ",
    "   *   ",
    "      *",
    "***    ",
    "* *    ",
    "       ",
  )

  showBoard(markMines(input))
}
