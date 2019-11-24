import cats.implicits._

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

  private val CoordinatesFns: List[(Int => Int, Int => Int)] = List(
    (_ - 1, _ - 1),
    (_ - 1, identity),
    (_ - 1, _ + 1),
    (identity, _ + 1),
    (_ + 1, _ + 1),
    (_ + 1, identity),
    (_ + 1, _ - 1),
    (identity, _ - 1),
  )

  private def getSurroundingCoordinates(coordinates: Coordinates): List[Coordinates] = {
    val (row, col) = coordinates
    CoordinatesFns.map(_.bimap(_(row), _(col)))
  }

  private def handleCell(board: Board, coordinates: Coordinates): Char =
    if (getCell(board, coordinates) === Mine) Mine
    else
      getSurroundingCoordinates(coordinates)
        .map(isMine(board))
        .map(if (_) 1 else 0)
        .sum
        .toString
        .head

  private def walkThroughRow(board: Board, currentRow: Int, currentChar: Int): List[Char] = {
    val updatedCell = handleCell(board, (currentRow, currentChar))
    if (board(currentRow).length - 1 === currentChar) List(updatedCell)
    else updatedCell +: walkThroughRow(board, currentRow, currentChar + 1)
  }

  private def processRow(board: Board, currentRow: Int): Board =
    if (currentRow === board.length - 1) Array(walkThroughRow(board, currentRow, 0).mkString)
    else walkThroughRow(board, currentRow, 0).mkString +: processRow(board, currentRow + 1)

  def markMines(board: Board): Board = {
    assert(!board.isEmpty, "The board must be at least one row high")
    val Array(head, tail@_*) = board.map(_.toList)
    assert(head.length > 0, "The board must be at least one column wide")
    assert(tail.forall(_.length === head.length), "All columns must have the same breadth")
    processRow(board, 0)
  }

  def showBoard: Board => Unit = _ foreach println

  val input: Board = Array(
    "       ",
    " *     ",
    "    *  ",
    "   *   ",
    "      *",
    "***    ",
    "* *    ",
    "***    ",
  )

  showBoard(markMines(input))
}
