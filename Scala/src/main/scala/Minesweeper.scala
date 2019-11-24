
object Minesweeper extends App {

	type Board = Array[String]
  type Coordinates = (Int, Int)

  private def getCell(board: Board, coordinates: Coordinates): Char =
    board(coordinates._1).toList(coordinates._2)

  private val Mine = '*'

  private def isMine(board: Board): Coordinates => Boolean =
		_ match {
      case (x, y) if x < 0 || x >= board.length || y < 0 || y >= board.head.length => false
			case coordinates => getCell(board, coordinates) == Mine
		}

  private val CoordinatesFns: List[(Function[Int, Int], Function[Int, Int])] = List(
    (_ - 1, _ - 1),
    (_ - 1, identity),
    (_ - 1, _ + 1),
    (identity, _ + 1),
    (_ + 1, _ + 1),
    (_ + 1, identity),
    (_ + 1, _ - 1),
    (identity, _ - 1),
  )

  private def getSurroundingCoordinates(coordinates: Coordinates): List[Coordinates] =
    CoordinatesFns.map(fnTuple => (fnTuple._1(coordinates._1), fnTuple._2(coordinates._2)))

  private def handleCell(board: Board, coordinates: Coordinates): Char =
    getCell(board, coordinates) match {
      case Mine => Mine
      case _ =>
        getSurroundingCoordinates(coordinates)
          .map(isMine(board))
          .map(if (_) 1 else 0)
          .sum
          .toString
          .toList(0)
    }

  private def walkThroughRow(board: Board, currentRow: Int, currentChar: Int): List[Char] = {
    val updatedCell = handleCell(board, (currentRow, currentChar))
    if (board(currentRow).length - 1 == currentChar) List(updatedCell)
    else updatedCell +: walkThroughRow(board, currentRow, currentChar + 1)
  }

  private def processRow(board: Board, currentRow: Int): Board =
    currentRow match {
      case x if x == board.length - 1 => Array(walkThroughRow(board, x, 0).mkString)
      case x => walkThroughRow(board, currentRow, 0).mkString +: processRow(board, x + 1)
    }

  def processBoard(board: Board): Board = {
    assert(!board.isEmpty, "The board must be at least one row high")
    val Array(head, tail@_*)= board.map(_.toList)
    assert(head.length > 0, "The board must be at least one column wide")
    assert(tail.forall(_.length == head.length), "All columns must have the same breadth")
    processRow(board, 0)
  }

	def showBoard(board: Board): Unit = board foreach println

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

  showBoard(processBoard(input))
}
