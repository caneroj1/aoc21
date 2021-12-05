package aoc.challenges

class Day4 extends Challenge {
  val BOARD_DIMENSION = 5
  case class BoardAndPosition(board: Int, position: (Int, Int))
  type PositionMap = Map[String, Iterable[BoardAndPosition]]

  case class Board(
      var rowMarks: Array[Int],
      var colMarks: Array[Int],
      var data: Array[Array[Option[String]]],
      var isFinished: Boolean
  ) {
    def getUnmarkedSum() = data.flatMap(_.flatMap(_.flatMap(_.toIntOption))).sum
  }

  object Board {
    def of(data: Array[Array[Option[String]]]) =
      Board(
        Array.fill(BOARD_DIMENSION)(0),
        Array.fill(BOARD_DIMENSION)(0),
        data,
        false
      )
  }

  override def part1(input: String): String = {
    val results = solveAllBoards(input)
    results.head
  }

  override def part2(input: String): String = {
    val results = solveAllBoards(input)
    results.last
  }

  private def parseBoards(
      lines: Iterator[String]
  ): (Seq[Board], PositionMap) = {
    val acc = (
      Seq.empty[Board],
      Map.empty[String, Iterable[BoardAndPosition]]
    )

    lines.filter(!_.isEmpty).grouped(BOARD_DIMENSION).foldLeft(acc) {
      case (x @ (boards, oldPositionMap), ls) =>
        val (map, board) = createBoard(boards.length, ls)
        val newPositionMap =
          oldPositionMap.toSeq.concat(map.toSeq).groupMapReduce(_._1)(_._2) {
            case (l, r) => l ++ r
          }
        (boards.appended(board), newPositionMap)
    }
  }

  private def createBoard(
      boardNumber: Int,
      lines: Iterable[String]
  ): (PositionMap, Board) = {
    val positionMap = lines.zipWithIndex
      .flatMap { case (line, row) =>
        line.split(" ").filter(!_.isEmpty).zipWithIndex.map {
          case (number, col) =>
            (number, BoardAndPosition(boardNumber, (row, col)))
        }
      }
      .groupMap(_._1)(_._2)
    val board =
      lines.map(_.split(" ").filter(!_.isEmpty).map(Option.apply)).toArray

    (positionMap, Board.of(board))
  }

  private def solveAllBoards(input: String): Vector[String] = {
    val iter = input.linesIterator
    val numbers = iter.next.split(",")
    val resultBuilder = Vector.newBuilder[String]
    val (boards, positionMap) = parseBoards(iter)
    for (number <- numbers) {
      positionMap.get(number) match {
        case Some(positions) =>
          positions.foreach { boardAndPosition =>
            val row = boardAndPosition.position._1
            val col = boardAndPosition.position._2
            var board = boards(boardAndPosition.board)
            if (!board.isFinished) {
              board.rowMarks(row) += 1
              board.colMarks(col) += 1
              board.data(row)(col) = None
              if (
                board.rowMarks(row) == BOARD_DIMENSION
                || board.colMarks(col) == BOARD_DIMENSION
              ) {
                val sum = board.getUnmarkedSum
                val result = number.toInt * sum
                board.isFinished = true
                resultBuilder.addOne(result.toString)
              }
            }
          }
        case None => throw new Error("Found input number not on any board")
      }
    }

    resultBuilder.result
  }
}
