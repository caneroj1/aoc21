package aoc.challenges

import scala.collection.immutable.HashMap
import scala.annotation.tailrec

class Day5 extends Challenge {
  type PositionMap = HashMap[(Int, Int), Int]
  @tailrec
  private def updateMap(
      currentCol: Int,
      currentRow: Int,
      minCol: Int,
      maxCol: Int,
      maxRow: Int,
      incrementDiag: Boolean,
      map: PositionMap
  ): PositionMap = {
    if (currentRow == maxRow) {
      map
    } else if (currentCol == maxCol) {
      updateMap(minCol, currentRow + 1, minCol, maxCol, maxRow, incrementDiag, map)
    } else {
      val pair = (currentCol, currentRow)
      val newMap = map.updatedWith(pair) { opt =>
        opt match {
          case Some(i) => Some(i + 1)
          case None    => Some(1)
        }
      }
      if (incrementDiag) {
        if (currentRow > maxRow) {
          updateMap(currentCol + 1, currentRow - 1, minCol, maxCol, maxRow, incrementDiag, newMap)
        } else {
          updateMap(currentCol + 1, currentRow + 1, minCol, maxCol, maxRow, incrementDiag, newMap)
        }
      } else {
        updateMap(currentCol + 1, currentRow, minCol, maxCol, maxRow, incrementDiag, newMap)
      }
    }
  }

  override def part1(input: String): String = {
    val vents =
      input.linesIterator.map(parseLine).filter(!isDiagonal(_)).toVector

    val counts = vents.foldLeft(HashMap.empty[(Int, Int), Int]) { case (map, line) =>
      updateMap(line._1._1, line._1._2, line._1._1, line._2._1 + 1, line._2._2 + 1, false, map)
    }
    counts.values.filter(_ > 1).toVector.size.toString
  }

  override def part2(input: String): String = {
    val (diagonals, nonDiags) =
      input.linesIterator.map(parseLine).partition(isDiagonal(_))

    val counts = nonDiags.foldLeft(HashMap.empty[(Int, Int), Int]) { case (map, line) =>
      updateMap(line._1._1, line._1._2, line._1._1, line._2._1 + 1, line._2._2 + 1, false, map)
    }
    val updatedCounts = diagonals.foldLeft(counts) { case (map, line) =>
      val modifier = if (line._1._2 > line._2._2) -1 else 1
      updateMap(line._1._1, line._1._2, line._1._1, line._2._1 + 1, line._2._2 + modifier, true, map)
    }

    updatedCounts.values.filter(_ > 1).toVector.size.toString
  }

  private def parseLine(line: String): ((Int, Int), (Int, Int)) = {
    line match {
      case s"$col1,$row1 -> $col2,$row2" => {
        Array((col1.toInt, row1.toInt), (col2.toInt, row2.toInt)).sorted match {
          case Array(p1, p2) => (p1, p2)
        }
      }
    }
  }

  private def isDiagonal(
      vent: ((Int, Int), (Int, Int))
  ): Boolean = {
    vent match {
      case ((col1, row1), (col2, row2)) => row1 != row2 && col1 != col2
    }
  }
}
