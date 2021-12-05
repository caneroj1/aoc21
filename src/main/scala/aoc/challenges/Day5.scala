package aoc.challenges

import scala.collection.immutable.HashMap
import scala.annotation.tailrec

class Day5 extends Challenge {
  case class Point(col: Int, row: Int) {
    def pair = (col, row)
  }
  implicit object PointOrdering extends Ordering[Point] {
    def compare(x: Point, y: Point): Int =
      Ordering[(Int, Int)].compare(x.pair, y.pair)
  }

  case class Vent(start: Point, end: Point) {
    def isDiagonal = start.row != end.row && start.col != end.col
  }
  type PositionMap = HashMap[Point, Int]

  @tailrec
  private def updateMap(
      point: Point,
      minCol: Int,
      endPoint: Point,
      incrementDiag: Boolean,
      map: PositionMap
  ): PositionMap = {
    if (point.row == endPoint.row) {
      map
    } else if (point.col == endPoint.col) {
      val nextPoint = Point(minCol, point.row + 1)
      updateMap(
        nextPoint,
        minCol,
        endPoint,
        incrementDiag,
        map
      )
    } else {
      val newMap = map.updatedWith(point) { opt =>
        opt match {
          case Some(i) => Some(i + 1)
          case None    => Some(1)
        }
      }
      if (incrementDiag) {
        val nextPoint =
          if (point.row > endPoint.row) Point(point.col + 1, point.row - 1)
          else Point(point.col + 1, point.row + 1)
        updateMap(nextPoint, minCol, endPoint, incrementDiag, newMap)
      } else {
        val nextPoint = Point(point.col + 1, point.row)
        updateMap(
          nextPoint,
          minCol,
          endPoint,
          incrementDiag,
          newMap
        )
      }
    }
  }

  override def part1(input: String): String = {
    val vents =
      input.linesIterator.map(parseLine).filter(!_.isDiagonal).toVector

    val counts = vents.foldLeft(HashMap.empty[Point, Int]) { case (map, line) =>
      updateMap(
        line.start,
        line.start.col,
        Point(line.end.col + 1, line.end.row + 1),
        false,
        map
      )
    }
    counts.values.filter(_ > 1).toVector.size.toString
  }

  override def part2(input: String): String = {
    val (diagonals, nonDiags) =
      input.linesIterator.map(parseLine).partition(_.isDiagonal)

    val counts = nonDiags.foldLeft(HashMap.empty[Point, Int]) {
      case (map, line) =>
        updateMap(
          line.start,
          line.start.col,
          Point(line.end.col + 1, line.end.row + 1),
          false,
          map
        )
    }
    val updatedCounts = diagonals.foldLeft(counts) { case (map, line) =>
      val modifier = if (line.start.row > line.end.row) -1 else 1
      updateMap(
        line.start,
        line.start.col,
        Point(line.end.col + 1, line.end.row + modifier),
        true,
        map
      )
    }

    updatedCounts.values.filter(_ > 1).toVector.size.toString
  }

  private def parseLine(line: String): Vent = {
    line match {
      case s"$col1,$row1 -> $col2,$row2" => {
        Array(
          Point(col1.toInt, row1.toInt),
          Point(col2.toInt, row2.toInt)
        ).sorted match {
          case Array(p1, p2) => Vent(p1, p2)
        }
      }
    }
  }
}
