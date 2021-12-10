package aoc.challenges

import scala.collection.immutable.HashSet

class Day9 extends Challenge {
  val GREATER_THAN_ZERO_CHAR = ('9' + 1).toChar

  override def part1(input: String): String = {
    val lines = input.linesIterator
    val previousLines = (Array("").iterator ++ input.linesIterator)
    val nextLines = input.linesIterator.drop(1)

    val zipped = lines.zipAll(nextLines, "", "")
    val result = zipped
      .zip(previousLines)
      .flatMap { case ((line, nextLine), prevLine) =>
        line.zipWithIndex.filter { case (c, i) =>
          val prevLeft = tryGetChar(line, i - 1)
          val prevTop = tryGetChar(prevLine, i)
          val nextRight = tryGetChar(line, i + 1)
          val nextBot = tryGetChar(nextLine, i)
          c < prevLeft &&
          c < prevTop &&
          c < nextRight &&
          c < nextBot
        }
      }
      .map(_._1.toString.toInt + 1)
      .sum
    result.toString
  }

  private def tryGetChar(s: String, i: Int): Char = {
    if (!s.isEmpty && i >= 0 && i < s.length) {
      s.charAt(i)
    } else {
      GREATER_THAN_ZERO_CHAR
    }
  }

  val COLUMNS = 100

  override def part2(input: String): String = {
    val lines = input.linesIterator.toVector
    val basins = Vector.empty[Int]
    val visited = HashSet.empty[(Int, Int)]

    val resultBasins =
      findEachBasin(lines, 0, lines.size, 0, COLUMNS, visited, basins)
    resultBasins.sorted.reverse.take(3).product.toString
  }

  private def findEachBasin(
      lines: Vector[String],
      i: Int,
      iMax: Int,
      j: Int,
      jMax: Int,
      visited: HashSet[(Int, Int)],
      basins: Vector[Int]
  ): Vector[Int] = {
    if (j == jMax) {
      findEachBasin(lines, i + 1, iMax, 0, jMax, visited, basins)
    } else {
      if (i == iMax) {
        basins
      } else {
        val (newVisited, basinSize) =
          findBasinFrom(lines, i, iMax, j, jMax, visited, 0)
        val newBasins = basins.appended(basinSize)
        findEachBasin(lines, i, iMax, j + 1, jMax, newVisited, newBasins)
      }
    }
  }

  private def findBasinFrom(
      lines: Vector[String],
      i: Int,
      iMax: Int,
      j: Int,
      jMax: Int,
      visited: HashSet[(Int, Int)],
      basinSize: Int
  ): (HashSet[(Int, Int)], Int) = {
    if (visited((i, j))) {
      return (visited, basinSize)
    }

    val nVisited = visited + ((i, j))

    if (i >= iMax || i < 0 || j >= jMax || j < 0) {
      return (visited, basinSize)
    }

    if (lines(i)(j) == '9') {
      return (nVisited, basinSize)
    }

    val nSize = basinSize + 1
    val (tVisited, tSize) =
      findBasinFrom(lines, i - 1, iMax, j, jMax, nVisited, nSize)
    val (bVisited, bSize) =
      findBasinFrom(lines, i + 1, iMax, j, jMax, tVisited, tSize)
    val (lVisited, lSize) =
      findBasinFrom(lines, i, iMax, j - 1, jMax, bVisited, bSize)
    val (rVisited, rSize) =
      findBasinFrom(lines, i, iMax, j + 1, jMax, lVisited, lSize)

    (rVisited, rSize)
  }
}
