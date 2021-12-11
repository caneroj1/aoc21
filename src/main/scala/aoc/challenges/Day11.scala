package aoc.challenges

import scala.collection.mutable.HashSet

class Day11 extends Challenge {
  val SIZE = 10
  val FLASH_LIMIT = 9

  override def part1(input: String): String = {
    val STEPS = 100
    val map = input.linesIterator.map(_.map(_.toString.toInt).toArray).toArray
    Range(0, STEPS).scan(0) { case (acc, _) => step(map) }.sum.toString
  }

  override def part2(input: String): String = {
    val ALL_FLASHING = SIZE * SIZE
    val map = input.linesIterator.map(_.map(_.toString.toInt).toArray).toArray
    var stepCount = 0
    var flashCount = 0
    do {
      flashCount = step(map)
      stepCount += 1
    } while (flashCount != ALL_FLASHING)
    stepCount.toString
  }

  private def step(map: Array[Array[Int]]): Int = {
    for (i <- Range(0, SIZE)) {
      for (j <- Range(0, SIZE)) {
        map(i)(j) += 1
      }
    }

    var flashed = HashSet.empty[(Int, Int)]
    var flashes = 0
    for (i <- Range(0, SIZE)) {
      for (j <- Range(0, SIZE)) {
        if (!flashed((i, j)) && map(i)(j) > FLASH_LIMIT) {
          flashed.add((i, j))
          flashes += 1
          flashes += flashFrom(map, flashed, i, j)
        }
      }
    }

    for (i <- Range(0, SIZE)) {
      for (j <- Range(0, SIZE)) {
        if (map(i)(j) > FLASH_LIMIT) {
          map(i)(j) = 0
        }
      }
    }

    flashes
  }

  private def flashFrom(
      map: Array[Array[Int]],
      flashed: HashSet[(Int, Int)],
      i: Int,
      j: Int
  ): Int = {
    var flashes = 0
    val positions = Vector(
      (i - 1, j - 1),
      (i - 1, j),
      (i - 1, j + 1),
      (i, j - 1),
      (i, j + 1),
      (i + 1, j - 1),
      (i + 1, j),
      (i + 1, j + 1)
    ).filter { case (x, y) => x >= 0 && x < SIZE && y >= 0 && y < SIZE }

    positions.foreach { case (x, y) =>
      if (!flashed((x, y))) {
        map(x)(y) += 1
        if (map(x)(y) > FLASH_LIMIT) {
          flashed.add((x, y))
          flashes += 1
          flashes += flashFrom(map, flashed, x, y)
        }
      }
    }
    flashes
  }
}
