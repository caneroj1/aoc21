package aoc.challenges

import annotation.tailrec

class Day1 extends Challenge {
  override def part1(input: String): String = {
    val ints = input.linesIterator.map(_.toInt).toArray
    // val result = calculateValue(ints, 1)
    val result = calculateValueTailRec(ints, 1, None, 0)
    result.toString
  }

  override def part2(input: String): String = {
    val ints = input.linesIterator.map(_.toInt).toArray
    // val result = calculateValue(ints, 3)
    val result = calculateValueTailRec(ints, 3, None, 0)
    result.toString
  }

  // alternative implementation using tail recursion
  // that might be considered more "functional" than
  // the other impl using iteration.
  @tailrec
  private def calculateValueTailRec(
      measurements: Array[Int],
      sliceSize: Int,
      previousSum: Option[Int],
      count: Int
  ): Int = {
    val slice = measurements.take(sliceSize)
    if (slice.size != sliceSize) {
      return count
    }

    val currentSum = slice.sum
    val increment = previousSum match {
      case Some(v) if currentSum > v => 1
      case _                         => 0
    }
    calculateValueTailRec(
      measurements.tail,
      sliceSize,
      Some(currentSum),
      count + increment
    )
  }

  private def calculateValue(
      measurements: Array[Int],
      sliceSize: Int
  ): Int = {
    var prevSumOpt: Option[Int] = None
    var count = 0
    for (i <- Range(0, measurements.length - (sliceSize - 1))) {
      val currentSum = measurements.slice(i, i + sliceSize).sum
      prevSumOpt match {
        case Some(prevSum) if currentSum > prevSum => count += 1
        case _                                     =>
      }
      prevSumOpt = Some(currentSum)
    }
    count
  }
}
