package aoc.challenges

import scala.annotation.tailrec

class Day3 extends Challenge {
  case class Counter(var zeroCount: Int, var oneCount: Int)

  override def part1(input: String): String = {
    val BIT_COUNT = 12
    var counters = Array.fill[Counter](BIT_COUNT)(Counter(0, 0))
    input.linesIterator.foreach(s =>
      s.zipWithIndex.foreach { case (c, i) =>
        c match {
          case '0' => counters(i).zeroCount += 1
          case '1' => counters(i).oneCount += 1
        }
      }
    )
    val gammaBits = counters.map(c => if (c.oneCount > c.zeroCount) '1' else '0').mkString
    val epsilonBits = counters.map(c => if (c.oneCount > c.zeroCount) '0' else '1').mkString
    val result = Integer.parseUnsignedInt(gammaBits, 2) * Integer.parseUnsignedInt(epsilonBits, 2)
    result.toString
  }

  override def part2(input: String): String = {
    val inputLines = input.linesIterator.toArray
    val oxygenNumber = findRating(inputLines, true, 0)
    val scrubberNumber = findRating(inputLines, false, 0)
    val result = oxygenNumber * scrubberNumber
    result.toString
  }

  private def getBitForPosition(
      inputNumbers: Array[String],
      position: Int,
      useMostCommon: Boolean
  ): Char = {
    var counter = Counter(0, 0)
    inputNumbers.foreach(s =>
      s(position) match {
        case '0' => counter.zeroCount += 1
        case '1' => counter.oneCount += 1
      }
    )

    if (useMostCommon) {
      if (counter.oneCount >= counter.zeroCount) '1' else '0'
    } else {
      if (counter.zeroCount <= counter.oneCount) '0' else '1'
    }
  }

  @tailrec
  private def findRating(
      inputNumbers: Array[String],
      useMostCommon: Boolean,
      index: Int
  ): Int = {
    val bit = getBitForPosition(inputNumbers, index, useMostCommon)
    val remainingNumbers = inputNumbers.filter(s => s(index) == bit)
    if (remainingNumbers.isEmpty) {
      0
    } else if (remainingNumbers.length == 1) {
      Integer.parseUnsignedInt(remainingNumbers.head, 2)
    } else {
      findRating(remainingNumbers, useMostCommon, index + 1)
    }
  }
}
