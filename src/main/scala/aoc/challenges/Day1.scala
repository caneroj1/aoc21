package aoc.challenges

class Day1 extends Challenge {
  def part1(input: String): String = {
    val ints = input.linesIterator.map(_.toInt).toArray
    calculateValue(ints, 1)
  }

  def part2(input: String): String = {
    val ints = input.linesIterator.map(_.toInt).toArray
    calculateValue(ints, 3)
  }

  private def calculateValue(measurements: Array[Int], sliceSize: Int): String = {
    var prevSumOpt: Option[Int] = None
    var count = 0
    for (i <- Range(0, measurements.length - (sliceSize - 1))) {
      val currentSum = measurements.slice(i, i + sliceSize).sum
      prevSumOpt match {
        case Some(prevSum) if currentSum > prevSum => count += 1
        case _ =>
      }
      prevSumOpt = Some(currentSum)
    }
    count.toString
  }
}
