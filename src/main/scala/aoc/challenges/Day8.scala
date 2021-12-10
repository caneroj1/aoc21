package aoc.challenges

class Day8 extends Challenge {

  override def part1(input: String): String = {
    val result = input.linesIterator
      .map(l => {
        val idx = l.indexOf(" | ")
        (l.substring(0, idx), l.substring(idx + 3))
      })
      .map(countUniqueDigits)
      .sum
    result.toString
  }

  override def part2(input: String): String = ???

  private def countUniqueDigits(strPair: (String, String)): Int = {
    strPair._2.split(' ').map(_.length).filter(l => l match {
      case 2 => true
      case 4 => true
      case 3 => true
      case 7 => true
      case _ => false
    }).length
  }
}
