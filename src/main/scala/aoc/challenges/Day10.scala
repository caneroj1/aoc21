package aoc.challenges

import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.immutable.Map
import scala.annotation.tailrec

class Day10 extends Challenge {

  val CLOSING_CHARS = Set.from(")]}>")
  val SCORE_TABLE = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val OPENING_TABLE = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')

  override def part1(input: String): String = {
    val result = input.linesIterator
      .flatMap(l => getFirstCorruptedChar(l, List.empty))
      .map(c => SCORE_TABLE.getOrElse(c, 0))
      .sum
    result.toString
  }

  override def part2(input: String): String = ???

  @tailrec
  private def getFirstCorruptedChar(
      line: String,
      stack: List[Char]
  ): Option[Char] = {
    if (line.isEmpty) {
      return None
    }

    val char = line.head
    if (CLOSING_CHARS.contains(char)) {
      stack.headOption match {
        case Some(lastCloser) if OPENING_TABLE(char) == lastCloser =>
          getFirstCorruptedChar(line.tail, stack.tail)
        case _ => Some(char)
      }
    } else {
      getFirstCorruptedChar(line.tail, stack.prepended(char))
    }
  }
}
