package aoc.challenges

import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.immutable.Map
import scala.annotation.tailrec

class Day10 extends Challenge {

  val CLOSING_CHARS = Set.from(")]}>")
  val SCORE_TABLE = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val CLOSING_SCORE_TABLE = Map(')' -> 1L, ']' -> 2L, '}' -> 3L, '>' -> 4L)
  val OPENING_TABLE = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')
  val CLOSING_TABLE = OPENING_TABLE.map(_.swap)

  override def part1(input: String): String = {
    val result = input.linesIterator
      .map(l => getFirstCorruptedCharOrStack(l, List.empty))
      .filter(_.isLeft)
      .map { case Left(c) => SCORE_TABLE.getOrElse(c, 0) }
      .sum
    result.toString
  }

  override def part2(input: String): String = {
    val sortedScores = input.linesIterator
      .map(l => getFirstCorruptedCharOrStack(l, List.empty))
      .filter(_.isRight)
      .map { case Right(s) =>
        s.flatMap(CLOSING_TABLE.get(_))
          .map(CLOSING_SCORE_TABLE.getOrElse(_, 0L))
          .fold(0L) { case (acc, score) =>
            5L * acc + score
          }
      }
      .toVector
      .sorted

    sortedScores(sortedScores.length / 2).toString
  }

  @tailrec
  private def getFirstCorruptedCharOrStack(
      line: String,
      stack: List[Char]
  ): Either[Char, List[Char]] = {
    if (line.isEmpty) {
      return Right(stack)
    }

    val char = line.head
    if (CLOSING_CHARS.contains(char)) {
      stack.headOption match {
        case Some(lastCloser) if OPENING_TABLE(char) == lastCloser =>
          getFirstCorruptedCharOrStack(line.tail, stack.tail)
        case _ => Left(char)
      }
    } else {
      getFirstCorruptedCharOrStack(line.tail, stack.prepended(char))
    }
  }
}
