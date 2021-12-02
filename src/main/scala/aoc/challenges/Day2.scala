package aoc.challenges

class Day2 extends Challenge {
  sealed trait Instruction
  case class Fwd(amount: Int) extends Instruction
  case class Down(amount: Int) extends Instruction
  case class Up(amount: Int) extends Instruction

  override def part1(input: String): String = {
    case class Position(horizontal: Int, vertical: Int)
    val instructions = input.linesIterator.map(parseInstruction)
    val starting = Position(0, 0)
    val end = instructions.foldLeft(starting) { case (Position(h, v), instruction) =>
      instruction match {
        case Down(amount) => Position(h, v + amount)
        case Up(amount) => Position(h, v - amount)
        case Fwd(amount) => Position(h + amount, v)
      }
    }
    (end.horizontal * end.vertical).toString
  }

  override def part2(input: String): String = {
    case class Position(horizontal: Int, vertical: Int, aim: Int)
    val instructions = input.linesIterator.map(parseInstruction)
    val starting = Position(0, 0, 0)
    val end = instructions.foldLeft(starting) { case (Position(h, v, a), instruction) =>
      instruction match {
        case Down(amount) => Position(h, v, a + amount)
        case Up(amount) => Position(h, v, a - amount)
        case Fwd(amount) => Position(h + amount, v + (a * amount), a)
      }
    }
    (end.horizontal * end.vertical).toString
  }

  private def parseInstruction(str: String): Instruction = {
    str match {
      case s"forward $amt" => Fwd(amt.toInt)
      case s"down $amt" => Down(amt.toInt)
      case s"up $amt" => Up(amt.toInt)
    }
  }
}
