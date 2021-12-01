package aoc

import aoc.challenges.Challenge
import scala.io.Source

object Main {
  def main(args: Array[String]) {
    args.map(_.toIntOption) match {
      case Array(Some(day), Some(part)) => runChallengeFor(day, part)
      case _                            => println("Invalid args")
    }
  }

  private def runChallengeFor(day: Int, part: Int) {
    val ctor = Class.forName(s"aoc.challenges.Day$day").getConstructor()
    val instance = ctor.newInstance().asInstanceOf[Challenge]
    val method = instance.getClass.getMethod(s"part$part", classOf[String])
    val input = Source.fromFile(s"input/day$day/part$part").mkString
    println(method.invoke(instance, input))
  }
}
