package olenkaka.aoc2017.day

import scala.util.Try

object Day01 extends Day[Int, Int] {

  override val inputName: String = "Day01-input"

  override def part1(inputLines: Seq[String]): Try[Int] = Try {
    val input = getInput(inputLines)
    calculateCaptcha(input, 1)
  }

  override def part2(inputLines: Seq[String]): Try[Int] = Try {
    val input = getInput(inputLines)
    calculateCaptcha(input, input.length / 2)
  }

  private def getInput(inputLines: Seq[String]) = inputLines.mkString.map(_.toString.toInt)

  private def calculateCaptcha(input: Seq[Int], step: Int) = {
    val length = input.length
    input.indices.map(i => if (input(i) == input((i + step) % length)) input(i) else 0).sum
  }
}
