package olenkaka.aoc2017.day

import scala.io.{BufferedSource, Source}
import scala.util.Try


object Day {
  def create[T1, T2](num: Int): Day[T1, T2] = {
    require(num >= 1 && num <= 25, s"day $num is invalid - it must be from range 1-25")
    val day = num match {
      case 1 => Day01
      case 10 => Day10
      case 14 => Day14
      case 15 => Day15
      case 16 => Day16
      case 17 => Day17
      case 18 => Day18
      case 19 => Day19
    }
    day.asInstanceOf[Day[T1, T2]]
  }
}

trait Day[T1, T2] {

  protected val inputName: String

  def part1(inputLines: Seq[String]): Try[T1] = Try(???)

  def part2(inputLines: Seq[String]): Try[T2] = Try(???)

  def input: BufferedSource = Source.fromResource(inputName)
}
