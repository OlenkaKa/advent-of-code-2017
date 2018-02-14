package olenkaka.aoc2017.day

import scala.io.{BufferedSource, Source}
import scala.util.Try

object Day {
  def apply[T1, T2](num: Int): Day[T1, T2] = {
    require(num >= 1 && num <= 25, s"day $num is invalid - it must be from range 1-25")
    (num match {
      case 1 => Day01
      case 2 => Day02
      case 4 => Day04
      case 10 => Day10
      case 12 => Day12
      case 14 => Day14
      case 15 => Day15
      case 16 => Day16
      case 17 => Day17
      case 18 => Day18
      case 19 => Day19
      case 20 => Day20
      case 21 => Day21
      case 22 => Day22
      case 23 => Day23
      case 24 => Day24
      case 25 => Day25
    }).asInstanceOf[Day[T1, T2]]
  }
}

trait Day[T1, T2] {

  protected val inputName: String

  def part1(inputLines: Seq[String]): Try[T1] = Try(???)

  def part2(inputLines: Seq[String]): Try[T2] = Try(???)

  def input: BufferedSource = Source.fromResource(inputName)
}
