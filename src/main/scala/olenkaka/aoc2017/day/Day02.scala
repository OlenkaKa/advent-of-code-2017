package olenkaka.aoc2017.day

import scala.util.Try

object Day02 extends Day[Int, Any] {
  override protected val inputName: String = "Day02-input"

  override def part1(inputLines: Seq[String]): Try[Int] = Try {
    getSpreadsheet(inputLines)
      .map(l => l.max - l.min)
      .sum
  }

  override def part2(inputLines: Seq[String]): Try[Any] = Try {
    getSpreadsheet(inputLines).map(l =>
      (for (dividend <- l; divisor <- l if dividend != divisor && dividend % divisor == 0) yield dividend / divisor).head)
      .sum
  }

  private def getSpreadsheet(inputLines: Seq[String]): Iterable[Iterable[Int]] = inputLines
    .filter(_.nonEmpty)
    .map(l => l.trim.split("\\s+").map(_.toInt).toIterable)
}
