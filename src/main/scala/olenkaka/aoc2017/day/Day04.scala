package olenkaka.aoc2017.day

import scala.util.Try

object Day04 extends Day[Int, Int] {

  override protected val inputName: String = "Day04-input"

  override def part1(inputLines: Seq[String]): Try[Int] = Try {
    inputLines.filter(_.nonEmpty)
      .map(_.trim.split("\\s+"))
      .count(words => words.distinct.length == words.length)
  }

  override def part2(inputLines: Seq[String]): Try[Int] = Try {
    inputLines.filter(_.nonEmpty)
      .map(_.trim.split("\\s+"))
      .count(words => !words.exists(duplicate => {
        {
          val (start, end) = words.span(w => w != duplicate)
          start ++ end.drop(1)
        }.exists(w => w.permutations.contains(duplicate))
      }))
  }
}
