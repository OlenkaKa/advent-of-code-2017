package olenkaka.aoc2017.day

import scala.io.Source

object Day14 {
  def main(args: Array[String]): Unit = {
    val scanner = Source.fromFile(args(0)).getLines
      .filter(_.nonEmpty)
  }
}
