package olenkaka.aoc2017

import olenkaka.aoc2017.day.DayFactory

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Application extends App {

  try {
    require(args.length >= 1, "at least one argument required")

    val day = DayFactory.create(args(0).toInt)
    val fileSource = if (args.length >= 2) Source.fromFile(args(1)) else day.input
    val input = fileSource.getLines.filter(_.nonEmpty).map(_.trim).toList
    fileSource.close

    println("--- Part One ---")
    printResult(day.part1(input))
    println("--- Part Two ---")
    printResult(day.part2(input))

  } catch {
    case e: Exception => println(e)
  }

  def printResult(result: Try[Any]): Unit = {
    result match {
      case Failure(e) => println(e)
      case Success(s) => println(s)
    }
  }
}
