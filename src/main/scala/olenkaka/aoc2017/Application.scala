package olenkaka.aoc2017

import olenkaka.aoc2017.day.Day

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Application extends App {

  try {
    require(args.length >= 1, "Missing day number")

    val day = Day(args(0).toInt)
    val fileSource = if (args.length >= 2) {
      Source.fromFile(args(1))
    } else {
      println("Using default day input")
      day.input
    }

    val input = fileSource.getLines.toList
    fileSource.close

    println("--- Part One ---")
    printResult(day.part1(input))
    println("--- Part Two ---")
    printResult(day.part2(input))

  } catch {
    case e: IllegalArgumentException => println(e.getLocalizedMessage)
  }

  def printResult[T](result: Try[T]): Unit = {
    result match {
      case Failure(e) => println("Unable to calculate the result: " + e)
      case Success(s) => println(s)
    }
  }
}
