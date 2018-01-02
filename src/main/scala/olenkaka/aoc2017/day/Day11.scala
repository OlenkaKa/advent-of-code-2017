package olenkaka.aoc2017.day

import scala.annotation.tailrec
import scala.io.Source

object Day11 {
  //} extends App {

  object Direction extends Enumeration {
    type Direction = Value
    val nw, n, ne, sw, s, se = Value

    def +(vec: (Int, Int), dir: Direction): (Int, Int) = {
      (1,2)
    }
  }

  def main(args: Array[String]): Unit = {
    val steps = Source.fromFile(args(0)).getLines.mkString.split(",")
      .map(s => s.trim)
      .map {
        //        case "nw" => List(0.5, -0.5)
        //        case "n" => List(1.0, 0.0)
        //        case "ne" => List(0.5, 0.5)
        //        case "sw" => List(-0.5, -0.5)
        //        case "s" => List(-1.0, 0.0)
        //        case "se" => List(-0.5, 0.5)
        case "nw" => (1, -1)
        case "n" => (1, 0)
        case "ne" => (1, 1)
        case "sw" => (-1, -1)
        case "s" => (-1, 0)
        case "se" => (-1, 1)
        case _ => throw new IllegalArgumentException("Invalid input")
      }

    println(walk(steps.toList, steps.head, true, 0))
    //      .transpose.map(_.sum)
    //        println(steps)
    //    println((math.abs(steps.head) + math.abs(steps(1))).toInt)
  }

//  def +(direction1: (Int, Int), direction2: (Int, Int)): Int = {
//    (direction1._1 + direction2._1, direction1._2 + direction2._2) match {
//      case _ => _
//    }
//  }

  @tailrec
  def walk(steps: List[(Int, Int)], dir: (Int, Int), plus: Boolean, distance: Int): Int = {

    def notSameSign(i1: Int, i2: Int): Boolean = {
      !((i1 == 0 && i2 == 0) || (i1 < 0 && i2 < 0))
    }

    steps match {
      case (v, h) :: rest =>
        println("----")
        println("direction: " + dir)
        println("(" + v + "," + h + ")")
        val distanceDiff = (dir._1 == v && plus, dir._2 == h && plus) match {
          case (true, true) => 1
          case (false, false) => -1
          case _ => 0
        }
        println("distance diff: " + distanceDiff)
        println("new distance:  " + (distance + distanceDiff))
        walk(rest, (v, h), distanceDiff >= 0, distance + distanceDiff)
      case Nil => distance
    }
  }

}
