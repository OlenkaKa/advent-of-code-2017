package olenkaka.aoc2017.day

import scala.annotation.tailrec
import scala.util.Try

object Day19 extends Day[String, Int] {
  override val inputName: String = "Day19-input"

  override def part1(inputLines: Seq[String]): Try[String] = Try {
    followPath(createRoutingDiagram(inputLines), Nil, (letters: List[Char], field) => {
      field match {
        case Letter(l) => l :: letters
        case _ => letters
      }
    }).reverse.mkString
  }

  override def part2(inputLines: Seq[String]): Try[Int] = Try {
    followPath(createRoutingDiagram(inputLines), 0, (counter: Int, _) => counter + 1)
  }

  private def createRoutingDiagram(inputLines: Seq[String]): Vector[Vector[Field]] = {
    (for (line <- inputLines.filter(_.nonEmpty).map(_.replaceAll("\\s+$", ""))) yield line
      .map {
        case c if c.isWhitespace => Empty
        case '|' | '-' => Path
        case '+' => Crossing
        case c => Letter(c)
      }.toVector).toVector
  }

  private def followPath[T](diagram: Vector[Vector[Field]], initResult: T, apply: (T, Field) => T): T = {
    @tailrec
    def followPath(index: (Int, Int), dir: Direction, result: T): T = {
      diagram(index) match {
        case Path => followPath(index + dir.step, dir, apply(result, Path))
        case Crossing =>
          val newDir = dir match {
            case Up | Down => if (diagram(index + Left.step) != Empty) Left else Right
            case Left | Right => if (diagram(index + Up.step) != Empty) Up else Down
            case _ => throw new IllegalArgumentException("unknown direction")
          }
          followPath(index + newDir.step, newDir, apply(result, Crossing))
        case letter: Letter => followPath(index + dir.step, dir, apply(result, letter))
        case Empty => result
      }
    }

    followPath((diagram(0).indexOf(Path), 0), Down, initResult)
  }

  private implicit class RoutingDiagram(diagram: Vector[Vector[Field]]) {
    def apply(index: (Int, Int)): Field = diagram
      .lift(index._2).getOrElse(Vector())
      .lift(index._1).getOrElse(Empty)
  }

  implicit class IndexAdd(idx1: (Int, Int)) {
    def +(idx2: (Int, Int)): (Int, Int) = (idx2._1 + idx1._1, idx2._2 + idx1._2)
  }


  private case class Direction(step: (Int, Int))
  private object Up extends Direction(0, -1)
  private object Down extends Direction(0, 1)
  private object Left extends Direction(-1, 0)
  private object Right extends Direction(1, 0)

  private trait Field
  private object Empty extends Field
  private object Path extends Field
  private object Crossing extends Field
  private case class Letter(letter: Char) extends Field

}
