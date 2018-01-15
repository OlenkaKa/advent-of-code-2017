package olenkaka.aoc2017.day

import scala.annotation.tailrec
import scala.util.Try

object Day22 extends Day[Int, Int] {
  override protected val inputName: String = "Day22-input"

  override def part1(inputLines: Seq[String]): Try[Int] = Try {

    @tailrec
    def virusWalk(grid: Grid, index: (Int, Int), direction: Direction, iterations: Int, infections: Int): Int = {
      if (iterations == 0) {
        infections
      } else if (grid.contains(index)) {
        val newDir = direction + Right
        virusWalk(grid - index, newDir + index, newDir, iterations - 1, infections)
      } else {
        val newDir = direction + Left
        virusWalk(grid + index, newDir + index, newDir, iterations - 1, infections + 1)
      }
    }

    virusWalk(initGrid(inputLines), (0, 0), Up, 10000, 0)
  }

  override def part2(inputLines: Seq[String]): Try[Int] = Try {
    // TODO
    0
  }

  private def initGrid(inputLines: Seq[String]): Grid = {
    val lines = inputLines.filter(_.nonEmpty).map(_.trim)
    val (dimX, dimY) = (lines.map(_.length).max, lines.size)
    val (rangeX, rangeY) = (dimX / 2, dimY / 2)

    (lines zip (rangeY to -rangeY by -1))
      .flatMap {
        case (l, y) => (l zip (-rangeX to rangeX))
          .filter(i => i._1 == '#')
          .map(i => (i._2, y))
      }
      .toSet
  }

  private type Grid = Set[(Int, Int)]

  private trait Direction {
    val forward: (Int, Int)

    def +(dir: Direction): Direction

    def +(index: (Int, Int)): (Int, Int) = (index._1 + forward._1, index._2 + forward._2)
  }

  private object Up extends Direction {
    override val forward: (Int, Int) = (0, 1)

    override def +(dir: Direction): Direction = dir match {
      case Left => Left
      case Right => Right
      case Up => Up
      case Down => Down
    }
  }

  private object Down extends Direction {
    override val forward: (Int, Int) = (0, -1)

    override def +(dir: Direction): Direction = dir match {
      case Left => Right
      case Right => Left
      case Up => Down
      case Down => Up
    }
  }

  private object Left extends Direction {
    override val forward: (Int, Int) = (-1, 0)

    override def +(dir: Direction): Direction = dir match {
      case Left => Down
      case Right => Up
      case Up => Left
      case Down => Right
    }
  }

  private object Right extends Direction {
    override val forward: (Int, Int) = (1, 0)

    override def +(dir: Direction): Direction = dir match {
      case Left => Up
      case Right => Down
      case Up => Right
      case Down => Left
    }
  }

}
