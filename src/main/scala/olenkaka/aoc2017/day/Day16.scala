package olenkaka.aoc2017.day

import scala.util.Try

object Day16 extends Day[String, String] {

  override protected val inputName: String = "Day16-input"

  private val spin = """s(\d+)""".r
  private val exchange = """x(\d+)/(\d+)""".r
  private val partner = """p([a-p])/([a-p])""".r

  private val iterations = 1000000000

  override def part1(inputLines: Seq[String]): Try[String] = Try {
    dance(initPrograms, getMoves(inputLines)).mkString
  }

  override def part2(inputLines: Seq[String]): Try[String] = Try {
    val initialState = initPrograms.toVector
    val moves = getMoves(inputLines)

    def findLoopIterations(danceState: Array[Char], counter: Int): Int = {
      if (danceState.sameElements(initialState) && counter < iterations && counter > 0)
        counter
      else {
        dance(danceState, moves)
        findLoopIterations(danceState, counter + 1)
      }
    }

    val programs = initPrograms
    val loopIterations = findLoopIterations(programs, 0)
    if (loopIterations < iterations) {
      (0 until iterations % loopIterations).foreach(_ => dance(programs, moves))
    }
    programs.mkString
  }

  private def initPrograms: Array[Char] = ('a' to 'p').toArray

  private def dance(programs: Array[Char], moves: Seq[Move]): Array[Char] = {
    moves.foreach(_.apply(programs))
    programs
  }

  private def getMoves(inputLines: Seq[String]): Seq[Move] = {
    inputLines.mkString.split(",")
      .filter(!_.isEmpty)
      .map {
        case spin(num) => Spin(num.toInt)
        case exchange(index1, index2) => Exchange(index1.toInt, index2.toInt)
        case partner(program1, program2) => Partner(program1(0), program2(0))
      }
  }

  private trait Move {
    def apply(programs: Array[Char]): Unit
  }

  private case class Spin(num: Int) extends Move {
    override def apply(programs: Array[Char]): Unit = {
      val spinIdx = programs.length - num
      val movedPrograms = programs.slice(spinIdx, programs.length)
      for (i <- spinIdx - 1 to 0 by -1) {
        programs(i + num) = programs(i)
      }
      for ((p, i) <- movedPrograms.zipWithIndex) {
        programs(i) = p
      }
    }
  }

  private case class Exchange(index1: Int, index2: Int) extends Move {
    override def apply(programs: Array[Char]): Unit = {
      val p1 = programs(index1)
      programs(index1) = programs(index2)
      programs(index2) = p1
    }
  }

  private case class Partner(program1: Char, program2: Char) extends Move {
    override def apply(programs: Array[Char]): Unit = {
      val i1 = programs.indexOf(program1)
      val i2 = programs.indexOf(program2)
      programs(i1) = program2
      programs(i2) = program1
    }
  }

}
