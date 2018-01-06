package olenkaka.aoc2017.day

import scala.annotation.tailrec
import scala.util.Try

object Day15 extends Day[Int, Any] {

  override val inputName: String = "Day15-input"

  private val inputRegex = "Generator (A|B) starts with (\\d+)".r
  private val factorA = 16807
  private val factorB = 48271
  private val divideBase = 2147483647

  override def part1(inputLines: Seq[String]): Try[Int] = Try {
    countMatches(getFirstPair(inputLines), 40000000, 0)
  }

  private def getFirstPair(inputLines: Seq[String]) = {
    def noValueError(gen: Char) = throw new IllegalArgumentException("Value for " + gen + " generator not defined")

    var a: Option[Int] = None
    var b: Option[Int] = None
    for (line <- inputLines) {
      line match {
        case inputRegex("A", value) => a = Some(value.toInt)
        case inputRegex("B", value) => b = Some(value.toInt)
        case inputRegex(_*) =>
      }
    }
    Pair(a.getOrElse(noValueError('A')), b.getOrElse(noValueError('B')))
  }

  @tailrec
  private def countMatches(pair: Pair, iterations: Int, count: Int): Int = {
    iterations match {
      case i if i == 0 => count
      case _ =>
        val nextPair = Pair(pair.valueA * factorA % divideBase, pair.valueB * factorB % divideBase)
        countMatches(nextPair, iterations - 1, if(nextPair.matches) count + 1 else count)
    }
  }

  private case class Pair(valueA: Long, valueB: Long) {
    def matches: Boolean = {
      val bytesA = f"${valueA.toBinaryString}%32s".replaceAll(" ", "0")
      val bytesB = f"${valueB.toBinaryString}%32s".replaceAll(" ", "0")
      bytesA.substring(16, 32) == bytesB.substring(16, 32)
    }
  }

}
