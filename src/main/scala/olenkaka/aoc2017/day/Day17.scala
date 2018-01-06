package olenkaka.aoc2017.day

import scala.annotation.tailrec
import scala.util.Try

object Day17 extends Day[Int, Any] {

  override protected val inputName: String = "Day17-input"

  val lastInsert1 = 2017
  val lastInsert2 = 50000000

  override def part1(inputLines: Seq[String]): Try[Int] = Try {
    val stepNum = inputLines.mkString.toInt

    def nextIndex(index: Int, listLength: Int): Int = (index + 1) % listLength

    @tailrec
    def iterate(list: List[Int], insertNum: Int, index: Int, stepCounter: Int): Int = {
      if (insertNum == lastInsert1) {
        list(nextIndex(index, lastInsert1 + 1))

      } else if (stepCounter == 0) {
        val newInsert = insertNum + 1
        iterate(list.slice(0, index + 1) ::: newInsert :: list.slice(index + 1, list.length),
          newInsert, nextIndex(index, list.length + 1), stepNum)

      } else {
        iterate(list, insertNum, nextIndex(index, list.length), stepCounter - 1)
      }
    }

    iterate(List(0), 0, 0, stepNum)
  }
}
