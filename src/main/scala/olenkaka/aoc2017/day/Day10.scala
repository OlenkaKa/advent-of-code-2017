package olenkaka.aoc2017.day

import scala.annotation.tailrec
import scala.util.Try

object Day10 extends Day[Int, String] {

  override val inputName: String = "Day10-input"

  private val range = List.range(0, 256)
  private val lengthsAppendix: List[Int] = List(17, 31, 73, 47, 23)

  override def part1(inputLines: Seq[String]): Try[Int] = Try {
    val lengths = getLengths(inputLines)
    val (hashResult, _, _) = hash(range, lengths, 0, 0)
    hashResult.head * hashResult(1)
  }

  override def part2(inputLines: Seq[String]): Try[String] = Try {
    knotHash(getLengths(inputLines).map(_.toString).mkString(","))
  }

  def knotHash(input: String): String = {
    val lengths = input.map(_.toInt).toList ::: lengthsAppendix
    knotHash(range, lengths)
  }

  private def getLengths(inputLines: Seq[String]): List[Int] = inputLines.mkString
    .split(",")
    .filter(!_.isEmpty)
    .map(s => s.trim.toInt)
    .toList

  private def knotHash(values: List[Int], lengths: List[Int]): String = {

    @tailrec
    def roundsHash(currentValues: List[Int], position: Int, skipSize: Int, rounds: Int): List[Int] = {
      rounds match {
        case 0 => currentValues
        case _ =>
          val (newValues, newPosition, newSkipSize) = hash(currentValues, lengths, position, skipSize)
          roundsHash(newValues, newPosition, newSkipSize, rounds - 1)
      }
    }

    roundsHash(values, 0, 0, 64)
      .grouped(16).map(_.reduce(_ ^ _))
      .map(n => "%02x".format(n)).mkString
  }

  private def hash(values: List[Int], lengths: List[Int], position: Int, skipSize: Int): (List[Int], Int, Int) = {
    lengths match {
      case Nil => (values, position, skipSize)
      case length :: lengthsTail => hash(reverse(values, position, length),
        lengthsTail,
        (position + length + skipSize) % values.length,
        skipSize + 1)
    }
  }

  private def reverse(list: List[Int], position: Int, length: Int): List[Int] = {
    val listLength = list.length
    if (position + length > listLength) {
      val endSublist = list.slice(position, listLength)
      val startSublist = list.slice(0, length - listLength + position)
      val reservedList = (endSublist ::: startSublist).reverse

      list.patch(position, reservedList.slice(0, listLength - position), listLength)
        .patch(0, reservedList.slice(listLength - position, reservedList.length), length - listLength + position)
    } else
      list.patch(position, list.slice(position, position + length).reverse, length)
  }
}
