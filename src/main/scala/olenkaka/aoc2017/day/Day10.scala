package olenkaka.aoc2017.day

import scala.io.Source

object Day10 {
  //extends App {
  private val adder: List[Int] = List(17, 31, 73, 47, 23)

  def main(args: Array[String]): Unit = {
    val lengths = Source.fromFile(args(0)).getLines.mkString.split(",").map(s => s.trim.toInt)
    //        val lengths = "3,4,1,5".split(",").map(_.toInt)
    val list = List.range(0, 256)
    println("-- Part One --")
    val hashList = hash(list, lengths.toList, 0, 0)
    println(hashList.head * hashList(1))

    println("-- Part Two --")
    println(hashList.length)
    val newLengths: List[Int] = lengths.toList ::: adder
    println(newLengths)
    val l = List(65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22,
      65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22)
    println(denseHash(l))
    val dense = denseHash(l)
    println(List(64, 7, 255)
      .map(n => String.format("%2s", n.toHexString).replace(" ", "0"))
      .mkString
    )
    val ll = List(1, 2, 3)
    println(knotHash(list, ll ::: adder, 0, 0, 64))
  }

  def asciiList(list: List[Int]): List[Int] = {
    list.map(_.toString).mkString(",").map(_.toInt).toList
    //    list.map(s => (DatatypeConverter.parseHexBinary(s.toString).toString.toInt, 44)).flatten ::: List(17, 31, 73, 47, 23)
  }

  def denseHash(list: List[Int]): List[Int] = {
    list.grouped(16).map(_.reduce(_ ^ _)).toList
  }

  def hash(list: List[Int], lengths: List[Int], position: Int, skipSize: Int): List[Int] = {
    if (lengths.isEmpty)
      list
    else {
      hash(reverse(list, position, lengths.head),
        lengths.tail,
        (position + lengths.head + skipSize) % list.length,
        skipSize + 1)
    }
  }

  def modhash(list: List[Int], lengths: List[Int], position: Int, skipSize: Int): (List[Int], Int, Int) = {
    if (lengths.isEmpty)
      (list, position, skipSize)
    else {
      modhash(reverse(list, position, lengths.head),
        lengths.tail,
        (position + lengths.head + skipSize) % list.length,
        skipSize + 1)
    }
  }

  def knotHash(list: List[Int], lengths: List[Int], position: Int, skipSize: Int, counter: Int): String = {
    counter match {
      case 0 => denseHash(list)
        .map(n => String.format("%2s", n.toHexString).replace(" ", "0"))
        .mkString
      case n => val hashResult = modhash(list, lengths, position, skipSize)
        knotHash(hashResult._1, lengths, hashResult._2, hashResult._3, counter - 1)
    }
  }

  def reverse(list: List[Int], position: Int, length: Int): List[Int] = {
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
