package olenkaka.aoc2017.day

import scala.io.Source

object Day12 {

  private val regex = """\s*(\d+)\s*<->\s*([\d,\s]+)\s*""".r

  def main(args: Array[String]): Unit = {
    val connectionMap = Source.fromFile(args(0)).getLines
      .filter(_.nonEmpty)
      .map {
        case regex(srcProgram, destinationPrograms) => (
          srcProgram.trim.toInt,
          destinationPrograms.split(",").map(_.trim).map(_.toInt).toSet
        )
        case _ => throw new IllegalArgumentException("Invalid input")
      }
      .toMap

    println("-- Part One --")
    println(getConnections(0, connectionMap).size)
    println("-- Part Two --")
  }

  def getConnections(currentProgram: Int, connectionMap: Map[Int, Set[Int]]): Set[Int] = {
    if (!connectionMap.contains(currentProgram))
      Set()
    else {
      val newConnections = connectionMap(currentProgram)
      newConnections ++ newConnections.flatMap(c => getConnections(c, connectionMap.filterKeys(_ != currentProgram)))
    }
  }
}
