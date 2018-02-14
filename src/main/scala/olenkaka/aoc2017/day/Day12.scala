package olenkaka.aoc2017.day

import scala.annotation.tailrec
import scala.util.Try

object Day12 extends Day[Int, Int] {

  override protected val inputName: String = "Day12-input"

  private val regex = """\s*(\d+)\s*<->\s*([\d,\s]+)\s*""".r

  override def part1(inputLines: Seq[String]): Try[Int] = Try {
    getConnections(0, parseInput(inputLines)).size
  }

  override def part2(inputLines: Seq[String]): Try[Int] = Try {
    val connectionsMap = parseInput(inputLines)

    @tailrec
    def countGroups(programsLeft: Set[Int], groupsNum: Int): Int = {
      if (programsLeft.isEmpty) {
        groupsNum
      } else {
        val newGroup = getConnections(programsLeft.head, connectionsMap)
        countGroups(programsLeft -- newGroup, groupsNum + 1)
      }
    }

    countGroups(connectionsMap.keySet, 0)
  }

  private def getConnections(currentProgram: Int, connectionMap: Map[Int, Set[Int]]): Set[Int] = {
    if (!connectionMap.contains(currentProgram))
      Set()
    else {
      val newConnections = connectionMap(currentProgram)
      newConnections ++ newConnections.flatMap(c => getConnections(c, connectionMap.filterKeys(_ != currentProgram)))
    }
  }

  private def parseInput(inputLines: Seq[String]): Map[Int, Set[Int]] = {
    inputLines
      .filter(_.nonEmpty)
      .map {
        case regex(srcProgram, destinationPrograms) => (
          srcProgram.trim.toInt,
          destinationPrograms.split(",").map(_.trim).map(_.toInt).toSet
        )
        case _ => throw new IllegalArgumentException("Invalid input")
      }
      .toMap
  }
}
