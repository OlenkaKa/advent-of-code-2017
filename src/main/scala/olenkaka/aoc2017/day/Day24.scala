package olenkaka.aoc2017.day

import scala.util.Try

object Day24 extends Day[Int, Int] {

  override protected val inputName: String = "Day24-input"

  private val components = "(\\d+)/(\\d+)".r

  private type Port = Int
  private type Component = (Port, Port)
  private type Bridge = List[Component]

  override def part1(inputLines: Seq[String]): Try[Int] = Try {
    findBridges(createComponents(inputLines))
      .map(b => b.map(c => c._1 + c._2).sum)
      .max
  }

  override def part2(inputLines: Seq[String]): Try[Int] = Try {
    findBridges(createComponents(inputLines))
      .groupBy(b => b.length)
      .maxBy(_._2.head.length)._2
      .map(b => b.map(c => c._1 + c._2).sum)
      .max
  }

  def dropFirstMatch[A](ls: Seq[A], value: A): Seq[A] = {
    val index = ls.indexOf(value) //index is -1 if there is no match
    if (index < 0) {
      ls
    } else if (index == 0) {
      ls.tail
    } else {
      // splitAt keeps the matching element in the second group
      val (a, b) = ls.splitAt(index)
      a ++ b.tail
    }
  }

  private def findBridges(components: Seq[Component]): Set[Bridge] = {
    def buildBridges(bridges: Set[(Bridge, Port)], freeComponents: Seq[Component]): Set[(Bridge, Port)] = {
      bridges.flatMap {
        case (bridge, port) =>
          val matchingComponents = freeComponents.filter(c => c._1 == port || c._2 == port)
          if (matchingComponents.isEmpty) {
            Seq((bridge, port))
          } else {
            matchingComponents.flatMap(c => buildBridges(
              Set((bridge ++ List(c), if (c._1 == port) c._2 else c._1)),
              dropFirstMatch(freeComponents, c))
            )
          }
      }
    }

    buildBridges(Set((List[Component](), 0)), components).map {
      case (bridge, _) => bridge
    }
  }

  private def createComponents(inputLines: Seq[String]): Seq[Component] = {
    inputLines
      .filter(_.nonEmpty)
      .map {
        case components(first, second) => (first.toInt, second.toInt)
      }
  }
}
