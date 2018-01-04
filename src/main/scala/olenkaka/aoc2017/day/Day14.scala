package olenkaka.aoc2017.day

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object Day14 extends Day[Int, Int] {

  override val inputName: String = "Day14-input"

  val gridSize = 128

  override def part1(inputLines: Seq[String]): Try[Int] = Try {
    createGrid(inputLines).flatten.count(_ == Used)
  }

  override def part2(inputLines: Seq[String]): Try[Int] = Try {
    val grid = createGrid(inputLines)
    val regions = Vector.fill[Array[Int]](gridSize)(Array.fill[Int](gridSize)(0))
    markRegions(grid, regions, 0, Index(0, 0))
    regions.flatten.distinct.count(_ != 0)
  }

  private def createGrid(inputLines: Seq[String]): Vector[Vector[Field]] = {
    val phase = inputLines.mkString
    (0 until gridSize)
      .map(i => Day10.knotHash(s"$phase-$i")
        .flatMap(c => "%04d".format(Integer.parseInt(c.toString, 16).toBinaryString.toInt))
        .map(c => if (c == '1') Used else Free)
        .toVector)
      .toVector
  }

  private def markRegions(grid: Vector[Vector[Field]], regions: Vector[Array[Int]], maxRegionNum: Int, idx: Index): Unit = {

    def nextIndex(i: Index): Option[Index] = {
      (i.x + 1, i.y + 1) match {
        case (overflowX, _) if overflowX == gridSize => None
        case (newX, overflowY) if overflowY == gridSize => Some(Index(newX, 0))
        case (_, newY) => Some(Index(i.x, newY))
      }
    }

    def neighbourIndices(index: Index): Seq[Index] = {
      val neighbours = new ArrayBuffer[Index](4)
      if (index.x + 1 < gridSize) {
        neighbours.append(Index(index.x + 1, index.y))
      }
      if (index.x - 1 >= 0) {
        neighbours.append(Index(index.x - 1, index.y))
      }
      if (index.y + 1 < gridSize) {
        neighbours.append(Index(index.x, index.y + 1))
      }
      if (index.y - 1 >= 0) {
        neighbours.append(Index(index.x, index.y - 1))
      }
      neighbours
    }

    def neighbourRegions(index: Index): List[Int] = {
      val neighbourRegions = new ArrayBuffer[Int](4)
      for (nIndex <- neighbourIndices(index); value = regions(nIndex.x)(nIndex.y) if value != 0) {
        neighbourRegions.append(value)
      }
      neighbourRegions.distinct.toList
    }

    def markRegion(index: Index, regionNum: Int, checkedIndices: mutable.Set[Index]): Unit = {
      if (!checkedIndices.contains(index)) {
        regions(index.x)(index.y) = regionNum
        checkedIndices.add(index)
        for (nIndex <- neighbourIndices(index); value = regions(nIndex.x)(nIndex.y) if value != 0 && value != regionNum) {
          markRegion(nIndex, regionNum, checkedIndices)
        }
      }
    }

    var lastRegion = maxRegionNum
    if (grid(idx.x)(idx.y) == Used) {
      lastRegion = if (neighbourRegions(idx) == Nil) lastRegion + 1 else lastRegion
      markRegion(idx, lastRegion, mutable.HashSet())
    }

    nextIndex(idx) match {
      case Some(nextIdx) => markRegions(grid, regions, lastRegion, nextIdx)
      case None =>
    }
  }

  case class Index(x: Int, y: Int)

  sealed trait Field
  object Used extends Field
  object Free extends Field
}
