package olenkaka.aoc2017.day

import scala.util.Try

object Day20 extends Day[Int, Any] {

  private val coordns = """<(-?\d+),(-?\d+),(-?\d+)>""".r
  private val line = ("p=" + coordns + "\\s*,\\s*" + "v=" + coordns + "\\s*,\\s*" + "a=" + coordns).r

  override protected val inputName: String = "Day20-input"

  override def part1(inputLines: Seq[String]): Try[Int] = Try {
    getParticles(inputLines).zipWithIndex.minBy(p => p._1.a.distance)._2
  }

  private def getParticles(inputLines: Seq[String]): Seq[Particle] = inputLines.map {
    case line(px, py, pz, vx, vy, vz, ax, ay, az) => Particle(
      Coordinates(px.toInt, py.toInt, pz.toInt),
      Coordinates(vx.toInt, vy.toInt, vz.toInt),
      Coordinates(ax.toInt, ay.toInt, az.toInt))
  }

  private case class Coordinates(x: Int, y: Int, z: Int) {
    def +(other: Coordinates): Coordinates = Coordinates(x + other.x, y + other.y, z + other.z)

    def distance: Int = x.abs + y.abs + z.abs
  }

  private case class Particle(p: Coordinates, v: Coordinates, a: Coordinates) {
    def updated: Particle = {
      val newV = v + a
      val newP = p + newV
      Particle(newP, newV, a)
    }

    def distance: Int = p.distance
  }

}
