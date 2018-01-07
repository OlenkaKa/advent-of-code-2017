package olenkaka.aoc2017.day

import scala.annotation.tailrec
import scala.util.Try

object Day18 extends Day[Long, Any] {

  override protected val inputName: String = "Day18-input"

  private val set = """set\s+([a-z])\s+(\S+)""".r
  private val add = """add\s+([a-z])\s+(\S+)""".r
  private val mul = """mul\s+([a-z])\s+(\S+)""".r
  private val mod = """mod\s+([a-z])\s+(\S+)""".r
  private val snd = """snd\s+([a-z])""".r
  private val rcv = """rcv\s+([a-z])""".r
  private val jgz = """jgz\s+([a-z])\s+(\S+)""".r

  override def part1(inputLines: Seq[String]): Try[Long] = Try {
    execute(inputLines.toVector, 0, Map(), None) match {
      case Some(value) => value
      case None => throw new IllegalArgumentException("cannot recover any frequency")
    }
  }

  @tailrec
  private def execute(commands: Vector[String], index: Int, registries: Map[Char, Long], lastFrequency: Option[Long]): Option[Long] = {

    def getInt(nameOrValue: String): Long = if (nameOrValue.isName) registries.getRegistry(nameOrValue.name) else nameOrValue.toInt

    // Not work with @tailrec
    // def executeNext(newRegistries: Map[Char, Long]): Option[Long] = execute(commands, index + 1, newRegistries, lastFrequency)

    commands(index) match {
      case set(reg, value) => execute(commands, index + 1, registries.update(reg.name, _ => getInt(value)), lastFrequency)
      case add(reg, value) => execute(commands, index + 1, registries.update(reg.name, v => v + getInt(value)), lastFrequency)
      case mul(reg, value) => execute(commands, index + 1, registries.update(reg.name, v => v * getInt(value)), lastFrequency)
      case mod(reg, value) => execute(commands, index + 1, registries.update(reg.name, v => v % getInt(value)), lastFrequency)

      case snd(reg) => registries.getRegistry(reg.name) match {
        case 0 => execute(commands, index + 1, registries, lastFrequency)
        case value => execute(commands, index + 1, registries, Some(value))
      }

      case rcv(reg) => registries.getRegistry(reg.name) match {
        case 0 => execute(commands, index + 1, registries, lastFrequency)
        case _ => lastFrequency
      }

      case jgz(reg, value) => registries.getRegistry(reg.name) match {
        case 0 => execute(commands, index + 1, registries, lastFrequency)
        case _ => execute(commands, index + getInt(value).toInt, registries, lastFrequency)
      }
    }
  }

  private implicit class RegistriesUtil(map: Map[Char, Long]) {
    def update(name: Char, apply: (Long) => Long): Map[Char, Long] = map + (name -> apply(map.getRegistry(name)))

    def getRegistry(registry: Char): Long = map.getOrElse(registry, 0)
  }

  private implicit class RegistryNameUtil(string: String) {
    def name: Char = string(0)

    def isName: Boolean = !(string matches "-?\\d+")
  }

}
