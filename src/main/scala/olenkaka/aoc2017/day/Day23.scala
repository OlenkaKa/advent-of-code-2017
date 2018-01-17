package olenkaka.aoc2017.day

import scala.annotation.tailrec
import scala.util.Try

object Day23 extends Day[Long, Long] {

  override protected val inputName: String = "Day23-input"

  private val set = """set\s+([a-h])\s+(\S+)""".r
  private val sub = """sub\s+([a-h])\s+(\S+)""".r
  private val mul = """mul\s+([a-h])\s+(\S+)""".r
  private val jnz = """jnz\s+(\S+)\s+(\S+)""".r

  private val initRegisters: Map[Char, Long] = ('a' to 'h').map(_ -> 0L).toMap

  override def part1(inputLines: Seq[String]): Try[Long] = Try {
    val commands = inputLines.toVector

    @tailrec
    def execute(registries: Map[Char, Long], idx: Int, mulNum: Long): Long = {
      if (idx >= commands.size) {
        mulNum
      } else {
        def toLong(value: String) = if (!value(0).isLower) value.toLong else registries(value(0))

        commands(idx) match {
          case set(reg, value) => execute(registries + (reg(0) -> toLong(value)), idx + 1, mulNum)
          case sub(reg, value) => execute(registries + (reg(0) -> (toLong(reg) - toLong(value))), idx + 1, mulNum)
          case mul(reg, value) => execute(registries + (reg(0) -> (toLong(reg) * toLong(value))), idx + 1, mulNum + 1)
          case jnz(reg, value) => toLong(reg) match {
            case 0 => execute(registries, idx + 1, mulNum)
            case _ => execute(registries, idx + toLong(value).toInt, mulNum)
          }
        }
      }
    }

    execute(initRegisters, 0, 0)
  }
}
