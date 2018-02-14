package olenkaka.aoc2017.day

import scala.annotation.tailrec
import scala.util.Try

object Day25 extends Day[Int, Any] {
  override protected val inputName: String = "Day25-input"

  private val beginStateRegex = "Begin in state (\\S).".r
  private val checksumRegex = "Perform a diagnostic checksum after (\\d+) steps.".r

  private val stateRegex = "In state (\\S):".r
  private val newValueRegex = "Write the value (\\d).".r
  private val shiftRegex = "Move one slot to the (\\S+).".r
  private val nextStateRegex = "Continue with state (\\S).".r

  private type Shift = Boolean
  private val left = false
  private val right = true

  private type Value = Boolean
  private val zero = false
  private val one = true

  private type Tape = List[Value]
  private implicit class TapeUtil(tape: Tape) {
    def restTape: List[Value] = tape.tail match {
      case Nil => List(zero)
      case tail => tail
    }
  }

  private case class Command(newValue: Value, shift: Shift, nextState: Char)

  private case class State(commandZero: Command, commandOne: Command) {
    def apply(value: Value): Command = if (value == one) commandOne else commandZero
  }

  override def part1(inputLines: Seq[String]): Try[Int] = Try {
    val (states, beginState, checksum) = parseBlueprints(inputLines)

    @tailrec
    def execute(tapeBefore: Tape, tapeAfter: Tape, currValue: Value, currState: Char, iter: Int): Int = {
      if (iter == 0) {
        tapeBefore.count(_ == one) + tapeAfter.count(_ == one) + (if (currValue == one) 1 else 0)
      } else {
        val command = states(currState)(currValue)
        if (command.shift == right) {
          execute(command.newValue :: tapeBefore, tapeAfter.restTape, tapeAfter.head, command.nextState, iter - 1)
        } else {
          execute(tapeBefore.restTape, command.newValue :: tapeAfter, tapeBefore.head, command.nextState, iter - 1)
        }
      }
    }

    execute(List(zero), List(zero), zero, beginState, checksum)
  }

  private def parseBlueprints(inputLines: Seq[String]): (Map[Char, State], Char, Int) = {
    @inline
    def throwInvalidInput = throw new IllegalArgumentException("Invalid input")

    def extractState(lines: List[String]): (Char, State) = {

      def extractCommand(lines: List[String]): Command = {
        lines match {
          case _ :: nextValueLine :: shiftLine :: stateLine :: Nil =>
            val nextValue = newValueRegex.findFirstMatchIn(nextValueLine).get.group(1)
            val shift = shiftRegex.findFirstMatchIn(shiftLine).get.group(1)
            val state = nextStateRegex.findFirstMatchIn(stateLine).get.group(1)
            Command(
              if (nextValue == "1") one else zero,
              if (shift == "right") right else left,
              state.charAt(0)
            )
          case _ => throwInvalidInput
        }
      }

      lines match {
        case stateInfo :: commands =>
          val state = stateInfo match {
            case stateRegex(s) => s.charAt(0)
          }
          val (commandZero, commandOne) = commands.splitAt(4) match {
            case (infoZero, infoOne) => (extractCommand(infoZero), extractCommand(infoOne))
            case _ => throwInvalidInput
          }
          (state, State(commandZero, commandOne))

        case _ => throwInvalidInput
      }
    }

    inputLines.filter(_.nonEmpty).toList match {
      case beginStateInfo :: checksumInfo :: statesInfo =>
        val beginState = beginStateInfo match {
          case beginStateRegex(state) => state.charAt(0)
        }
        val checksum = checksumInfo match {
          case checksumRegex(num) => num.toInt
        }
        val states = statesInfo.grouped(9)
          .map(extractState)
          .toMap
        (states, beginState, checksum)

      case _ => throwInvalidInput
    }
  }
}
