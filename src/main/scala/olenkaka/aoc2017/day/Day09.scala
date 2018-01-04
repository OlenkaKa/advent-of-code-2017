package olenkaka.aoc2017.day

import scala.io.Source

object Day09 {

  private val garbageRegex = """<(([^>!]|!.)*)>""".r
  private val groupRegex = """\{(.*)\}""".r

  def splitGroupContent(stream: String): List[String] = {
    if (stream.isEmpty) Nil
    else {
      var startIdx, endIdx = 0
      var openingBrackets, closingBrackets = 0
      var inGarbage, ignoreNext = false
      var resultArray: List[String] = Nil

      stream.foreach(c => {
        c match {
          case ',' =>
            if (!inGarbage && openingBrackets == closingBrackets) {
              resultArray = resultArray :+ stream.substring(startIdx, endIdx)
              startIdx = endIdx + 1
            }
            ignoreNext = false
          case '{' =>
            if (!inGarbage) openingBrackets += 1
            ignoreNext = false
          case '}' =>
            if (!inGarbage) closingBrackets += 1
            ignoreNext = false
          case '<' =>
            inGarbage = true
            ignoreNext = false
          case '>' =>
            if (inGarbage && !ignoreNext) inGarbage = false
            ignoreNext = false
          case '!' =>
            if (inGarbage) ignoreNext = !ignoreNext
          case _ =>
            ignoreNext = false
        }
        endIdx += 1
      })
      resultArray :+ stream.substring(startIdx, endIdx)
    }
  }

  def calculateScore(stream: String, nestingValue: Int): Int = {
    stream match {
      case garbageRegex(_*) => 0
      case groupRegex(content) => nestingValue + splitGroupContent(content)
        .map(s => calculateScore(s, nestingValue + 1))
        .sum
      case _ => throw new IllegalArgumentException("Invalid input: '" + stream + "'");
    }
  }

  def calculateGarbageCharacters(stream: String): Int = {
    stream match {
      case garbageRegex(content, _*) => countCharacters(content)
      case groupRegex(content) => splitGroupContent(content)
        .map(s => calculateGarbageCharacters(s))
        .sum
      case _ => throw new IllegalArgumentException("Invalid input: '" + stream + "'");
    }
  }

  def countCharacters(garbage: String): Int = {
    var ignoreNext = false
    var counter = 0
    garbage.foreach {
      case '!' => ignoreNext = !ignoreNext
      case _ => if (ignoreNext) ignoreNext = false else counter += 1
    }
    counter
  }

  def main(args: Array[String]): Unit = {
    val stream = Source.fromFile(args(0)).getLines.mkString.trim
    println("-- Part One --")
    println(calculateScore(stream, 1))
    println("-- Part Two --")
    println(calculateGarbageCharacters(stream))
  }
}
