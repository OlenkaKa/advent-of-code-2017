package olenkaka.aoc2017.day

import scala.io.Source

object Day13 {

  private val regex = """^\s*(\d+)\s*:\s*(\d+)\s*$""".r

  class Scanner(configuration: List[(Int, Int)]) {

    private val _configuration = configuration
    private var _state: List[(Int, Int)] = configuration.map(_ => (0, 1))

    def size(): Int = {
      _configuration.last._1
    }

    def getRange(position: Int): Int = {
      if (!_configuration.map(e => e._1).contains(position))
        0
      else
        _configuration.find(n => n._1 == position).get._2
    }

    def isScanned(position: Int): Boolean = {
      val depths = _configuration.map(e => e._1)
      if (!depths.contains(position))
        false
      else
        _state(depths.indexOf(position))._1 == 0
    }

    def update(): Unit = {
      _state = (_state, _configuration.map(_._2)).zipped
        .map((state, range) => {
          state._1 + state._2 match {
            case tooSmall if tooSmall < 0 => (1, 1)
            case tooLarge if tooLarge > (range - 1) => (state._1 - 1, -1)
            case sum => (sum, state._2)
          }
        })
    }
  }

  def main(args: Array[String]): Unit = {
    val scanner = new Scanner(Source.fromFile(args(0)).getLines
      .filter(_.nonEmpty)
      .map {
        case regex(depth, range) => (depth.toInt, range.toInt)
        case _ => throw new IllegalArgumentException("Invalid input")
      }.toList)

    var result = 0
    for (depth <- 0 to scanner.size()) {
      if (scanner.isScanned(depth)) {
        result += depth * scanner.getRange(depth)
      }
      scanner.update()
    }

    println(result)
  }
}
