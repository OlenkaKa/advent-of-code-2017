package olenkaka.aoc2017.day

import scala.annotation.tailrec
import scala.util.Try

object Day21 extends Day[Int, Any] {

  override protected val inputName: String = "Day21-input"

  private val rule2 = "(..)/(..)\\s+=>\\s+(...)/(...)/(...)".r
  private val rule3 = "(...)/(...)/(...)\\s+=>\\s+(....)/(....)/(....)/(....)".r

  private val initSquare = Square(Vector(Vector(Off, On, Off), Vector(Off, Off, On), Vector(On, On, On)))

  override def part1(inputLines: Seq[String]): Try[Int] = Try {
    val rules = createRules(inputLines)

    @tailrec
    def iterate(square: Square, counter: Int): Int = {
      if (counter == 0) {
        square.rows.flatten.count(_ == On)
      } else {
        val divideAndTransform = DividedSquare(
          square.divide.rows
            .map(row => row.map(sq =>
              rules.find(r => r.matches(sq)) match {
                case Some(rule) => rule.result
                case None => throw new IllegalArgumentException("cannot find match")
              })))

        iterate(Square.merge(divideAndTransform), counter - 1)
      }
    }

    iterate(initSquare, 5)
  }

  private def createRules(inputLines: Seq[String]): Seq[Rule] = {

    def createSquare(charLines: String*): Square = Square(charLines.map(l => l.map {
      case '.' => Off
      case '#' => On
    }.toVector).toVector)

    inputLines.map {
      case rule2(i1, i2, o1, o2, o3) => Rule(createSquare(i1, i2), createSquare(o1, o2, o3))
      case rule3(i1, i2, i3, o1, o2, o3, o4) => Rule(createSquare(i1, i2, i3), createSquare(o1, o2, o3, o4))
    }
  }


  private case class DividedSquare(rows: Vector[Vector[Square]]) {

    require({
      val colSize = rows.size
      rows.map(r => r.size == colSize).forall(identity)
    }, "invalid rows: cannot create DividedSquare")

    def size: Int = rows.map(r => r.map(sq => sq.size).sum).sum
  }

  private case class Square(rows: Vector[Vector[Field]]) {

    require({
      val colSize = rows.size
      (colSize % 3 == 0 || colSize % 2 == 0) && rows.map(r => r.size == colSize).forall(identity)
    }, "invalid rows: cannot create Square")

    private val div = if (size % 2 == 0) 2 else 3

    def size: Int = rows.size

    def divide: DividedSquare = {

      def divide(square: Square): Vector[Square] = {
        if (square.size == 2 || square.size == 3) {
          Vector(square)
        } else {
          (for (ri <- rows.indices by square.div; ci <- rows.indices by square.div)
            yield Square(rows.slice(ri, ri + square.div).map(r => r.slice(ci, ci + square.div))))
            .flatMap(sq => divide(sq))
            .toVector
        }
      }

      DividedSquare(divide(this).grouped(size / div).toVector)
    }

    def flipVertical: Square = Square(rows.reverse)

    def flipHorizontal: Square = Square(rows.map(r => r.reverse))

    def rotate: Square = Square((for (i <- rows.indices) yield rows.map(r => r(i)).reverse).toVector)
  }

  private object Square {
    def merge(squares: DividedSquare): Square = Square(
      squares.rows
        .flatMap(squaresLine => squaresLine
          .foldLeft(Vector.fill(squares.size)(Vector[Field]()))
          ((rows, sq) => (rows zip sq.rows).map(r => r._1 ++ r._2)))
    )
  }

  private case class Rule(pattern: Square, result: Square) {
    def matches(square: Square): Boolean = {

      def matchesTransformed(sq: Square, counter: Int): Boolean = {
        val currentMatches = sq == pattern || sq.flipHorizontal == pattern || sq.flipVertical == pattern
        if (counter == 0) {
          currentMatches
        } else {
          currentMatches || matchesTransformed(sq.rotate, counter - 1)
        }
      }

      matchesTransformed(square, 4)
    }
  }

  private trait Field
  private object On extends Field
  private object Off extends Field

}
