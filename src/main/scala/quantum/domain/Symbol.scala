package quantum.domain

abstract class Symbol extends Labeled

object Symbol {

  // Standard { |0>, |1> }
  abstract sealed class Std(val label: String) extends Symbol

  case object S0 extends Std("0")

  case object S1 extends Std("1")

  // Sign { |+>, |-> }
  abstract sealed class Sign(val label: String) extends Symbol

  case object S_+ extends Sign("+")

  case object S_- extends Sign("-")

  // H-V polarization
  abstract sealed class Polarization(val label: String) extends Symbol

  case object Horizontal extends Polarization("H")

  case object Vertical extends Polarization("V")

  case class Word[B <: Symbol](letters: List[B]) extends Labeled {
    val label = letters.map(_.label).mkString
  }

  object Word {
    def fromInt(i: Int, width: Int): Word[Std] = {
      def helper(i: Int, width: Int, acc: List[Std]): List[Std] = {
        if (width == 0) acc
        else helper(i / 2, width - 1, (if (i % 2 == 0) S0 else S1) :: acc)
      }

      Word(helper(i, width, Nil))
    }

    def toInt(s: Word[Std]): Int = {
      def helper(ls: List[Std], acc: Int): Int = {
        ls match {
          case Nil => acc
          case S0 :: rest => helper(rest, acc * 2)
          case S1 :: rest => helper(rest, acc * 2 + 1)
        }
      }

      helper(s.letters, 0)
    }

    def tailInt(s: Word[Std]): Int = toInt(Word(s.letters.tail))
  }
}





