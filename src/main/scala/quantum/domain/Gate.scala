package quantum.domain

import quantum.domain.Labeled._
import quantum.domain.QState._
import quantum.domain.Symbol._

final class Gate[A, B <: Labeled](val f: A => QState[B]) {
  def +(g: A => QState[B]): A => QState[B] = (a: A) => f(a) + g(a)
  def -(g: A => QState[B]): A => QState[B] = (a: A) => f(a) - g(a)
  def *(z: Complex): A => QState[B] = (a: A) => f(a) * z
  def >=>[C <: Labeled](g: B => QState[C]): A => QState[C] = (a: A) => f(a) >>= g

  def apply(s: QState[A with Labeled]) = s.flatMap(f)
}

object Gate {

  implicit def functionToGate[A, B <: Labeled](f: A => QState[B]): Gate[A, B] = new Gate(f)
  implicit def gateToFunction[A, B <: Labeled](op: Gate[A, B]): A => QState[B] = op.f
  def repeat[A <: Labeled](n: Int)(f: A => QState[A]): A => QState[A] = (a: A) => {
    @scala.annotation.tailrec
    def helper(n: Int, acc: QState[A]): QState[A] = {
      if (n <= 0) acc
      else helper(n-1, acc >>= f)
    }
    helper(n, QState.pure(a))
  }

  // Identity gate
  //def I[B <: Labeled](b: B): QState[B] = pure(b)
  def I[B <: Labeled]: Gate[B, B] = {s:B => pure(s)}


  // Not gate
  val X: Gate[Std, Std] = (s1 >< s0) + (s0 >< s1)

  val Y: Gate[Std, Std] = (s1 * Complex.i >< s0) + (-s0 * Complex.i >< s1)

  // Phase flip gate
  val Z: Gate[Std, Std] = (s0 >< s0) + (-s1 >< s1)

  // Hadamard gate
  val H: Gate[Std, Std] = (plus >< s0) + (minus >< s1)

  def controlled[B <: Labeled](g: B => QState[B]): Tensor[Std, B] => QState[Tensor[Std, B]] = s => s match {
    case Tensor(S0, b) => pure(Tensor(S0, b))
    case Tensor(S1, b) => s1 * g(b)
  }

  def controlled1[B <: Labeled](g: B => QState[B]): Tensor[Std, B] => QState[Tensor[Std, B]] = s => s._1 match {
    case S0 => pure(Tensor(S0, s._2))
    case S1 => s1 * g(s._2)
  }

  // Controlled not (CNOT) gate
  val cnot: Gate[Tensor[Std, Std], Tensor[Std, Std]] = controlled(X)

  def R(theta: Double): Gate[Std, Std] = (s0 >< s0) + (s1 * Complex.one.rot(theta) >< s1)

  // Rotation gate
  val tau = 2 * math.Pi
  def rot(theta: Double): Gate[Std, Std] = {
    val s0a = s0 * math.cos(theta) + s1 * math.sin(theta)
    val s1a = s0 * -math.sin(theta) + s1 * math.cos(theta)
    (s0a >< s0) + (s1a >< s1)
  }

  // Square root of NOT gate
  val sqrtNot: Gate[Std, Std] = rot(tau/8)

  def Rx(theta: Double): Gate[Std, Std] = I[Std] * math.cos(theta / 2) - X * Complex.i * math.sin(theta / 2)

  def Ry(theta: Double): Gate[Std, Std] = I[Std] * math.cos(theta / 2) - Y * Complex.i * math.sin(theta / 2)

  def Rz(theta: Double): Gate[Std, Std] = I[Std] * math.cos(theta / 2) - Z * Complex.i * math.sin(theta / 2)

  // Implementation of f as a quantum gate
  def U(f: Int => Int): Tensor[Word[Std], Word[Std]] => QState[Tensor[Word[Std], Word[Std]]] = s => {
    val Tensor(x, out) = s
    val fx = Word.fromInt(f(Word.toInt(x)) ^ Word.toInt(out), out.letters.length)
    pure(x) * pure(fx)
  }


  /**
   * Wire manipulation gates
   */

  // Lift 2 gates into a tensor product
  def lift12[B1 <: Labeled, B1a <: Labeled, B2 <: Labeled, B2a <: Labeled](t1: B1 => QState[B1a], t2: B2 => QState[B2a])(s: Tensor[B1, B2]): QState[Tensor[B1a, B2a]] = {
    t1(s._1) * t2(s._2)
  }

  // Lift a gate into the left side of a tensor product
  def lift1[B1 <: Labeled, B1a <: Labeled, B2 <: Labeled](t1: B1 => QState[B1a])(s: Tensor[B1, B2]): QState[Tensor[B1a, B2]] = {
    t1(s._1) * pure(s._2)
  }

  // Lift a gate into the right side of a tensor product
  def lift2[B1 <: Labeled, B2 <: Labeled, B2a <: Labeled](t2: B2 => QState[B2a])(s: Tensor[B1, B2]): QState[Tensor[B1, B2a]] = {
    pure(s._1) * t2(s._2)
  }

  // Word manipulation
  def liftWord[B1 <: Symbol, B2 <: Symbol](t: B1 => QState[B2])(s: Word[B1]): QState[Word[B2]] = {
    s match {
      case Word(Nil) => pure(Word(Nil))
      case Word(h :: rest) => t(h) *: liftWord(t)(Word(rest))
    }
  }

  def cons[B <: Symbol](s: Tensor[B, Word[B]]): QState[Word[B]] = s match {
    case Tensor(h, Word(t)) => pure(Word(h :: t))
  }

  def decons[B <: Symbol](s: Word[B]): QState[Tensor[B, Word[B]]] = s match {
    case Word(Nil) => QState[Tensor[B, Word[B]]]()
    case Word(h :: t) => pure(Tensor(h, Word(t)))
  }

  // Re-associate a nested tensor product
  def assoc1[B1 <: Labeled, B2 <: Labeled, B3 <: Labeled](b: Tensor[B1, Tensor[B2, B3]]): QState[Tensor[Tensor[B1, B2], B3]] = {
    b match { case Tensor(b1, Tensor(b2, b3)) => pure(Tensor(Tensor(b1, b2), b3)) }
  }

  // Re-associate a nested tensor product the other way
  def assoc2[B1 <: Labeled, B2 <: Labeled, B3 <: Labeled](b: Tensor[Tensor[B1, B2], B3]): QState[Tensor[B1, Tensor[B2, B3]]] = {
    b match { case Tensor(Tensor(b1, b2), b3) => pure(Tensor(b1, Tensor(b2, b3))) }
  }

  // Swap the two sides of tensor product
  def swap[B1 <: Labeled, B2 <: Labeled](b: Tensor[B1, B2]): QState[Tensor[B2, B1]] = {
    b match { case Tensor(b1, b2) => pure(Tensor(b2, b1)) }
  }

  def reverse[B <: Symbol](s: Word[B]): QState[Word[B]] = {
    pure(Word(s.letters.reverse))
  }

  def wire(t: Int, g: Std => QState[Std])(s: Word[Std]): QState[Word[Std]] = {
    s match {
      case Word(Nil) => pure(Word(Nil))
      case Word(h :: rest) if t == 0 => g(h) *: pure(Word(rest))
      case Word(h :: rest) => pure(h) *: wire(t - 1, g)(Word(rest))
    }
  }

  def controlledW1(c: Int, t: Int, g: Std => QState[Std])(s: Word[Std]): QState[Word[Std]] = {
    def helper(c: Int, t: Int, g: Std => QState[Std])(s: Word[Std]): QState[Word[Std]] = {
      s match {
        case Word(Nil) => pure(Word(Nil))
        case _ if t <= c => throw new Error("control has to be less than target")
        case Word(S0 :: rest) if c == 0 => pure(Word(S0 :: rest))
        case Word(S1 :: rest) if c == 0 => s1 *: wire(t - 1, g)(Word(rest))
        case Word(h :: rest) => pure(h) *: helper(c - 1, t - 1, g)(Word(rest))
      }
    }

    t - c match {
      case diff if diff > 0 => helper(c, t, g)(s)
      case diff if diff < 0 => {
        val size = s.letters.size
        helper(size - 1 - c, size - 1 - t, g)(Word(s.letters.reverse)) >>= reverse _
      }
      case _ => throw new Error("control and target have to be different")
    }
  }

  def controlledW(c: Int, t: Int, g: Std => QState[Std])(s: Word[Std]): QState[Word[Std]] = {
    controlledL(Set(c), t, g)(s)
  }

  def controlledL(c: Set[Int], t: Int, g: Std => QState[Std])(s: Word[Std]): QState[Word[Std]] = {
    s match {
      case Word(Nil) => pure(Word(Nil))
      case w if c.isEmpty => wire(t, g)(w)
      case _ if c.contains(t) => throw new Error("target cannot be in the control set")
      case _ if t == 0 => {
        val size = s.letters.size
        controlledL(c.map { i => size - 1 - i }, size - 1 - t, g)(Word(s.letters.reverse)) >>= reverse _
      }
      case Word(S0 :: rest) if c.contains(0) => pure(Word(S0 :: rest))
      case Word(S1 :: rest) if c.contains(0) => s1 *: controlledL((c - 0).map { i => i - 1 }, t - 1, g)(Word(rest))
      case Word(h :: rest) if !c.contains(0) => pure(h) *: controlledL(c.map { i => i - 1 }, t - 1, g)(Word(rest))
    }
  }
}
