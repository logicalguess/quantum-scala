package quantum.domain

import quantum.domain.QState._
import quantum.domain.Labeled._
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
  def I[B <: Labeled](b: B): QState[B] = pure(b)

  // Not gate
  val X: Gate[Std, Std] = (s1 >< s0) + (s0 >< s1)

  // Phase flip gate
  val Z: Gate[Std, Std] = (s0 >< s0) + (-s1 >< s1)

  // Hadamard gate
  val H: Gate[Std, Std] = (plus >< s0) + (minus >< s1)

  def controlled[B <: Labeled](g: B => QState[B]): Tensor[Std, B] => QState[Tensor[Std, B]] = s => s match {
    case Tensor(S0, b) => pure(Tensor(S0, b))
    case Tensor(S1, b) => s1 * g(b)
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

  // Implementation of f as a quantum gate
  def U(f: Int => Int): Tensor[Word[Std], Word[Std]] => QState[Tensor[Word[Std], Word[Std]]] = s => {
    val Tensor(x, out) = s
    val fx = Symbol.fromInt(f(Symbol.toInt(x)) ^ Symbol.toInt(out), out.letters.length)
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
}
