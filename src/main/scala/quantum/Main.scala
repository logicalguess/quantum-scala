package quantum

import quantum.Complex._
import quantum.QState._
import quantum.Symbol._
import quantum.Gate._

import scala.language.reflectiveCalls

object Main {

  def HZH(s: QState[Std]): QState[Std] = s >>= H >>= Z >>= H
  def runHZHequalsX(s: QState[Std]): (QState[Std], QState[Std]) = (HZH(s), s >>= X)

  // Some convenient states for testing
  val state1: QState[Std] = QState(S0 -> 0.6, S1 -> 0.8.i)
  val state2: QState[Std] = QState(S0 -> -0.5, S1 -> r3quarters)

  def iterate[A](n: Int, a: A)(f: A => A): A = {
    if (n <= 0) a
    else iterate(n-1, f(a))(f)
  }

  /**
   * Grover's algorithm
   */
  def grover(f: Int => Int, width: Int) = {
    val Hn = liftWord(H) _
    val zeroes = pure(Symbol.fromInt(0, width))
    val one = pure(Symbol.fromInt(1, 1))
    val inv = U(f)
    val refl = {
      val s = zeroes >>= Hn
      (s >< s) * 2 - I
    }

    val r = (math.Pi * math.sqrt(math.pow(2, width)) / 4).toInt
    // zeroes * one >>= lift12(Hn, Hn) >>= repeat(r)(inv >=> lift1(refl))
    val init = zeroes * one >>= lift12(Hn, Hn)
    iterate(r, init)(_ >>= (inv >=> lift1(refl)))
  }

  def runGrover(n: Int) = {
    def f(x: Int) = if (x == n) 1 else 0
    val bits = (math.log(n) / math.log(2)).toInt + 1
    val s = grover(f, bits)
    println("final state: " + s.toString)
    val m = Symbol.toInt(s.measure(_._1).outcome)
    println("measurement: " + m)
  }



  def main(args: Array[String]): Unit = {
    println(runHZHequalsX(state1))
    println(runHZHequalsX(state2))

    runGrover(3)
  }
}
