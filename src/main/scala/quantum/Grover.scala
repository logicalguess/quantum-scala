package quantum

import quantum.QState._
import quantum.Gate._

import scala.language.reflectiveCalls

object Grover {

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

  def run(n: Int): Int = {
    def f(x: Int) = if (x == n) 1 else 0
    val bits = (math.log(n) / math.log(2)).toInt + 1
    val s = grover(f, bits)
    println("final state: " + s.toString)
    val m = Symbol.toInt(s.measure(_._1).outcome)
    println("measurement: " + m)
    m
  }
}