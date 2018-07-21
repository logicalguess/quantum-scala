package quantum.domain

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import quantum.domain
import quantum.domain.Complex._
import quantum.domain.Gate._
import quantum.domain.QState._
import quantum.domain.Symbol._

import scala.language.reflectiveCalls

class SingleQbitSpec extends FlatSpec with GeneratorDrivenPropertyChecks {

  val state1: QState[Std] = domain.QState(S0 -> 0.6, S1 -> 0.8.i)
  val state2: QState[Std] = domain.QState(S0 -> -0.5, S1 -> r3quarters)

  implicit val qbits: Arbitrary[QState[Std]] = Arbitrary {
    for {
      re0 <- Gen.choose[Double](-99, 99)
      im0 <- Gen.choose[Double](-99, 99)
      re1 <- Gen.choose[Double](-99, 99)
      im1 <- Gen.choose[Double](-99, 99)
    } yield QState(S0 -> (re0 + im0.i), S1 -> (re1 + im1.i))
  }

  "Probabilities of final states" should "equal the squares of amplitudes' lengths" in forAll { s: QState[Std] =>
    println(s)
    s.hist
  }

  "I|0>" should "be |0>" in {
    val s = I[Std](S0)

    println(s)
    s.hist

    assert(s(S0) == Complex.one)
    assert(s(S1) == Complex.zero)
  }

  "I|1>" should "be |1>" in {
    val s = I[Std](S1)

    println(s)
    s.hist

    assert(s(S0) == Complex.zero)
    assert(s(S1) == Complex.one)
  }

  "H|0>" should "be a multiple of |0> + |1>" in {
    val s = H(S0)

    println(s)
    s.hist

    assert(s(S0) == s(S1))
  }

  "H|1>" should "be a multiple of |0> - |1>" in {
    val s = H(S1)

    println(s)
    s.hist

    assert(s(S0) == -s(S1))
  }

  "The Identity gate" should "preserve amplitudes" in forAll { s: QState[Std] =>
    println(s)
    assert((s >>= I[Std]) (S0) == s(S0))
    assert((s >>= I[Std]) (S1) == s(S1))
  }

  "The NOT gate" should "swap amplitudes" in forAll { s: QState[Std] =>
    println(s)
    assert((s >>= X) (S0) == s(S1))
    assert((s >>= X) (S1) == s(S0))
  }

  "X" should "swap the amplitudes of |0> and |1>" in forAll { s: QState[Std] =>
    val t: QState[Std] = X(s)

    assert(t(S0) == s(S1))
    assert(t(S1) == s(S0))
  }

  "Y" should "swap the amplitudes of |0> abd |1>, multiply each amplitude by i, and negate the amplitude of |1>" in forAll { s: QState[Std] =>
    val t: QState[Std] = Y(s)

    assert(t(S0) == - s(S1) * Complex.i)
    assert(t(S1) == s(S0) * Complex.i)
  }

  "Z" should "negate the amplitude of |1>, leaving the amplitude of |0> unchanged" in forAll { s: QState[Std] =>
    val t: QState[Std] = Z(s)

    assert(t(S0) == s(S0))
    assert(t(S1) == -s(S1))
  }

  def HZH(s: QState[Std]): QState[Std] = s >>= H >>= Z >>= H

  "HZH = X" should "be true for state1 and state2" in {
    //val HZH = H >=> Z >=> H

    assert(HZH(state1) == X(state1))
    assert(HZH(state2) == X(state2))
  }

  "HZH = X" should "be true for any state" in forAll { s: QState[Std] =>
    println(s)
    assert(HZH(s) == X(s))
  }
}
