package quantum.domain

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import quantum.domain.Complex._
import quantum.domain.Gate._
import quantum.domain.QState._
import quantum.domain.Symbol._

import scala.language.reflectiveCalls

class SingleQbitSpec extends FlatSpec with GeneratorDrivenPropertyChecks {

  val state1: QState[Std] = QState(S0 -> 0.6, S1 -> 0.8.i)
  val state2: QState[Std] = QState(S0 -> -0.5, S1 -> r3quarters)

  implicit val qbits: Arbitrary[QState[Std]] = Arbitrary {
    for {
      re0 <- Gen.choose[Double](-99, 99)
      im0 <- Gen.choose[Double](-99, 99)
      re1 <- Gen.choose[Double](-99, 99)
      im1 <- Gen.choose[Double](-99, 99)
    } yield QState(S0 -> (re0 + im0.i), S1 -> (re1 + im1.i))
  }

  implicit val thetaAndState: Arbitrary[(Double, QState[Std])] = Arbitrary {
    for {
      theta <- Gen.choose[Double](0.0, math.Pi)

      re0 <- Gen.choose[Double](-99, 99)
      im0 <- Gen.choose[Double](-99, 99)
      re1 <- Gen.choose[Double](-99, 99)
      im1 <- Gen.choose[Double](-99, 99)
    } yield (theta, QState(S0 -> (re0 + im0.i), S1 -> (re1 + im1.i)))
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

  "Y" should "swap the amplitudes of |0> and |1>, multiply each amplitude by i, and negate the amplitude of |1>" in forAll { s: QState[Std] =>
    val t: QState[Std] = Y(s)

    assert(t(S0) == - s(S1) * Complex.i)
    assert(t(S1) == s(S0) * Complex.i)
  }

  "Z" should "negate the amplitude of |1>, leaving the amplitude of |0> unchanged" in forAll { s: QState[Std] =>
    val t: QState[Std] = Z(s)

    assert(t(S0) == s(S0))
    assert(t(S1) == -s(S1))
  }

  "Z" should "should equal R(pi)" in forAll { s: QState[Std] =>

    val t: QState[Std] = Z(s)
    val r: QState[Std] = R(math.Pi)(s)

    assert(t(S0) == r(S0))
    assert(t(S1).toString == r(S1).toString)
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

  "Rz(theta)" should "rotate the amplitude of |0> by -theta/2 and the amplitude of |1> by theta/2" in forAll { ts: (Double, QState[Std]) =>
    val theta = ts._1
    val state = ts._2

    val z: QState[Std] = Rz(theta)(state)

    // Rz rotates the amplitude of |0> by -theta/2
    assert(z(S0) == state(S0) * Complex.one.rot(-theta / 2))
    // Rz rotates the amplitude of |1> by theta/2
    assert(z(S1) == state(S1) * Complex.one.rot(theta / 2))
  }

  "R(theta)" should "change the amplitude of |0> and rotate the amplitude of |1> by theta" in forAll { ts: (Double, QState[Std]) =>
    val theta = ts._1
    val state = ts._2

    val r: QState[Std] = R(theta)(state)

    // R doesn't change the amplitude of |0>
    assert(r(S0) == state(S0))
    // R rotates the amplitude of |1> by theta/
    assert(r(S1) == state(S1) * Complex.one.rot(theta))
  }

  "Rz(theta)" should "be \"similar\" to R(theta/2)" in forAll { ts: (Double, QState[Std]) =>
    val theta = ts._1
    val state = ts._2

    val z: QState[Std] = Rz(theta)(state)
    val r: QState[Std] = R(theta/2)(state)

    // R doesn't change the amplitude of |0>
    assert(r(S0) == state(S0))
    // Rz rotates the amplitude of |0> by -theta/2
    assert(z(S0) == state(S0) * Complex.one.rot(-theta/2))
    // both Rz and R rotate the amplitude of |1> by theta/2
    assert(z(S1) == state(S1) * Complex.one.rot(theta/2))
    assert(r(S1) == state(S1) * Complex.one.rot(theta/2))

    // amplitudes of |0> have same norm and differ by a phase of theta/2
    assert((z(S0) * Complex.one.rot(theta/2) - r(S0) ).norm2 < 0.00000000001)
    assert((z(S0) - r(S0) * Complex.one.rot(-theta/2)).norm2 < 0.00000000001)
    assert(math.abs(z(S0).norm2 - r(S0).norm2) < 0.00000000001)

    // amplitudes of |1> are equal (phase shift by theta/2)
    assert(z(S1) == r(S1))

  }

  "Ry(theta)" should "equal rot(theta/2)" in forAll { ts: (Double, QState[Std]) =>
    val theta = ts._1
    val state = ts._2

    val y: QState[Std] = Ry(theta)(state)
    val r: QState[Std] = rot(theta/2)(state)

    assert(y(S0) == r(S0))
    assert(y(S1) == r(S1))
  }

  "Ry(theta)" should "mix the amplitudes of |0> and |1> (like vector rotation)" in forAll { ts: (Double, QState[Std]) =>
    val theta = ts._1
    val state = ts._2

    val y: QState[Std] = Ry(theta)(state)

    // same formula as 2-dimensional vector rotation (but with half angle)
    val t0 = state(S0) * math.cos(theta/2) - state(S1) * math.sin(theta/2)
    val t1 = state(S0) * math.sin(theta/2) + state(S1) * math.cos(theta/2)

    assert(y(S0) == t0)
    assert(y(S1) == t1)
  }

  "Rz(pi)" should "equal -i*Z" in forAll { s: QState[Std] =>

    val t: QState[Std] = (Z * -Complex.i) (s)
    val r: QState[Std] = Rz(math.Pi)(s)

    assert(t(S0).toString == r(S0).toString)
    assert(t(S1).toString == r(S1).toString)
  }

  "Z" should "equal i*Rz(pi)" in forAll { s: QState[Std] =>

    val t: QState[Std] = Z(s)
    val r: QState[Std] = (Rz(math.Pi) * Complex.i)(s)

    assert(t(S0).toString == r(S0).toString)
    assert(t(S1).toString == r(S1).toString)
  }

  "Y" should "equal i*Ry(pi)" in forAll { s: QState[Std] =>

    val t: QState[Std] = Y(s)
    val r: QState[Std] = (Ry(math.Pi) * Complex.i)(s)

    assert(t(S0).toString == r(S0).toString)
    assert(t(S1).toString == r(S1).toString)
  }

  "X" should "equal i*Rx(pi)" in forAll { s: QState[Std] =>

    val t: QState[Std] = X(s)
    val r: QState[Std] = (Rx(math.Pi) * Complex.i)(s)

    assert(t(S0).toString == r(S0).toString)
    assert(t(S1).toString == r(S1).toString)
  }
}
