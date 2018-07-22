package quantum.domain

import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import quantum.domain.Gate._
import quantum.domain.QState._
import quantum.domain.Labeled.Tensor
import quantum.domain.Symbol.{S0, S1, Std, Word}

class TwoQbitSpec extends FlatSpec with GeneratorDrivenPropertyChecks {

  def crot(theta: Double)(s: QState[Tensor[Std, Std]]): QState[Tensor[Std, Std]] =
    s >>= lift2(rot(theta/2)) >>= cnot >>= lift2(rot(-theta/2)) >>= cnot

  "rot" should "s0" in {
    for (i <- 0 to 8)
      println(rot(math.Pi * i/4)(s0))
  }

  "rot" should "s1" in {
    for (i <- 0 to 8)
      println(rot(math.Pi * i/4)(s1))
  }

  "prob" should "rot" in {
    val p = 0.3
    val theta = math.asin(math.sqrt(p))

    val t: QState[Std] = rot(theta)(s0)
    println(t)
    println(t(S0)*t(S0))
    println(t(S1)*t(S1))

    t.hist
  }

  "prob" should "crot" in {
    val p = 0.3
    val theta = math.asin(math.sqrt(p))

    val t: QState[Tensor[Std, Std]] = crot(theta)(s0*s0)

    println(t)
    t.hist
  }

  "prob" should "circuit" in {
    val p = 0.3
    val theta = math.asin(math.sqrt(p))

    //val s: QState[Tensor[Std, Std]] = s0*s0 >>= lift1(H) >>= lift2(rot(theta))
    val s: QState[Tensor[Std, Std]] = s0*s0 >>= lift12(H, rot(theta))

    val t: QState[Tensor[Std, Std]] = crot(2*theta)(s)

    println(t)
    t.probs
    t.hist
  }

  "prob" should "2 qbits" in {
    val p = 0.3
    val theta = math.asin(math.sqrt(p))

    val Hn = liftWord(H) _

    val ancillas = pure(Word.fromInt(0, 1))
    val target = pure(Word.fromInt(0, 1))

    val t = ancillas * target >>= lift1(Hn) >>= lift2(liftWord(rot(theta)))

    println(t)
    t.probs
    t.hist
  }

  "crot" should "s0*s0" in {
    for (i <- 0 to 8)
      println(crot(math.Pi * i/4)(s0*s0))
  }

  "crot" should "s0*s1" in {
    for (i <- 0 to 8)
      println(crot(math.Pi * i/4)(s0*s1))
  }
}
