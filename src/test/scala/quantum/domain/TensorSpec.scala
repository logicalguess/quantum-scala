package quantum.domain

import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import quantum.domain.Gate.{lift2, _}
import quantum.domain.Labeled.Tensor
import quantum.domain.QState._
import quantum.domain.Symbol.{S0, S1, Std, Word}

class TensorSpec extends FlatSpec with GeneratorDrivenPropertyChecks {

  def crot(theta: Double)(s: QState[Tensor[Std, Std]]): QState[Tensor[Std, Std]] =
    s >>= lift2(rot(theta/2)) >>= cnot >>= lift2(rot(-theta/2)) >>= cnot

  "tensor" should "s0*s0" in {
    val init: QState[Tensor[Std, Std]] = s0 * s0

    assert(init(Tensor(S0, S0)) == Complex.one)
    assert(init(Tensor(S0, S1)) == Complex.zero)
    assert(init(Tensor(S1, S0)) == Complex.zero)
    assert(init(Tensor(S1, S1)) == Complex.zero)

    val s = init >>= lift12(H, H)
    s.probs
  }

  "tensor" should "s0*s0*s0" in {
    val init: QState[Tensor[Tensor[Std, Std], Std]] = s0 * s0 * s0

    val s1 = init >>= lift12(lift12(H, H), H)
    s1.probs

    val s2 = init >>= assoc2 >>= lift12(H, lift12(H, H))
    s2.probs


    val s3 = init >>= assoc2 >>= lift12(H, lift12(H, rot(math.Pi)))
    s3.probs
  }

  "tensor" should "controlled s1*s0*s0" in {
    val init: QState[Tensor[Tensor[Std, Std], Std]] = s1 * s0 * s0

    val c1: QState[Tensor[Tensor[Std, Std], Std]] = init >>=
      lift1(controlled(rot(math.Pi/2)))

    c1.probs

    println()

    val c2: QState[Tensor[Std, Tensor[Std, Std]]] = init >>=
      assoc2 >>=
      lift2(controlled(rot(math.Pi/2)))

    c2.probs
  }

  "tensor" should "controlled s0*s1*s0" in {
    val init: QState[Tensor[Tensor[Std, Std], Std]] = s0 * s1 * s0

    val c1: QState[Tensor[Tensor[Std, Std], Std]] = init >>=
      lift1(controlled(rot(math.Pi/2)))

    c1.probs

    println()

    val c2: QState[Tensor[Std, Tensor[Std, Std]]] = init >>=
      assoc2 >>=
      lift2(controlled(rot(math.Pi/2)))

    c2.probs
  }

  "tensor" should "match" in {
    val t: QState[Tensor[Tensor[Std, Std], Std]] = s1 * s0 * s0 >>= lift12(lift12(H, H), H)
    println(t)

    val one = pure(Word.fromInt(0, 1))
    val zeroes = pure(Word.fromInt(0, 2))

    val Hn = liftWord(H) _

    val s = one * zeroes >>= lift12(Hn, Hn)
    println(s)
  }
}
