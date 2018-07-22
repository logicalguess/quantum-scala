package quantum.domain

import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import quantum.domain.Gate._
import quantum.domain.QState._
import quantum.domain.Labeled.Tensor
import quantum.domain.Symbol.Std

class TwoQbitSpec extends FlatSpec with GeneratorDrivenPropertyChecks {

  def crot(theta: Double)(s: QState[Tensor[Std, Std]]): QState[Tensor[Std, Std]] =
    s >>= lift1(rot(theta/2)) >>= cnot >>= lift1(rot(-theta/2)) >>= cnot

  "rot" should "s0" in {
    for (i <- 0 to 8)
      println(rot(math.Pi * i/4)(s0))
  }

  "rot" should "s1" in {
    for (i <- 0 to 8)
      println(rot(math.Pi * i/4)(s1))
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
