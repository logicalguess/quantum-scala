package quantum.algorithm

import org.scalatest.FlatSpec
import quantum.domain.Gate.{H, I, Y, Z, rot}
import quantum.domain.Symbol.Std
import quantum.domain.{Complex, Gate, QState}

class AmplitudeSpec extends FlatSpec {

  "bernoulli" should "circuit" in {

    val p = 0.3
    val theta = math.asin(math.sqrt(p))

    val op: Gate[Std, Std] = rot(theta)

    def q(j: Int): Gate[Std, Std] = rot(math.pow(2, j + 1) * theta)

    val n = 4
    val circuit = Amplitude.run(n, op, q)

    circuit.probs
    circuit.hist

    val estimates = Amplitude.estimate(circuit)
    println(estimates)
  }

  "count" should "circuit" in {

    val op: Gate[Std, Std] = Y * Complex.i // -HZHZ
    //val op: Gate[Std, Std] = (s: Std) => QState.pure(s) >>= H >=> Z >>= H >>= Z

    def q(j: Int): Gate[Std, Std] = if (j % 2 == 0) I[Std] else op

    val n = 5
    val circuit = Amplitude.run(n, op, q)

    circuit.probs
    circuit.hist

    val estimates = Amplitude.estimate(circuit, true)
    println(estimates)
  }
}
