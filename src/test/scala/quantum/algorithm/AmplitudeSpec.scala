package quantum.algorithm

import org.scalatest.FlatSpec
import quantum.domain.Gate.{H, I, Y, Z, controlledL, rot, wire, _}
import quantum.domain.QState.{pure, s0, s1, _}
import quantum.domain.Symbol.{Std, Word}
import quantum.domain.{Complex, Gate, QState}

class AmplitudeSpec extends FlatSpec {

  "bernoulli" should "circuit" in {

    val p = 0.3
    val theta = math.asin(math.sqrt(p))

    val op: Gate[Std, Std] = rot(theta)

    def q(j: Int): Gate[Std, Std] = rot(math.pow(2, j + 1) * theta)

    val n = 4
    val circuit = Amplitude.run(n, op, q _)

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
    val circuit = Amplitude.run(n, op, q _)

    circuit.probs
    circuit.hist

    val estimates = Amplitude.estimate(circuit, true)
    println(estimates)
  }


  val zg: Gate[Std, Std] = (s0 >< s0) + (s0 >< s1)

  def fib(n: Int): QState[Word[Std]] = {
    var state = pure(Word.fromInt(0, n))
    for (i <- 0 until n) state = state >>= wire(i, H)
    for (i <- 0 until n - 1)  state = state >>= controlledL(Set(i), i + 1, zg)
    state
  }

  "count" should "fib" in {

    val op: Gate[Std, Std] = Y * Complex.i // -HZHZ
    //val op: Gate[Std, Std] = (s: Std) => QState.pure(s) >>= H >=> Z >>= H >>= Z

    def q(j: Int): Gate[Std, Std] = if (j % 2 == 0) I[Std] else op

    for (i <- 1 to 5) {

      val init = fib(i)
      println(s"Fib(${i + 1}) = ${init.state.size}")
      val circuit = Amplitude.run(op, q _, init)

      val estimates = Amplitude.estimate(circuit, true)
      println(estimates)
    }
  }
}
