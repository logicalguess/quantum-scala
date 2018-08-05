package quantum.algorithm

import org.scalatest.FlatSpec
import quantum.algorithm.Grover._
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

  "bernoulli1" should "circuit" in {

    val p = 0.3
    val theta = math.asin(math.sqrt(p))

    val op: Gate[Std, Std] = rot(theta)

    def q(j: Int): Gate[Std, Std] = rot(math.pow(2, j + 1) * theta)

    val n_targets = 3

    var state = pure(Word.fromInt(0, n_targets + 1))
    state = state >>= wire(n_targets, op)
    for (j <- (0 until n_targets)) {
      state = state >>= wire(j, H)
    }
    state = state >>= Amplitude.lambdaL(List(0, 1, 2), 3)(q) // Amplitude.lambda(q) //
    state = state >>= QFT.iqftL(List(0, 1, 2)) // Amplitude.iqft //

    state.probs
    state.hist

    val result = state.measure(w => Word.toInt(Word(w.letters.take(3)))).outcome
    println(result)
    println(math.pow(math.sin(result/math.pow(2, 3)), 2))

    val estimates = Amplitude.estimate(state)
    println(estimates)
  }

  "count" should "all states" in {

    val op: Gate[Std, Std] = I

    def f(x: Int) = true
    //def f(x: Int) = x <= 3
    //def f(x: Int) = x <= 3
    //def f(n: Int): Boolean = (n & (n >>> 1)) == 0


    def gf(shift: Int) =
      wire(3 + shift, Z) _ >=> oracleL(f)(List(0, 1, 2).map { i => i + shift }, 3 + shift) >=> wire(3 + shift, Z) >=> invL((List(0, 1, 2) ++ List(3)).map { i => i + shift })

    val n_controls = 3
    val g = gf(n_controls)

    var state = pure(Word.fromInt(0, n_controls + 4))

    //state = state >>= wire(n_controls + 3, op)

    for (j <- (0 to n_controls + 3)) {
      state = state >>= wire(j, H)
    }

    for (i <- 0 until n_controls) {
      for (j <- 1 to math.pow(2, i).toInt)
        state = state >>= controlledI(i, g)
    }

    state = state >>= QFT.iqftL((0 until n_controls).toList)

    state.hist

    val result = state.measure(w => Word.toInt(Word(w.letters.take(n_controls)))).outcome
    println(result)
    println(math.pow(2, 3)*math.pow(math.sin(math.Pi * result/math.pow(2, n_controls)), 2))
  }
}
