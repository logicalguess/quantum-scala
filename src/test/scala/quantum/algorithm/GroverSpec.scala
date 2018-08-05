package quantum.algorithm

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import quantum.algorithm.Grover._
import quantum.domain.Gate._
import quantum.domain.QState._
import quantum.domain.Symbol.{S0, S1, Std, Word}
import quantum.domain.{Gate, Labeled, QState}

class GroverSpec extends FlatSpec with GeneratorDrivenPropertyChecks {

  val report: QState[_ <: Labeled] => Unit = s => {
    println("Iteration state: " + s)
    s.hist
  }

  "Grover algorithm" should "find 5" in {
    assert(Grover.run(5)(report) == 5)
  }

  "oracle" should "" in {
    def f(x: Int) = x == 5
    val in = 5

    println(oracle(f)(Word.fromInt(in, 3)))
  }

  "inv" should "compare" in {

    for (i <- 1 to 100) {
      val bits = (math.log(i) / math.log(2)).toInt + 1
      val w = Word.fromInt(i, bits)

      val Hn: Gate[Word[Std], Word[Std]] = liftWord(H) _
      val state = pure(w)

      def refl(width: Int) = {
        val s = pure(Word.fromInt(0, width)) >>= Hn
        (s >< s) * 2.0 - I[Word[Std]]
      }
      assert(inv(w) == (pure(w) >>= refl(bits)))
    }
  }

  "oracle" should "compare" in {
    def f(x: Int) = x == 5
    def g(x: Int) = if (x == 5) 1 else 0

    for (i <- 1 to 100) {
      val bits = (math.log(i) / math.log(2)).toInt + 1
      val w = Word.fromInt(i, bits)

      val state = pure(w)
      val one = pure(Word.fromInt(1, 1))

      //assert(Amplitude.oracle(f)(Word(w.letters ++ List(S1))) == (pure(w) * one >>= U(g)))
      println(oracle(f)(Word(w.letters ++ List(S1))))
      println(pure(w) * one >>= U(g))

      println()
    }

  }

  "grover" should "find 5" in {

    def f(x: Int) = x == 5

    val s = grover(f)(3)
    println(s)
    val m = Word.toInt(s.measure().outcome)
    println(m/2)

  }

  "grover steps" should "find 5 compare" in {

    def f(x: Int) = x == 5

    var state = pure(Word[Std](List.fill(3)(S0) ++ List(S1)))
    state = state >>= wire(0, H) >>= wire(1, H) >>= wire(2, H) >>= wire(3, H)
    state = state >>= oracle(f) >>= inv

    println(state)
    state.hist

    def g(x: Int) = if (x == 5) 1 else 0
    val Hn: Gate[Word[Std], Word[Std]] = liftWord(H) _

    def refl(width: Int) = {
      val s = pure(Word.fromInt(0, width)) >>= Hn
      (s >< s) * 2.0 - I[Word[Std]]
    }

    val w = Word.fromInt(0, 3)
    val one = pure(Word.fromInt(1, 1))

    var s = pure(w) * one >>= lift12(Hn, Hn)
    s = s >>= U(g) >>= lift1(refl(3))
    println(s)
    s.hist


  }

  implicit val searchValues: Arbitrary[Int] = Arbitrary {
    for {
      v <- Gen.choose[Int](1, 99)
    } yield v
  }

  "Grover algorithm" should "find any value" in forAll { v: Int =>
    println("input: " + v)
    assert(Grover.run(v) == v)
  }

  "grover" should "no consecutive 1s" in {
    def noConsecutiveOnes(n: Int): Boolean = (n & (n >>> 1)) == 0

    def f(x: Int) = if (noConsecutiveOnes(x)) 1 else 0

    var results = scala.collection.mutable.Map[Boolean, Int](true -> 0, false -> 0)

    val count = 100
    for (i <- 1 to count) {
      val s = grover(f, 4)

      println("final state: " + s.toString)
      val m = Word.toInt(s.measure(_._1).outcome)
      println("measurement: " + m + " -> " + Integer.toBinaryString(m))
      results(noConsecutiveOnes(m)) = results(noConsecutiveOnes(m)) + 1
    }
    println(results)
  }

  "groverb" should "no consecutive 1s" in {
    def noConsecutiveOnes(n: Int): Boolean = (n & (n >>> 1)) == 0

    def f(x: Int) = if (noConsecutiveOnes(x)) 1 else 0

    var results = scala.collection.mutable.Map[Boolean, Int](true -> 0, false -> 0)

    val count = 100
    for (i <- 1 to count) {
      val s = grover(noConsecutiveOnes)(4)

      println("final state: " + s.toString)
      val m = Word.toInt(s.measure().outcome)
      println("measurement: " + m/2 + " -> " + Integer.toBinaryString(m/2))
      results(noConsecutiveOnes(m)) = results(noConsecutiveOnes(m)) + 1
    }
    println(results)
  }

  "grover indexed" should "work" in {
    def f(x: Int) = x == 5

    val n_controls = 0
    val n_targets = 3

    val controls = (0 to n_controls - 1).toList
    val targets = (n_controls to n_controls + n_targets - 1).toList
    val ancilla = n_controls + n_targets

    val g = oracleL(f)(targets, ancilla) _ >=> invL(targets ++ List(ancilla))

    var state = pure(Word[Std](List.fill(n_controls + n_targets)(S0) ++ List(S1)))

    for (j <- (0 to n_controls + n_targets)) {
      state = state >>= wire(j, H)
    }

    state = state >>= g
    state.hist

    state = state >>= g
    state.hist
  }


  "controls shift" should "work" in {
    def f(x: Int) = x == 5

    val n_controls = 1
    val n_targets = 3

    val g0 = oracleL(f)(List(0, 1, 2), 3) _ >=> invL(List(0, 1, 2) ++ List(3))

    def gf(shift: Int) =
      oracleL(f)(List(0, 1, 2).map { i => i + shift }, 3 + shift) _ >=> invL((List(0, 1, 2) ++ List(3)).map { i => i + shift })

    lazy val g = gf(n_controls)

    var state = pure(Word[Std](List.fill(n_controls + n_targets)(S0) ++ List(S1)))

    for (j <- (0 to n_controls + n_targets)) {
      state = state >>= wire(j, H)
    }

    state = state >>= g
    state.hist

    state = state >>= g
    state.hist
  }

  "controlled g" should "work" in {
    def f(x: Int) = x == 5

    val n_controls = 2
    val n_targets = 3

    val g0 = oracleL(f)(List(0, 1, 2), 3) _ >=> invL(List(0, 1, 2) ++ List(3))

    def gf(shift: Int) =
      oracleL(f)(List(0, 1, 2).map { i => i + shift }, 3 + shift) _ >=> invL((List(0, 1, 2) ++ List(3)).map { i => i + shift })

    lazy val g = gf(n_controls)

    var state = pure(Word[Std](List.fill(n_controls + n_targets)(S0) ++ List(S1)))

    for (j <- (0 to n_controls + n_targets)) {
      state = state >>= wire(j, H)
    }

    for (i <- 0 until n_controls) {
      state = state >>= controlledI(0, g)
      state.hist
    }

  }
}
