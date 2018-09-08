package quantum.domain

import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import quantum.algorithm.Grover.{invL, oracle}
import quantum.algorithm.{Amplitude, Grover}
import quantum.domain.Gate._
import quantum.domain.QState._
import quantum.domain.Symbol.{S0, S1, Std, Word}

import scala.collection.immutable.ListMap

class WordSpec extends FlatSpec with GeneratorDrivenPropertyChecks {

  def trunc(x: Double, n: Int) = {
    def p10(n: Int, pow: Long = 10): Long = if (n == 0) pow else p10(n - 1, pow * 10)

    if (n < 0) {
      val m = p10(-n).toDouble
      math.round(x / m) * m
    }
    else {
      val m = p10(n).toDouble
      math.round(x * m) / m
    }
  }

  // assuming t > c
  def cliftWord(c: Int, t: Int, g: Std => QState[Std])(s: Word[Std]): QState[Word[Std]] = {
    s match {
      case Word(Nil) => pure(Word(Nil))
      case _ if t <= c => throw new Error("control has to be less than target have to be different")
      case Word(S0 :: rest) if c == 0 => pure(Word(S0 :: rest))
      case Word(S1 :: rest) if c == 0 => s1 *: wire(t - 1, g)(Word(rest))
      case Word(h :: rest) => pure(h) *: cliftWord(c - 1, t - 1, g)(Word(rest))
    }
  }


  // t < c
  def cliftWord1(c: Int, t: Int, g: Std => QState[Std])(s: Word[Std]): QState[Word[Std]] = {
    val size = s.letters.size
    cliftWord(size - 1 - c, size - 1 - t, g)(Word(s.letters.reverse)) >>= reverse _
  }

  "control" should "words" in {
    println(wire(0, H)(Word.fromInt(0, 2)))

    println(cliftWord(0, 1, H)(Word(List(S1, S1))))
    println(cliftWord(0, 2, H)(Word(List(S1, S0, S0))))
    println(cliftWord(1, 2, H)(Word(List(S0, S1, S0))))

    println(cliftWord(0, 1, H)(Word(List(S1, S1))))
    println(controlledW1(0, 1, H)(Word(List(S1, S1))))
    println(controlledL(Set(0), 1, H)(Word(List(S1, S1))))

    println(controlledW1(1, 2, H)(Word(List(S0, S1, S0))))
    println(controlledL(Set(1), 2, H)(Word(List(S0, S1, S0))))
  }

  "control" should "reverse" in {

    val state = pure(Word.fromInt(0, 3)) >>= wire(0, H) _ >>= wire(1, rot(math.Pi / 8)) _
    println(state)
    println(state >>= reverse _)

    println(cliftWord1(2, 0, H)(Word(List(S0, S0, S1))))
    println(controlledW1(2, 0, H)(Word(List(S0, S0, S1))))
    println(controlledL(Set(2), 0, H)(Word(List(S0, S0, S1))))
  }

  "control" should "multi" in {

    println(controlledL(Set(0, 1), 2, H)(Word(List(S0, S1, S0))))
    println(controlledL(Set(0, 1), 2, H)(Word(List(S1, S0, S0))))
    println(controlledL(Set(0, 1), 2, H)(Word(List(S1, S1, S0))))

    println(controlledL(Set(0, 2), 1, H)(Word(List(S0, S0, S1))))
    println(controlledL(Set(0, 2), 1, H)(Word(List(S1, S0, S0))))
    println(controlledL(Set(0, 2), 1, H)(Word(List(S1, S0, S1))))
  }

  "word and tensor" should "words1" in {
    val one = pure(Word.fromInt(0, 1))
    val zeroes = pure(Word.fromInt(0, 2))

    val Hn = liftWord(H) _

    val s = one * zeroes >>= lift12(Hn, Hn)
    println(s)
    println(one * zeroes >>= lift1(wire(0, H) _) >>= lift2(wire(0, H) _) >>= lift2(wire(1, H) _))
  }

  def fib(n: Int): QState[Word[Std]] = {
    var state = pure(Word.fromInt(0, n))
    for (i <- 0 until n) state = state >>= wire(i, H)
    for (i <- 0 until n - 1)  state = state >>= controlledL(Set(i), i + 1, ZERO)
    //for (i <- 0 until n - 1)  state = state >>= wire(i + 1, Ry(-math.Pi/4)) >>= controlledL(Set(i), i + 1, X) >>=
    //  wire(i + 1, Ry(math.Pi/4)) >>= controlledL(Set(i), i + 1, X)

    state
  }

  "fib" should "circuit" in {

    for (n <- 1 to 16) {
      val q = fib(n)
      //val possibleStates = q.state.filter({ case (w, a) => a.norm2 > 0.000001 })
      println(s"F($n) = ${q.state.size} : ${q}")
      //q.hist
    }
  }

  "fib" should "shots" in {
    val max = 14

    for (n <- 1 to max) {

      var results = scala.collection.mutable.Set[String]()
      val q = fib(n)

      for (shot <- 1 to math.pow(2, max + 1).toInt) {
        val m = q.measure(w => pure(w)).outcome
        results += m.toString
      }
      println(s"F($n) expected = ${q.state.size} : ${q}")
      println(s"F($n) measured = ${results.size} : ${results}")
      assert(results.size == q.state.size)
    }
  }

  def probToInt(prob: Double, angle: Double, bits: Int): Int =
    (math.asin(math.sqrt(prob*math.pow(2, bits)))/angle).round.toInt

  def counter(k: Int, angle: Double, bits: Int): Double = math.pow(math.sin(k*angle), 2)/math.pow(2, bits)


  "count consecutive ones" should "single input" in {
    def anglef(bits: Int) = 1 / math.pow(2, bits)

    def consecutiveOnes(i: Int, width: Int): Double = {
      var state = pure(Word.fromInt(2 * i, width))
      for (i <- 0 until width - 2) state = state >>= controlledL(Set(i, i + 1), width - 1, rot(anglef(width)))

      // probability last bit is 1
      state(Word.fromInt(2 * i + 1, width)).norm2
    }

    def probToInt(prob: Double, angle: Double, bits: Int): Int =
      (math.asin(math.sqrt(prob))/angle).round.toInt

    for (w <- 2 to 6) {
      println(w)
      val angle = anglef(w)
      for (i <- 0 until math.pow(2, w - 1).toInt) {
        println(Word.fromInt(i, w) + " -> " + probToInt(consecutiveOnes(i, w), angle, w))
      }
    }
  }

  "count consecutive ones" should "superposition of all inputs" in {
    def anglef(bits: Int) = 1 / math.pow(2, bits)

    def circuit(width: Int) = {
      var state = pure(Word.fromInt(0, width))
      for (i <- 0 until width - 1) state = state >>= wire(i, H)
      for (i <- 0 until width - 2) state = state >>= controlledL(Set(i, i + 1), width - 1, rot(anglef(width)))
      state
    }

    def probToInt(prob: Double, angle: Double, bits: Int): Int =
      (math.asin(math.sqrt(prob*math.pow(2, bits - 1)))/angle).round.toInt

    for (n <- 2 to 6) {
      println(n)
      val angle = anglef(n)
      val state = circuit(n)
      val power: Int = math.pow(2, n - 1).toInt
      for (k <- 0 until power) {
        //prob of last bit being 1
        val prob = state(Word.fromInt(2 * k + 1, n)).norm2

        println(Word.fromInt(k, n) + " -> " + probToInt(prob, angle, n))
      }
    }
  }

  "count ones" should "single input" in {
    def anglef(bits: Int) = 1 / math.pow(2, bits)

    def ones(i: Int, width: Int): Double = {
      var state = pure(Word.fromInt(2 * i, width))
      for (i <- 0 until width - 1) state = state >>= controlledL(Set(i), width - 1, rot(anglef(width)))

      // probability last bit is 1
      state(Word.fromInt(2 * i + 1, width)).norm2
    }

    def probToInt(prob: Double, angle: Double, bits: Int): Int =
      (math.asin(math.sqrt(prob))/angle).round.toInt

    for (w <- 2 to 15) {
      println(w)
      val angle = anglef(w)
      for (i <- 0 until math.pow(2, w - 1).toInt) {
        assert(trunc(ones(i, w), 9) == trunc(math.pow(math.sin(Integer.bitCount(i)*angle), 2), 9))
        assert(Integer.bitCount(i) == probToInt(ones(i, w), angle, w))
      }
    }
  }

  "count ones" should "superposition of all inputs" in {
    def anglef(bits: Int) = 1 / math.pow(2, bits)

    def circuit(width: Int) = {
      var state = pure(Word.fromInt(0, width))
      for (i <- 0 until width - 1) state = state >>= wire(i, H)
      for (i <- 0 until width - 1) state = state >>= controlledL(Set(i), width - 1, rot(anglef(width)))
      state
    }

    def probToInt(prob: Double, angle: Double, bits: Int): Int =
      (math.asin(math.sqrt(prob*math.pow(2, bits - 1)))/angle).round.toInt

    for (n <- 2 to 15) {
      println(n)
      val angle = anglef(n)
      val state = circuit(n)
      val power: Int = math.pow(2, n - 1).toInt
      for (k <- 0 until power) {
        //prob of last bit being 1
        val prob = state(Word.fromInt(2 * k + 1, n)).norm2

        assert(trunc(prob, 9) == trunc(2 * counter(Integer.bitCount(k), angle, n), 9))
        assert(Integer.bitCount(k) == probToInt(prob, angle, n))
      }
    }
  }

  "count" should "circuit" in {

    def anglef(bits: Int) = 1/math.pow(2, 5/*bits*/)

    // |input>|1>
    def probLastBitOne(state: QState[Word[Std]], width: Int) = state(Word.fromInt(2 * (width - 1) + 1, width)).norm2


    def circuit(bits: Int, k: Int): QState[Word[Std]] = {
      val angle = anglef(bits)

      var state = pure(Word.fromInt(0, bits + 1))
      for (i <- 0 until bits) state = state >>= wire(i, H)
      for (i <- 0 until k) state = state >>= wire(bits, rot(angle))

      state
    }

    for (n <- 0 to 8) {
      println(n)
      val angle = anglef(n)
      val power: Int = math.pow(2, n).toInt
      for (k <- 0 to power) {
        //println(s"C($n) = ${circuit(n, k).state.size/2} : ${circuit(n, k)}")

        //prob of last bit being 1
        val prob = probLastBitOne(circuit(n, k), n + 1)

        assert(trunc(prob, 9) == trunc(counter(k, angle, n), 9))
        //assert(k == probToInt(prob, angle, n))
      }
    }
  }

  "oracle" should "last bit 1" in {

    val width = 3
    var state = pure(Word[Std](List.fill(width)(S0) ++ List(S0)))

    for (j <- (0 to width)) {
      state = state >>= wire(j, H)
    }

    //def f(x: Int) = true
    //def f(x: Int) = x == 3
    //def f(x: Int) = x <= 1
    def f(n: Int): Boolean = (n & (n >>> 1)) == 0

    state = state >>= Grover.oracleLQ(f)
    println(state)
    state.hist

  }

  "oracle" should "ancilla qbit rotation" in {
    def anglef(bits: Int) = 0.01

    def oracle(f: Int => Boolean)(s: Word[Std]): QState[Word[Std]] = {
      val size = s.letters.size
      val x = Word.toInt(Word(s.letters.take(size - 1)))

      val state = pure(s)
      println(s + " -> " + f(x))
      if (f(x)) state else state >>= wire(size - 1, rot(math.Pi/4))
    }

    //def f(x: Int) = true
    //def f(x: Int) = x == 3
    //def f(x: Int) = x <= 1
    def f(n: Int): Boolean = (n & (n >>> 1)) == 0

    val width = 3
    var state = pure(Word[Std](List.fill(width)(S0) ++ List(S1)))

    for (j <- (0 to width)) {
      state = state >>= wire(j, H)
    }

    state = state >>= oracle(f)
    println(state)
    state.hist

    //prob of last bit being 1
//    val prob = probLastBitOne(state, width)
//    println(prob)
//    println(probToInt(prob, anglef(width), 3))

    val top = state.state.filter({ case (w, a) => w.letters.last == S1} ).sortBy(_._2.norm2).reverse
    val ints = top.map { case (w, a) => (Word.toInt(Word(w.letters.take(width))), a.norm2) }
    println(ints)

    val estimates = Amplitude.estimate(state, true)
    println(estimates.maxBy(_._2))
  }


  "count" should "circuit2" in {

    def anglef(bits: Int) = 0.01

    def circuit(bits: Int): QState[Word[Std]] = {
      val angle = anglef(bits)

      var state = pure(Word.fromInt(0, bits + 1))
      for (i <- 0 until bits) state = state >>= wire(i, H)
      //for (i <- 0 until bits - 1)  state = state >>= controlledL(Set(i), i + 1, ZERO)
      for (i <- 0 until bits) state = state >>= controlledL(Set(i), bits, rot(angle))

      state
    }

    for (n <- 0 to 5) {
      val angle = anglef(n)
      val power: Int = math.pow(2, n).toInt
      val circ = circuit(n)
      println(s"C($n) = ${circ.state.size / 2} : ${circ}")

      var probs = for {
        x <- circ.state.sortBy(_._1)
      } yield Word.transform(l => l.take(n))(x._1) -> x._2.norm2
      println(probs)

      val mapped = probs.groupBy(_._1).mapValues { l => l.foldLeft(0.0) { case (s, (k, v)) => s + v } }
      println(ListMap(mapped.toSeq.sortBy(_._1): _*))
    }
  }

  "controlled0" should "inv" in {
    val n = 4
    var state = pure(Word.fromInt(0, n))

    for (i <- 0 until n) state = state >>= wire(i, X) >>= wire(i, H)
    println(state)

    state = state >>= controlledL0((0 until n - 1).toList.toSet, n - 1, X)

    println(state)
  }

  "controlled1" should "inv" in {
    val n = 4
    var state = pure(Word.fromInt(0, n))

    for (i <- 0 until n) state = state >>= wire(i, H)
    println(state)

    for (i <- 0 until n) state = state >>= wire(i, X)
    state = state >>= controlledL((0 until n - 1).toList.toSet, n - 1, Z)
    for (i <- 0 until n) state = state >>= wire(i, X)
    println(state)
  }

  "controlled0" should "grover" in {
    val n = 4
    var state = pure(Word.fromInt(0, n))

    for (i <- 0 until n - 1) state = state >>= wire(i, X) >>= wire(i, H)

    // oracle
    state = state >>= controlledL(Set(n - 3, n - 2), n - 1, X)
    state = state >>= wire(n - 1, Z)
    state = state >>= controlledL(Set(n - 3, n - 2), n - 1, X)

    // diffusion
    state = state >>= controlledL0((0 until n - 2).toList.toSet, n - 2, X)

    // oracle
    state = state >>= controlledL(Set(n - 3, n - 2), n - 1, X)
    state = state >>= wire(n - 1, Z)

    println(state)
    state.hist

  }

  "controlled1" should "grover" in {
    val n = 3
    val o = oracle(i => i == 3) _
    var state = pure(Word[Std](List.fill(n - 1)(S0) ++ List(S1)))

    for (i <- 0 until n) state = state >>= wire(i, H)

    // oracle
    state = state >>= o

    // diffusion
    for (i <- 0 until n - 1) state = state >>= wire(i, H) >>= wire(i, X)
    state = state >>= controlledL((0 until n - 2).toList.toSet, n - 2, Z)
    for (i <- 0 until n - 1) state = state >>= wire(i, X) >>= wire(i, H)

    println(state)
    state.hist

  }

}