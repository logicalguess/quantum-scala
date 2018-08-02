package quantum.domain

import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import quantum.domain.Gate._
import quantum.domain.Labeled.Tensor
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

  "word and tensor" should "circuit" in {
    def crot(theta: Double)(s: Tensor[Std, Std]): QState[Tensor[Std, Std]] =
      pure(s) >>= lift2(rot(theta / 2)) >>= cnot >>= lift2(rot(-theta / 2)) >>= cnot

    val p = 0.3
    val theta = math.asin(math.sqrt(p))

    val start = pure(Word.fromInt(0, 3)) >>=
      wire(0, H) _ >>= wire(1, H) _ >>= wire(2, rot(theta)) _

    println(start)

    val init: QState[Tensor[Tensor[Std, Std], Std]] = s0 * s0 * s0 >>=
      lift12(lift12(H, H), rot(theta))

    println(init)

    val start1 = start >>=
      cliftWord(0, 2, rot(2 * theta))

    start1.probs

    val stage1 = init >>=
      lift1(swap) >>=
      assoc2 >>=
      lift2(crot(2 * theta)) >>=
      assoc1 >>=
      lift1(swap)

    stage1.probs
    //stage1.hist

    val start2 = start1 >>=
      cliftWord(1, 2, rot(4 * theta))

    start2.probs

    val stage2 = stage1 >>=
      assoc2 >>=
      lift2(crot(4 * theta)) >>=
      assoc1

    stage2.probs
  }

  "word" should "circuit1" in {

    val p = 0.3
    val theta = math.asin(math.sqrt(p))

    val start = pure(Word.fromInt(0, 4)) >>=
      wire(3, rot(theta)) _ >>= wire(0, H) _ >>= wire(1, H) _ >>= wire(2, H) _

    val stage1 = start >>=
      cliftWord(0, 3, rot(2 * theta)) >>=
      cliftWord(1, 3, rot(4 * theta)) >>=
      cliftWord(2, 3, rot(8 * theta))

    stage1.probs
    stage1.hist

    var stage2 = stage1 >>=
      wire(2, H) >>=
      controlledW1(2, 1, R(-math.Pi / math.pow(2, 2 - 1))) >>=
      controlledW1(2, 0, R(-math.Pi / math.pow(2, 2 - 0))) >>=
      wire(1, H) >>=
      controlledW1(1, 0, R(-math.Pi / math.pow(2, 1 - 0))) >>=
      wire(0, H)

    stage2.probs
    stage2.hist
  }

  "word" should "circuit2" in {
    val p = 0.3
    val theta = math.asin(math.sqrt(p))

    def hs(s: Word[Std]): QState[Word[Std]] = {
      var state = pure(s)
      for (j <- 0 to s.letters.size - 2)
        state = state >>= wire(j, H)
      state
    }

    def rots(s: Word[Std]): QState[Word[Std]] = {
      var state = pure(s)
      val last = s.letters.size - 1
      for (j <- 0 to last - 1)
        state = state >>= controlledW(j, last, rot(math.pow(2, j + 1) * theta))
      state
    }

    def iqft(s: Word[Std]): QState[Word[Std]] = {
      var state = pure(s)
      for (j <- (0 to s.letters.size - 2).reverse) {
        state = state >>= wire(j, H)
        for (k <- (0 to j - 1).reverse)
          state = state >>= controlledW(j, k, R(-math.Pi / math.pow(2, j - k)))
      }
      state
    }

    val start = pure(Word.fromInt(0, 4)) >>= wire(3, rot(theta)) >>= hs
    val stage1 = start >>= rots
    val stage2 = stage1 >>= iqft

    //stage1.probs
    //stage1.hist
    stage2.probs
    stage2.hist

    val probs = for {
      x <- stage2.state.sortBy(_._1)
    } yield (Word.toInt(l => l.tail)(x._1) -> x._2.norm2) // Word.toInt(x._1) % 8

    println(probs)
    val mapped = probs.groupBy(_._1).mapValues { l => l.foldLeft(0.0) { case (s, (k, v)) => s + v } }
    println(ListMap(mapped.toSeq.sortBy(_._1): _*))

    val sinProbs = for {
      x <- stage2.state.sortBy(_._1)
    } yield (trunc(math.pow(math.sin(math.Pi * Word.toInt(l => l.tail)(x._1) / 8), 2), 3) -> x._2.norm2)
    println(sinProbs)

    val estimates = sinProbs.groupBy(_._1).mapValues { l => l.foldLeft(0.0) { case (s, (k, v)) => trunc(s + v, 3) } }
    println(estimates)
  }

  "poly" should "a*x^2 + b*x + c" in {
    val a = 1.0
    val b = 1.0
    val c = 1.0

    // i = 0, 1, 2 or 3
    def poly(i: Int): Double = {
      val circuit = pure(Word.fromInt(2 * i, 3)) >>=
        wire(2, rot(c)) >>=
        controlledL(Set(0), 2, rot(4 * a + 2 * b)) >>=
        controlledL(Set(0, 1), 2, rot(4 * a)) >>=
        controlledL(Set(1), 2, rot(a + b))

      // probability last bit is 1
      circuit(Word.fromInt(2 * i + 1, 3)).norm2
    }

    for (i <- 0 to 3) {
      val sinp = math.sin(a * i * i + b * i + c)
      assert(trunc(poly(i), 9) == trunc(sinp * sinp, 9))
    }

    // 2 bits for input, 1 ancilla
    val circuit = pure(Word.fromInt(0, 3)) >>=
      wire(0, H) >>=
      wire(1, H) >>=
      wire(2, rot(c)) >>=
      controlledL(Set(0), 2, rot(4 * a + 2 * b)) >>=
      controlledL(Set(0, 1), 2, rot(4 * a)) >>=
      controlledL(Set(1), 2, rot(a + b))

    for (i <- 0 to 3) {
      // probability last bit is 1
      val prob = circuit(Word.fromInt(2 * i + 1, 3)).norm2
      val sinp = math.sin(a * i * i + b * i + c) / 2
      assert(trunc(prob, 9) == trunc(sinp * sinp, 9))
    }
  }

  "fib" should "circuit" in {

    val zg: Gate[Std, Std] = (s0 >< s0) + (s0 >< s1)

    def fib(n: Int): QState[Word[Std]] = {
      var state = pure(Word.fromInt(0, n))
      for (i <- 0 until n) state = state >>= wire(i, H)
      for (i <- 0 until n - 1)  state = state >>= controlledL(Set(i), i + 1, zg)
      state
    }

    for (n <- 1 to 5) {
      val q = fib(n)
      println(s"F($n) = ${q.state.size} : ${q}")
      q.hist
    }
  }

  def probLastBitOne(state: QState[Word[Std]], width: Int) = state(Word.fromInt(2 * width - 1, width)).norm2

  def probToInt(prob: Double, angle: Double, bits: Int): Int =
    (math.asin(math.sqrt(prob)*math.pow(2, bits/2.0))/angle).floatValue().intValue()

  def counter(k: Int, angle: Double, bits: Int): Double = math.pow(math.sin(k*angle)/math.pow(2, bits/2.0), 2)

  "count" should "circuit" in {

    def anglef(bits: Int) = 1/math.pow(2, 5/*bits*/)

    def circuit(bits: Int, k: Int): QState[Word[Std]] = {
      val angle = anglef(bits)

      var state = pure(Word.fromInt(0, bits + 1))
      for (i <- 0 until bits) state = state >>= wire(i, H)
      for (i <- 0 until k) state = state >>= wire(bits, rot(angle))

      state
    }

    for (n <- 0 to 5) {
      val angle = anglef(n)
      val power: Int = math.pow(2, n).toInt
      for (k <- 0 to power) {
        println(s"C($n) = ${circuit(n, k).state.size/2} : ${circuit(n, k)}")

        //prob of last bit being 1
        val prob = probLastBitOne(circuit(n, k), n + 1)

        assert(trunc(prob, 9) == trunc(counter(k, angle, n), 9))
        assert(k == probToInt(prob, angle, n))
      }
    }
  }


  "count" should "circuit2" in {

    def anglef(bits: Int) = 0.01

    def circuit(bits: Int): QState[Word[Std]] = {
      val angle = anglef(bits)

      var state = pure(Word.fromInt(0, bits + 1))
      for (i <- 0 until bits) state = state >>= wire(i, H)
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
}