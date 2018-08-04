package quantum.algorithm

import quantum.domain.{Gate, QState}
import quantum.domain.Gate._
import quantum.domain.QState._
import quantum.domain.Symbol.{S0, S1, Std, Word}

// Brassard
object Amplitude {

  def hs(s: Word[Std]): QState[Word[Std]] = {
    var state = pure(s)
    for (j <- 0 to s.letters.size - 2)
      state = state >>= wire(j, H)
    state
  }

  def lambda(q: Int => Gate[Std, Std])(s: Word[Std]): QState[Word[Std]] = {
    var state = pure(s)
    val last = s.letters.size - 1
    for (j <- 0 to last - 1) {
      state = state >>= controlledW(j, last, q(j))
    }
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

  def inv(s: Word[Std]): QState[Word[Std]] = {
    val size = s.letters.size
    var state = pure(s)
    for (j <- (0 to size - 1)) {
      state = state >>= wire(j, H) >>= wire(j, X)
    }
    state = state >>= controlledL((0 to size - 2).toSet, size - 1, Z)
    for (j <- (0 to size - 1)) {
      state = state >>= wire(j, X) >>= wire(j, H)
    }
    state
  }

  def oracle(f: Int => Boolean)(s: Word[Std]): QState[Word[Std]] = {
    val size = s.letters.size
    val x = Word.toInt(Word(s.letters.take(size - 1)))
    val fx = if (f(x))  S1 else S0
    val state = pure(Word[Std](s.letters.take(size - 1) ++ List(fx)))
    state
  }

  def grover(f: Int => Boolean)(width: Int): QState[Word[Std]] = {
    val r = (math.Pi * math.sqrt(math.pow(2, width)) / 4).toInt
    var state = pure(Word[Std](List.fill(width)(S0) ++ List(S1)))

    for (j <- (0 until width + 1)) {
      state = state >>= wire(j, H)
    }
    state.hist

    for (i <- 1 to r) {
      state = state  >>= oracle(f) //>>= inv
      println("Iteration " + i)
      state.hist
    }
    state
  }

  def run(bits: Int, op: Gate[Std, Std], q: Int => Gate[Std, Std]): QState[Word[Std]] =
    pure(Word.fromInt(0, bits)) >>= wire(bits - 1, op) >>= hs >>= lambda(q) >>= iqft

  def run(op: Gate[Std, Std], q: Int => Gate[Std, Std], init: QState[Word[Std]]): QState[Word[Std]] =
    init >>= wire(init.state.head._1.letters.size - 1, op) >>= lambda(q) >>= iqft

  def estimate(circuit: QState[Word[Std]], count: Boolean = false): Map[Double, Double] = {

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

    val n = circuit.state.head._1.letters.size
    val N = math.pow(2, n - 1)

    val factor = if (count) N else 1
    val sinProbs = for {
      x <- circuit.state.sortBy(_._1)
    } yield (trunc(factor * math.pow(math.sin(math.Pi * Word.toInt(l => l.tail)(x._1) / N), 2), 3) -> x._2.norm2)
    //println(sinProbs)

    val estimates = sinProbs.groupBy(_._1).mapValues { l => l.foldLeft(0.0) { case (s, (k, v)) => trunc(s + v, 3) } }
    estimates
  }
}
