package quantum.algorithm

import quantum.domain.{Gate, QState}
import quantum.domain.Gate.{H, R, controlledW, rot, wire}
import quantum.domain.QState.pure
import quantum.domain.Symbol.{Std, Word}

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

  def run(bits: Int, op: Gate[Std, Std], q: Int => Gate[Std, Std]): QState[Word[Std]] =
    pure(Word.fromInt(0, bits)) >>= wire(bits - 1, op) >>= hs >>= lambda(q) >>= iqft

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
