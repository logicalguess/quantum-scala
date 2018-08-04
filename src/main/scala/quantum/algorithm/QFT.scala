package quantum.algorithm

import quantum.domain.Gate._
import quantum.domain.QState._
import quantum.domain.Labeled.Tensor
import quantum.domain.QState
import quantum.domain.Symbol.{Std, Word}

object QFT {

  def iqft(qs: List[Int])(s: QState[Word[Std]]): QState[Word[Std]] = {
    var state = s
    for (j <- (0 to qs.size - 1).reverse) {
      state = state >>= wire(qs(j), H)
      for (k <- (0 to j - 1).reverse)
        state = state >>= controlledW(qs(j), qs(k), R(-math.Pi / math.pow(2, j - k)))
    }
    state
  }

  def qft1(qs: List[Int])(s: QState[Word[Std]]): QState[Word[Std]] = {
    var state = s
    for (j <- (0 to qs.size - 1).reverse) {
      state = state >>= wire(qs(j), H)
      for (k <- (0 to j - 1).reverse)
        state = state >>= controlledW(qs(k), qs(j), R(math.Pi / math.pow(2, j - k)))
    }
    state
  }

  def qftL(qs: List[Int])(s: QState[Word[Std]]): QState[Word[Std]] = {
    var state = s
    for (j <- (0 to qs.size - 1)) {
      state = state >>= wire(qs(j), H)
      for (k <- (j + 1 to qs.size - 1))
        state = state >>= controlledW(qs(k), qs(j), R(math.Pi / math.pow(2, k - j)))
    }
    state >>= reverse
  }

  def qft(s: Word[Std]): QState[Word[Std]] = {
    qftL((0 to s.letters.size - 1).toList)(pure(s))
//    var state = pure(s)
//    for (j <- (0 to s.letters.size - 1)) {
//      state = state >>= wire(j, H)
//      for (k <- (j + 1 to s.letters.size - 1))
//        state = state >>= controlledW(k, j, R(math.Pi / math.pow(2, k - j)))
//    }
//    state >>= reverse
  }

  // Quantum Fourier Transform
  def QFT(b: Word[Std]): QState[Word[Std]] = {
    def wires(theta: Double)(xs: Tensor[Std, Word[Std]]): QState[Tensor[Std, Word[Std]]] = xs match {
      case Tensor(c, Word(Nil)) => pure(Tensor(c, Word(Nil)))
      case t => {
        pure(t) >>=
          lift2(decons) >>=
          assoc1 >>=
          lift1(controlled(R(theta))) >>=
          lift1(swap) >>=
          assoc2 >>=
          lift2(wires(theta / 2)) >>=
          assoc1 >>=
          lift1(swap) >>=
          assoc2 >>=
          lift2(cons)
      }
    }
    def QFT_(b: Word[Std]): QState[Word[Std]] = b match {
      case Word(Nil) => pure(Word(Nil))
      case xs => {
        pure(xs) >>= decons >>= lift2(QFT_) >>= wires(tau / 4) >>= lift1(H) >>= cons
      }
    }
    pure(b) >>= reverse >>= QFT_
  }

  def run = {
    val s = (pure(Word.fromInt(0, 4)) + pure(Word.fromInt(8, 4))) * rhalf
    s >>= QFT
  }


}
