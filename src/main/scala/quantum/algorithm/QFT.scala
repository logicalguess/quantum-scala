package quantum.algorithm

import quantum.domain.Gate._
import quantum.domain.QState._
import quantum.domain.Labeled.Tensor
import quantum.domain.QState
import quantum.domain.Symbol.{Std, Word}


object QFT {

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