package quantum.domain

import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import quantum.domain.Gate.{H, R, assoc1, assoc2, cnot, controlledW, lift1, lift12, lift2, liftWord, reverse, rot, swap, wire}
import quantum.domain.Labeled.Tensor
import quantum.domain.QState.{pure, s0, s1}
import quantum.domain.Symbol.{S0, S1, Std, Word}

class WordSpec extends FlatSpec with GeneratorDrivenPropertyChecks {


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

  "tensor" should "words" in {
    println(wire(0, H)(Word.fromInt(0, 2)))

    println(cliftWord(0, 1, H)(Word(List(S1, S1))))
    println(cliftWord(0, 2, H)(Word(List(S1, S0, S0))))
    println(cliftWord(1, 2, H)(Word(List(S0, S1, S0))))
  }

  "control" should "reverse" in {

    val state = pure(Word.fromInt(0, 3)) >>= wire(0, H) _ >>= wire(1, rot(math.Pi/8)) _
    println(state)
    println(state  >>= reverse _)

    println(cliftWord1(2, 0, H)(Word(List(S0, S0, S1))))
    println(controlledW(2, 0, H)(Word(List(S0, S0, S1))))

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
      pure(s) >>= lift2(rot(theta/2)) >>= cnot >>= lift2(rot(-theta/2)) >>= cnot

    val p = 0.3
    val theta = math.asin(math.sqrt(p))

    val start = pure(Word.fromInt(0, 3)) >>=
      wire(0, H) _  >>= wire(1, H) _  >>= wire(2, rot(theta)) _

    println(start)

    val init: QState[Tensor[Tensor[Std, Std], Std]] = s0 * s0 * s0 >>=
      lift12(lift12(H, H), rot(theta))

    println(init)

    val start1 = start >>=
      cliftWord(0, 2, rot(2*theta))

    start1.probs

    val stage1 = init >>=
      lift1(swap) >>=
      assoc2 >>=
      lift2(crot(2*theta)) >>=
      assoc1 >>=
      lift1(swap)

    stage1.probs
    //stage1.hist

    val start2 = start1 >>=
      cliftWord(1, 2, rot(4*theta))

    start2.probs

    val stage2 = stage1 >>=
      assoc2 >>=
      lift2(crot(4*theta)) >>=
      assoc1

    stage2.probs
  }

  "word" should "circuit1" in {

    val p = 0.3
    val theta = math.asin(math.sqrt(p))

    val start = pure(Word.fromInt(0, 4)) >>=
      wire(3, rot(theta)) _ >>= wire(0, H) _  >>= wire(1, H) _  >>= wire(2, H) _

    val stage1 = start >>=
      cliftWord(0, 3, rot(2*theta)) >>=
      cliftWord(1, 3, rot(4*theta))>>=
      cliftWord(2, 3, rot(8*theta))

    stage1.probs
    stage1.hist

    var stage2 = stage1 >>=
      wire(2, H) >>=
      controlledW(2, 1, R(-math.Pi/math.pow(2, 2 - 1))) >>=
      controlledW(2, 0, R(-math.Pi/math.pow(2, 2 - 0))) >>=
      wire(1, H)  >>=
      controlledW(1, 0, R(-math.Pi/math.pow(2, 1 - 0))) >>=
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
      for (j <- 0 to s.letters.size - 2)
        state = state >>= controlledW(j, s.letters.size - 1, rot(math.pow(2, j + 1)*theta))
      state
    }

    def iqft(s: Word[Std]): QState[Word[Std]] = {
      var state = pure(s)
      for (j <- (0 to s.letters.size - 2).reverse) {
        state = state >>= wire(j, H)
        for (k <- (0 to j - 1).reverse)
          state = state >>= controlledW(j, k, R(-math.Pi/math.pow(2, j - k)))
      }
      state
    }

    val start = pure(Word.fromInt(0, 4)) >>= wire(3, rot(theta)) >>= hs
    val stage1 = start >>= rots
    val stage2 = stage1 >>= iqft

    //stage1.probs
    //stage1.hist
    //stage2.probs
    stage2.hist
  }
}
