package quantum.domain

import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import quantum.domain.Gate.{lift2, _}
import quantum.domain.Labeled.Tensor
import quantum.domain.QState._
import quantum.domain.Symbol._

class TensorSpec extends FlatSpec with GeneratorDrivenPropertyChecks {

  def crot(theta: Double)(s: Tensor[Std, Std]): QState[Tensor[Std, Std]] =
    pure(s) >>= lift2(rot(theta/2)) >>= cnot >>= lift2(rot(-theta/2)) >>= cnot

  def crot_gate(theta: Double): Gate[Tensor[Std, Std], Tensor[Std, Std]] =
    (lift2[Std, Std, Std] _)(rot(theta / 2)) >=> cnot >=> (lift2[Std, Std, Std] _)(rot(-theta / 2)) >=> cnot


  "tensor" should "s0*s0" in {
    val init: QState[Tensor[Std, Std]] = s0 * s0

    assert(init(Tensor(S0, S0)) == Complex.one)
    assert(init(Tensor(S0, S1)) == Complex.zero)
    assert(init(Tensor(S1, S0)) == Complex.zero)
    assert(init(Tensor(S1, S1)) == Complex.zero)

    val s = init >>= lift12(H, H)
    s.probs
  }

  "tensor" should "s0*s0*s0" in {
    val init: QState[Tensor[Tensor[Std, Std], Std]] = s0 * s0 * s0

    val s1 = init >>= lift12(lift12(H, H), H)
    s1.probs

    val s2 = init >>= assoc2 >>= lift12(H, lift12(H, H))
    s2.probs


    val s3 = init >>= assoc2 >>= lift12(H, lift12(H, rot(math.Pi)))
    s3.probs
  }

  "tensor" should "controlled s1*s0*s0" in {
    val init: QState[Tensor[Tensor[Std, Std], Std]] = s1 * s0 * s0

    val c1: QState[Tensor[Tensor[Std, Std], Std]] = init >>=
      lift1(crot(math.Pi/2))

    c1.probs

    println()

    val c2: QState[Tensor[Std, Tensor[Std, Std]]] = init >>=
      assoc2 >>=
      lift2(controlled(rot(math.Pi/2)))

    c2.probs
  }

  "tensor" should "controlled s0*s1*s0" in {
    val init: QState[Tensor[Tensor[Std, Std], Std]] = s0 * s1 * s0

    val c1: QState[Tensor[Tensor[Std, Std], Std]] = init >>=
      lift1(controlled(rot(math.Pi/2)))

    c1.probs

    println()

    val c2: QState[Tensor[Std, Tensor[Std, Std]]] = init >>=
      assoc2 >>=
      lift2(crot(math.Pi/2))

    c2.probs
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

  "tensor" should "words1" in {
    val one = pure(Word.fromInt(0, 1))
    val zeroes = pure(Word.fromInt(0, 2))

    val Hn = liftWord(H) _

    val s = one * zeroes >>= lift12(Hn, Hn)
    println(s)
    println(one * zeroes >>= lift1(wire(0, H) _) >>= lift2(wire(0, H) _) >>= lift2(wire(1, H) _))
  }

  "word" should "circuit" in {

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

  "tensor" should "circuit" in {
    val p = 0.3
    val theta = math.asin(math.sqrt(p))

    val init: QState[Tensor[Tensor[Std, Std], Std]] = s0 * s0 * s0 >>= lift12(lift12(H, H), rot(theta))

    val stage1 = init >>=
      lift1(swap) >>=
      assoc2 >>=
      lift2(crot(2*theta)) >>=
      assoc1 >>=
      lift1(swap)

    stage1.probs
    stage1.hist

    val stage2 = stage1 >>=
      assoc2 >>=
      lift2(crot(4*theta)) >>=
      assoc1

    stage2.probs
    stage2.hist
  }

  "tensor" should "ctrl" in {
    def c1(init: QState[Tensor[Tensor[Std, Std], Std]])(g: Tensor[Std, Std] => QState[Tensor[Std, Std]]) = {
      val state = init >>=
        lift1(swap) >>=
        assoc2 >>=
        lift2(g) >>=
        assoc1 >>=
        lift1(swap)
      state
    }

    val p = 0.3
    val theta = math.asin(math.sqrt(p))

    val init: QState[Tensor[Tensor[Std, Std], Std]] = s0 * s0 * s0 >>= lift12(lift12(H, H), rot(theta))

    c1(init)(crot(2*theta)).probs
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
      for (j <- 0 to s.letters.size - 2) {
        state = state >>= wire(j, H)

      }
      state
    }

    val start = pure(Word.fromInt(0, 4)) >>= wire(3, rot(theta)) >>= hs

    def rots(s: Word[Std]): QState[Word[Std]] = {
      var state = pure(s)
      for (j <- 0 to s.letters.size - 2) {
        state = state >>= controlledW(j, s.letters.size - 1, rot(math.pow(2, j + 1)*theta))

      }
      state
    }

      val stage1 = start >>= rots

    //stage1.probs
    //stage1.hist

    def iqft(s: Word[Std]): QState[Word[Std]] = {
      var state = pure(s)
      for (j <- (0 to s.letters.size - 2).reverse) {
        state = state >>= wire(j, H)
        for (k <- (0 to j - 1).reverse) {
          state = state >>= controlledW(j, k, R(-math.Pi/math.pow(2, j - k)))
        }
      }
      state
    }

    val stage2 = stage1 >>= iqft

    //stage2.probs
    stage2.hist
  }

  "tensor" should "component" in {

    case class Letter(label: String) extends Symbol

    def createTensor(n: Int): Labeled= n match {
      case 0 => Letter("0")
      case _ => Tensor(createTensor(n - 1), Letter("" + n))
    }

    def component1(t: Tensor[Labeled, Labeled], i: Int): Labeled = (t._1, i) match {
      case (l: Letter, _) => l
      case (_, 0) => t._2
      case (u: Tensor[Labeled, Labeled], 1) => u._2
      case (u: Tensor[Labeled, Labeled], _) => component1(u, i - 1)
      case (_, _) => throw new Error("Illegal arguments")
    }

    def component2(t: Tensor[Labeled, Labeled], i: Int): Labeled = (t._2, i) match {
      case (l: Letter, _) => l
      case (_, 0) => t._1
      case (u: Tensor[Labeled, Labeled], 1) => u._1
      case (u: Tensor[Labeled, Labeled], _) => component2(u, i - 1)
      case (_, _) => throw new Error("Illegal arguments")
    }

    def depth1(t: Tensor[Labeled, Labeled]): Int = {
      def helper(t: Tensor[Labeled, Labeled], i: Int): Int = t._1 match {
        case u: Tensor[Labeled, Labeled] => helper(u, i + 1)
        case l: Letter => i + 1
      }
      helper(t, 1)
    }

    def depth2(t: Tensor[Labeled, Labeled]): Int = {
      def helper(t: Tensor[Labeled, Labeled], i: Int): Int = t._2 match {
        case u: Tensor[Labeled, Labeled] => helper(u, i + 1)
        case l: Letter => i + 1
      }
      helper(t, 1)
    }

    val u = createTensor(5).asInstanceOf[Tensor[Labeled, Labeled]]
    println(u)
    println(component1(u, 0))
    println(component1(u, 1))
    println(component1(u, 2))
    println(component1(u, 3))
    println(component1(u, 4))
    println(component1(u, 5))

    println(depth1(u))


    val t = Tensor(Tensor(Tensor(Letter("a"), Letter("b")), Letter("c")), Letter("d"))
    println(t)
    println(component1(t, 0))
    println(component1(t, 1))
    println(component1(t, 2))
    println(component1(t, 3))

    println(depth1(t))

    val v = Tensor(Letter("d"), Tensor(Letter("c"), Tensor(Letter("b"), Letter("a"))))
    println(v)
    println(component2(v, 0))
    println(component2(v, 1))
    println(component2(v, 2))
    println(component2(v, 3))

    println(depth2(v))

    val p = Tensor(Letter("a"), Letter("b"))
    println(p)
    println(component1(p, 0))
    println(component1(p, 1))
    println(depth1(p))
    println(component2(p, 0))
    println(component2(p, 1))
    println(depth2(p))

    val m = Tensor(Tensor(Letter("a"), Letter("b")), Tensor(Letter("c"), Letter("d")))
    println(m)
    println(component1(m, 0))
    println(component1(m, 1))
    println(component1(m, 2))

    println(depth1(m))
    println(component2(m, 0))
    println(component2(m, 1))
    println(component2(m, 2))

    println(depth2(m))

  }
}
