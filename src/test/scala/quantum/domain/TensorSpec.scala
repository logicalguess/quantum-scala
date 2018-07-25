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
