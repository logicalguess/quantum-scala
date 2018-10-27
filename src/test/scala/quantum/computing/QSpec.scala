package quantum.computing

import org.scalatest.FlatSpec
import quantum.domain.Complex

class QSpec extends FlatSpec {

  def collect(bins: List[(String, Complex)]): List[(String, Complex)] = {
    bins.groupBy(_._1).toList.map {
      case (b, vs) => (b, vs.map(_._2).foldLeft(Complex.zero)(Complex.plus))
    }
  }

  def fMap(bins: List[(String, Complex)], f: String => List[(String, Complex)]) =
    collect(bins.flatMap({ case (b, v) => f(b).map { case (c, u) => (c, v*u) } }))

  def total(bins: List[(String, Complex)]): Double = math.sqrt(bins.map(_._2.norm2).foldLeft(0.0)(_ + _))

  def ><(bins1: List[(String, Complex)], bins2: List[(String, Complex)]): String => List[(String, Complex)] = {
    val m = collect(bins2).toMap
    b => bins1.map { case (b1, v1) => (b1, v1 * m.getOrElse(b, Complex.zero).conj) }
  }

  def pure(b: String): List[(String, Complex)] = List(b -> Complex.one)

  "1" should "" in {
    val bins: List[(String, Complex)] = List("|0>" -> 1 / math.sqrt(2), "|1>" -> 1 / math.sqrt(2))

    assert((total(bins) - 1.0) < 0.001)
  }

  "2" should "" in {
    def add(f: String => List[(String, Complex)], g: String => List[(String, Complex)]): String => List[(String, Complex)] =
      b => collect(f(b) ++ g(b))

    val s0 = pure("|0>")
    val s1 = pure("|1>")

    println(><(s1, s0)("|0>"))
    println(><(s0, s1)("|1>"))

    val X = add(><(s1, s0), ><(s0, s1))

    println(X("|0>"))
    println(X("|1>"))
  }

  "3" should "" in {
    val S0 = List("|0>" -> Complex.one, "|1>" -> Complex.zero)
    val S1 = List("|0>" -> Complex.zero, "|1>" -> Complex.one)

    import quantum.domain.Complex.toComplex

    val sq = toComplex(1 / math.sqrt(2))
    val H = Map(
      "|0>" -> List("|0>" -> sq, "|1>" -> sq),
      "|1>" -> List("|0>" -> sq, "|1>" -> -sq)
    )

    val s0 = QState[String](S0)
    val step = s0 >>= H
    println(step)

    assert(step.bins.toSet == Set("|0>" -> sq, "|1>" -> sq))

    val s1 = QState[String](S1)
    val step1 = s1 >>= H
    println(step1)

    assert(step1.bins.toSet == Set("|0>" -> sq, "|1>" -> -sq))

  }
}
