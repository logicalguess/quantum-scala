package quantum.computing

import org.scalatest.FlatSpec
import quantum.domain.Complex

class QSpec extends FlatSpec {

  def collect(bins: List[(String, Complex)]): List[(String, Complex)] = {
    bins.groupBy(_._1).toList.map {
      case (b, vs) => (b, vs.map(_._2).foldLeft(Complex.one)(Complex.plus))
    }
  }

  def fMap(bins: List[(String, Complex)], f: String => List[(String, Complex)]) =
    collect(bins.flatMap({ case (b, v) => f(b).map { case (c, u) => (c, v*u) } }))

  def total(bins: List[(String, Complex)]): Double = math.sqrt(bins.map(_._2.norm2).foldLeft(0.0)(_ + _))

  def ><(bin1: List[(String, Complex)], bin2: List[(String, Complex)]): String => List[(String, Complex)] = {
    val m = collect(bin2).toMap
    b => bin1.map { case (b1, v1) => (b1, v1 * m.getOrElse(b, Complex.zero).conj) }
  }

  "1" should "" in {
    val bins: List[(String, Complex)] = List("0" -> 1 / math.sqrt(2), "1" -> 1 / math.sqrt(2))

    assert((total(bins) - 1.0) < 0.001)

  }
}
