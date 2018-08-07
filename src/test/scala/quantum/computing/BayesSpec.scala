package quantum.computing

import org.scalatest.FlatSpec

class BayesSpec extends FlatSpec {

  def collect(bins: List[(String, Double)]): List[(String, Double)] = {
    bins.groupBy(_._1).toList.map {
      case (b, vs) => (b, vs.map(_._2).foldLeft(1.0)(_ * _))
    }
  }

  def normalize(bins: List[(String, Double)]): List[(String, Double)] = {
    val sum = total(bins)
    if (sum == 1.0) bins else bins.map {
      case (b, v) => (b, v/sum)
    }
  }

  def fMap(bins: List[(String, Double)], f: String => List[(String, Double)]) =
    normalize(collect(bins.flatMap({ case (s, i) => List((s -> i)) ++ f(s) })))

  def total(bins: List[(String, Double)]): Double = bins.map(_._2).foldLeft(0.0)(_ + _)

  "1" should "" in {
    val bins: List[(String, Double)] = List("a" -> 0.1, "b" -> 0.3, "c" -> 0.6)

    assert(total(bins) == 1.0)
    assert(normalize(bins) == bins)
    assert((collect(bins ++ List("a" -> 0.2)).filter(t => (t._1 == "a")).head._2 - 0.2) < 0.0001)

    val change = List("a" -> 0.2, "b" -> 0.8, "c" -> 0.0) // likelihoods of certain data point
    assert(total(change) == 1.0)
    assert(total(fMap(bins, Map("a" -> List("a" -> 0.2), "b" -> List("b" -> 0.8), "c" -> List("c" -> 0.0)))) == 1.0)
    assert(total(fMap(bins, Map("a" -> change, "b" -> Nil, "c" -> Nil))) == 1.0)

    val change_a = List("a" -> 0.2, "b" -> 0.4, "c" -> 0.4)
    val change_b = List("a" -> 0.1, "b" -> 0.8, "c" -> 0.1)
    val change_c = List("a" -> 0.5, "b" -> 0.5, "c" -> 0.0)

    //assert(fMap(bins, Map("a" -> change, "b" -> Nil, "c" -> Nil)) == fMap(bins, Map("a" -> change_a, "b" -> change_b, "c" -> change_c)))

  }
}
