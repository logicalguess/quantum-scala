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
      case (b, v) => (b, v / sum)
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

    val change = List("a" -> 0.2, "b" -> 0.8, "c" -> 0.0) // likelihoods of a given data point
    assert(total(change) == 1.0)
    assert(total(fMap(bins, Map("a" -> List("a" -> 0.2), "b" -> List("b" -> 0.8), "c" -> List("c" -> 0.0)))) == 1.0)
    assert(total(fMap(bins, Map("a" -> change, "b" -> Nil, "c" -> Nil))) == 1.0)
  }


  implicit def likelihoodFunctionToChange[K](lf: Function[K, Double])(implicit hypos: List[K]): Map[K, List[(K, Double)]] = {
    hypos.map { h => (h, lf(h)) }
    //val z: List[(K, List[(K, Double)])] = Nil
    //ls.foldLeft(z) { case (buffer, (b, v)) => buffer ++ List((b -> List((b -> v)))) }.toMap
  }

  implicit def likelihoodMapToChange[K](ls: List[(K, Double)]): Map[K, List[(K, Double)]] = {
    val z: List[(K, List[(K, Double)])] = Nil
    ls.foldLeft(z) { case (buffer, (b, v)) => buffer ++ List((b -> List((b -> v)))) }.toMap
  }

  "2" should "" in {

    val bins: List[(String, Double)] = List("a" -> 0.1, "b" -> 0.3, "c" -> 0.6)
    val p = PState(bins)

    val change = Map("a" -> List("a" -> 0.2), "b" -> List("b" -> 0.7), "c" -> List("c" -> 0.0))
    println(p >>= change)

    val likelihoods = List("a" -> 0.2, "b" -> 0.7, "c" -> 0.0) // likelihoods of certain data point
    println(p >>= likelihoods)

    assert((p >>= likelihoods) == (p >>= change))
  }

  def printChart[K](hist: Map[K, Double])(implicit ord: Ordering[K]) {
    def pad(str: String, n: Int): String =
      if (str.length > n) str.substring(0, n) else str + (" " * (n - str.length))

    if (hist.nonEmpty) {
      val keyLen = hist.keys.map(_.toString.length).max
      hist.toSeq.sortBy(_._1).map {
        case (h, prob) =>
          pad(h.toString, keyLen).mkString + " " +
            pad(prob.toString, 6) + " " +
            ("#" * (50 * prob).toInt)
      }.foreach(println)
    }
  }

  def printChart[K](state: PState[K])(implicit ord: Ordering[K]): Unit = {
    state match {
      case PState(bins) => printChart(bins.toMap)
    }
  }

  "3" should "dice" in {
    def likelihood(data: Int)(hypo: Int) =
      if (hypo < data) 0.0 else 1.0 / hypo

    val hypos = List(4, 6, 8, 12, 20)

    implicit def dataPointToChange(data: Int) =
      likelihoodFunctionToChange(likelihood(data))(hypos)

    val bins: List[(Int, Double)] = hypos.map { h => (h, 1.0) }
    val prior = PState(bins).normalize()
    println("Priors:")
    printChart(prior)

    println()
    println("After a 6 is rolled:")
    val posterior = prior >>= 6
    printChart(posterior)

    println()
    println("After 6, 8, 7, 7, 5, 4 are rolled after the first 6:")
    val posterior2 = posterior >>= 6 >>= 8 >>= 7 >>= 7 >>= 5 >>= 4
    printChart(posterior2)

  }
}
