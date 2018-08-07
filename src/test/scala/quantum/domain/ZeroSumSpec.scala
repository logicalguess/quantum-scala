package quantum.domain

import org.scalatest.FlatSpec

class ZeroSumSpec extends FlatSpec {

  def collect(bins: List[(String, Int)]): List[(String, Int)] = {
    bins.groupBy(_._1).toList.map {
      case (a, azs) => (a, azs.map(_._2).foldLeft(0)(_ + _))
    }
  }

  def flatMap(bins: List[(String, Int)], f: PartialFunction[String, List[(String, Int)]]) =
    collect(bins.flatMap({
      case (s, i) if f.isDefinedAt(s) => List((s -> i)) ++ f(s)
      // case (s, i) if f.isDefinedAt(s) => List((s -> (i - total(f(s))))) ++ f(s)
      case (s, i) => List((s -> i))
    }))

  def fMap(bins: List[(String, Int)], f: String => List[(String, Int)]) =
    collect(bins.flatMap({ case (s, i) => List((s -> i)) ++ f(s) }))

  def total(bins: List[(String, Int)]): Int = bins.map(_._2).foldLeft(0)(_ + _)

  "1" should "" in {
    val bins: List[(String, Int)] = List("a" -> 2, "b" -> 3, "c" -> 5, "d" -> -8, "e" -> -2)

    assert(total(bins) == 0)
    assert(collect(bins ++ List("a" -> 3)).contains(("a" -> 5)))

    val change = List("a" -> -1, "b" -> 1)
    assert(total(change) == 0)

    val step1 = flatMap(bins, { case "a" => change })
    assert(total(step1) == 0)
    assert(step1.contains("a" -> 1))
    assert(step1.contains("b" -> 4))

    val step2 = flatMap(bins, Map("a" -> change))
    assert(total(step2) == 0)
    assert(step2.contains("a" -> 1))
    assert(step2.contains("b" -> 4))

    val step3 = fMap(bins, Map("a" -> change, "b" -> Nil, "c" -> Nil, "d" -> Nil, "e" -> Nil))
    assert(total(step3) == 0)
    assert(step3.contains("a" -> 1))
    assert(step3.contains("b" -> 4))
  }
}
