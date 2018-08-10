package quantum.computing

import org.scalatest.FlatSpec

class ZeroSumSpec extends FlatSpec {

  def collect(bins: List[(String, Int)]): List[(String, Int)] = {
    bins.groupBy(_._1).toList.map {
      case (b, vs) => (b, vs.map(_._2).foldLeft(0)(_ + _))
    }
  }

  def flatMap(bins: List[(String, Int)], f: PartialFunction[String, List[(String, Int)]]) =
    collect(bins.flatMap({
      case (b, v) if f.isDefinedAt(b) => List((b -> v)) ++ f(b)
      // case (b, v) if f.isDefinedAt(b) => List((b -> (v - total(f(b))))) ++ f(b)
      case (b, v) => List((b -> v))
    }))

  def fMap(bins: List[(String, Int)], f: String => List[(String, Int)]) =
    collect(bins.flatMap({ case (b, v) => List((b -> v)) ++ f(b) }))

  def total(bins: List[(String, Int)]): Int = bins.map(_._2).foldLeft(0)(_ + _)

  def ><(bin1: List[(String, Int)], bin2: List[(String, Int)]): String => List[(String, Int)] = {
    val m = collect(bin2).toMap
    b => bin1.map { case (b1, v1) => (b1, v1 - m.getOrElse(b, 0)) }
  }

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

  "2" should "" in {
    val bins: List[(String, Double)] = List("a" -> 2, "b" -> 3, "c" -> 5, "d" -> -8, "e" -> -2)

    val z = ZState[String](bins)
    val change = List("a" -> -1.0, "b" -> 1.0)

    val step = z.>>=(Map("a" -> change, "b" -> Nil, "c" -> Nil, "d" -> Nil, "e" -> Nil))

    assert(step.bins.contains("a" -> 1.0))
    assert(step.bins.contains("b" -> 4.0))
  }

  "3" should "" in {
    val bins: List[(String, Double)] = List("a" -> 2, "b" -> 3, "c" -> 5, "d" -> -8, "e" -> -2)

    val z = OState[String](bins)
  }
}
