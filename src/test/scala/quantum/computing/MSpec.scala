package quantum.computing

import org.scalatest.FlatSpec

class MSpec extends FlatSpec {
  "1" should "" in {
    val bins: List[(String, Double)] = List("a" -> .1, "b" -> .2, "c" -> .3, "d" -> .4)

    val m = MState[String](bins)
    val changeA = List("a" -> .5, "b" -> .25, "c" -> .25)
    val changeB = List("b" -> 1.0)
    val changeC = List("c" -> 1.0)
    val changeD = List("d" -> 1.0)

    val step = m >>= Map("a" -> changeA, "b" -> changeB, "c" -> changeC, "d" -> changeD)

    println(step.bins)

    assert(step.bins.contains("a" -> .05))
    assert(step.bins.contains("b" -> .225))
    assert(step.bins.contains("c" -> .325))
    assert(step.bins.contains("d" -> .4))

  }
}
