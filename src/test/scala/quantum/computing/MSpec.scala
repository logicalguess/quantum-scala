package quantum.computing

import org.scalatest.FlatSpec

class MSpec extends FlatSpec {
  "1" should "" in {
    val bins: List[(String, Double)] = List("a" -> .2, "b" -> .1, "c" -> .3, "d" -> .4)

    val m = MState[String](bins)
    val changeA = List("a" -> .25, "b" -> .5, "c" -> .25)
    val changeB = List("b" -> 1.0)
    val changeC = List("c" -> 1.0)
    val changeD = List("d" -> 1.0)

    val state = m >>= Map("a" -> changeA, "b" -> changeB, "c" -> changeC, "d" -> changeD)

    assert(state.bins.toSet == Set("a" -> .05, "b" -> .2, "c" -> .35, "d" -> .4))
  }
}
