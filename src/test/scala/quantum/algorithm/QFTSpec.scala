package quantum.algorithm

import org.scalatest.FlatSpec

import quantum.domain.QState._
import quantum.domain.Symbol.Word

class QFTSpec extends FlatSpec {

  "QFT algorithm" should "run" in  {
    println(QFT.run)
  }

  "fourier" should "" in {
    def qft(qs: List[Int]) = {
      for (j <- (0 to qs.size - 1).reverse) {
        println(qs(j))
        for (k <- (0 to j - 1).reverse)
          println("\t" + qs(k) + "  R" + (j - k))
      }
    }

    qft((1 to 5).toList.reverse)
  }

  "compare" should "work" in {

    val s = pure(Word.fromInt(0, 4))
    assert((s >>= QFT.qft) == (s >>= QFT.QFT))
    assert((s * rhalf >>= QFT.qft) == (s * rhalf >>= QFT.QFT))

    val s1 = pure(Word.fromInt(8, 4))
    assert((s1 >>= QFT.qft) == (s1 >>= QFT.QFT))
    assert((s1 * rhalf >>= QFT.qft) == (s1 * rhalf >>= QFT.QFT))

    val s2 = pure(Word.fromInt(0, 4)) + pure(Word.fromInt(8, 4))
    assert((s2 >>= QFT.qft) == (s2 >>= QFT.QFT))
    assert((s2 * rhalf >>= QFT.qft) == (s2 * rhalf >>= QFT.QFT))

    assert((s2 >>= QFT.qft) == (QFT.qftS((0 to s.state.head._1.letters.size - 1).toList)(s2)))
  }

}
