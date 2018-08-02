package quantum.algorithm

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import quantum.algorithm.Grover.grover
import quantum.domain.Symbol.Word
import quantum.domain.{Labeled, QState}

class GroverSpec extends FlatSpec with GeneratorDrivenPropertyChecks {

  val report: QState[_ <: Labeled] => Unit = s => {
    println("Iteration state: " + s)
    s.hist
  }

  "Grover algorithm" should "find 5" in {
    assert(Grover.run(5)(report) == 5)
  }

  implicit val searchValues: Arbitrary[Int] = Arbitrary {
    for {
      v <- Gen.choose[Int](1, 99)
    } yield v
  }

  "Grover algorithm" should "find any value" in forAll { v: Int =>
    println("input: " + v)
    assert(Grover.run(v) == v)
  }

  "grover" should "no consecutive 1s" in {
    def noConsecutiveOnes(n: Int): Boolean = (n & (n >>> 1)) == 0

    def f(x: Int) = if (noConsecutiveOnes(x)) 1 else 0

    var results = scala.collection.mutable.Map[Boolean, Int](true -> 0, false -> 0)

    val count = 100
    for (i <- 1 to count) {
      val s = grover(f, 4)

      println("final state: " + s.toString)
      val m = Word.toInt(s.measure(_._1).outcome)
      println("measurement: " + m + " -> " + Integer.toBinaryString(m))
      results(noConsecutiveOnes(m)) = results(noConsecutiveOnes(m)) + 1
    }

    println(results)
  }
}
