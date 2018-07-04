package quantum

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class GroverSpec extends FlatSpec with GeneratorDrivenPropertyChecks {

  val report: QState[_ <: Labeled] => Unit = s => {
    println("Iteration state: " + s)
    s.hist
  }

  "Grover algorithm" should "find 5" in  {
    assert(Grover.run(5)(report) == 5)
  }

  implicit val searchValues: Arbitrary[Int] = Arbitrary {
    for {
      v <- Gen.choose[Int](0, 99)
    } yield v
  }

  "Grover algorithm" should "find any value" in forAll  { v: Int =>
      println("input: " + v)
      assert(Grover.run(v) == v)
    }
}
