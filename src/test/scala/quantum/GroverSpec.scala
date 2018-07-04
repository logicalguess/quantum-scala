package quantum

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class GroverSpec extends FlatSpec with GeneratorDrivenPropertyChecks {

  implicit val searchValues: Arbitrary[Int] = Arbitrary {
    for {
      v <- Gen.choose[Int](0, 99)
    } yield v
  }

  "Grover algorithm" should "find any value" in forAll  { v: Int =>
      println("input: " + v)
      Grover.run(v) === v
    }
}
