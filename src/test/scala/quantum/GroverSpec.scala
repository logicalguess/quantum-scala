package quantum

import org.specs2.mutable.Specification

class GroverSpec extends Specification {

  "Grover algorithm" should {

    "find 3" in {
      Grover.run(3) === 3
    }

    "find 5" in {
      Grover.run(5) === 5
    }
  }
}
