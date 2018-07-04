package quantum

import org.specs2.mutable.Specification

import quantum.Complex._
import quantum.QState._
import quantum.Symbol._
import quantum.Gate._

import scala.language.reflectiveCalls

class GateSpec extends Specification {

  val state1: QState[Std] = QState(S0 -> 0.6, S1 -> 0.8.i)
  val state2: QState[Std] = QState(S0 -> -0.5, S1 -> r3quarters)

  "Gates" should {

    "HZH = X" in {
      def HZH(s: QState[Std]): QState[Std] = s >>= H >>= Z >>= H
      //val HZH = H >=> Z >=> H

      HZH(state1) === X(state1)
      HZH(state2) === X(state2)
    }
  }
}
