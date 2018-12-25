package quantum.algorithm

import org.scalatest.FlatSpec
import quantum.algorithm.Grover._
import quantum.domain.Gate
import quantum.domain.Gate.{H, I, Z, controlledL, rot, wire, _}
import quantum.domain.QState.pure
import quantum.domain.Symbol.{Std, Word}

class AmplitudeSpec extends FlatSpec {

  "count" should "phase" in {

    //def f(x: Int) = true
    //def f(x: Int) = x == 3
    //def f(x: Int) = x <= 1
    def f(n: Int): Boolean = (n & (n >>> 1)) == 0

    val n_controls = 4
    val controls = (0 until n_controls).toList

    val n_targets = 3
    val targets = (0 until n_targets).toList

    val A = oracleLQQ(f)(targets.map { i => i + n_controls }, n_targets + n_controls) _

    val g = wire(n_targets + n_controls, Z) _ >=> oracleL(f)(targets.map { i => i + n_controls }, n_targets + n_controls) >=>
      wire(n_targets + n_controls, Z) >=> invL(targets.map { i => i + n_controls })

    var state = pure(Word.fromInt(0, n_controls + n_targets + 1))

    for (j <- (0 until n_controls)) {
      state = state >>= wire(j, H)
    }
    //state = state >>= QFT.qftL(controls)

    for (j <- (n_controls until n_controls + n_targets)) {
      state = state >>= wire(j, H)
    }

    state = state >>= A

    for (i <- 0 until n_controls) {
      for (j <- 1 to math.pow(2, i).toInt)
        state = state >>= controlledI(i, g)
    }
    //state.hist

    state = state >>= QFT.iqftLR(controls)
    println(state)
    state.hist

    //val result = state.measure(w => Word.toInt(Word(w.letters.take(n_controls)))).outcome
    //println(result)
    //println(math.pow(2, n_targets)*math.pow(math.sin(math.Pi * result/math.pow(2, n_controls)), 2))

    val top = state.state.sortBy(_._2.norm2).reverse
    val ints = top.map { case (w, a) => (Word.toInt(Word(w.letters.take(n_controls))), a.norm2) }
    println(ints)

    println(ints.map { case (i, p) => (math.pow(2, n_targets)*math.pow(math.sin(math.Pi * i/math.pow(2, n_controls)), 2), p)})

  }

  "fib" should "count" in {

    val theta = math.asin(math.sqrt(0.25))

    val op: Gate[Std, Std] = rot(theta)

    val n_targets = 3

    var state = pure(Word.fromInt(0, n_targets + 1))
    //state = state >>= wire(n_targets, op)
    for (j <- (0 until n_targets)) {
      state = state >>= wire(j, H)
    }

    for (i <- 0 until n_targets - 1)  state = state >>= controlledL(Set(i), i + 1, ZERO)

    for (j <- 0 until n_targets) {
      state = state >>= controlledW(j, n_targets, rot(math.pow(2, j + 1) * theta))
    }

    state = state >>= QFT.iqftL((0 until n_targets).toList)

    state.probs
    state.hist

    val result = state.measure(w => Word.toInt(Word(w.letters.take(3)))).outcome
    println(result)
    println(math.pow(math.sin(result/math.pow(2, 3)), 2))

    val estimates = Amplitude.estimate(state)
    println(estimates)
  }

  "count" should "states" in {

    val op: Gate[Std, Std] = I

    //def f(x: Int) = true
    def f(x: Int) = x == 3
    //def f(x: Int) = x >= 4
    //def f(n: Int): Boolean = (n & (n >>> 1)) != 0

    val n_controls = 6
    val controls = (0 until n_controls).toList

    val n_targets = 4
    val targets = (0 until n_targets).toList

    def gf(shift: Int) =
      wire(n_targets + shift, Z) _ >=> oracleL(f)(targets.map { i => i + shift }, n_targets + shift) >=> wire(n_targets + shift, Z) >=> invL(targets.map { i => i + shift })

    val g = gf(n_controls)

    var state = pure(Word.fromInt(0, n_controls + n_targets + 1))

    //state = state >>= wire(n_controls + n_targets, op)

    for (j <- (0 to n_controls + n_targets)) {
      state = state >>= wire(j, H)
    }
    //state = state >>= QFT.qftL((0 to n_targets).toList)

    //for (i <- n_controls until n_controls + n_targets - 1)  state = state >>= controlledL(Set(i), i + 1, ZERO)
    //state.hist
    //println(state.state.size) // 2^n_controls * F(n_targets) * 2

    for (i <- 0 until n_controls) {
      for (j <- 1 to math.pow(2, i).toInt)
        state = state >>= controlledI(i, g)
    }
    //state.hist

    state = state >>= QFT.iqftLR(controls)
    state.hist

    //val result = state.measure(w => Word.toInt(Word(w.letters.take(n_controls)))).outcome
    //println(result)
    //println(math.pow(2, n_targets)*math.pow(math.sin(math.Pi * result/math.pow(2, n_controls)), 2))

    val top = state.state.sortBy(_._2.norm2).reverse
    val ints = top.map { case (w, a) => (Word.toInt(Word(w.letters.take(n_controls))), a.norm2) }
    println(ints)

    val factor = math.pow(2, n_targets) //2*n_targets //2*n_controls
    println(ints.map { case (i, p) => (factor*math.pow(math.sin(math.Pi * i/math.pow(2, n_controls)), 2), p)})

  }

  def trunc(x: Double, n: Int = 3): Double = {
    def p10(n: Int, pow: Long = 10): Long = if (n == 0) pow else p10(n - 1, pow * 10)

    if (n < 0) {
      val m = p10(-n).toDouble
      math.round(x / m) * m
    }
    else {
      val m = p10(n).toDouble
      math.round(x * m) / m
    }
  }
  import scala.collection.immutable.ListMap
  def highToLow(m: Map[_, Double]) = ListMap(m.mapValues(v => trunc(v)).toSeq.sortWith(_._2 > _._2):_*)

  "bias" should "be approximated" in {

    val n_controls = 7
    val controls = (0 until n_controls).toList

    val n_targets = 0
    val targets = (0 until n_targets).toList

    val bias = 0.62
    val theta = 2 * math.asin(math.sqrt(bias))
    val A: Gate[Std, Std] = Ry(theta)
    val Q = Ry(2 * theta)

    var state = pure(Word.fromInt(0, n_controls + n_targets + 1))

    state = state >>= wire(n_controls + n_targets, A)

    for (j <- (0 to n_controls + n_targets)) {
      state = state >>= wire(j, H)
    }

    for (i <- 0 until n_controls) {
      for (j <- 1 to math.pow(2, i).toInt)
        state = state >>= controlledI(i, wire(n_targets + n_controls, Q))
    }
    //state.hist

    state = state >>= QFT.iqftL(controls)
    state.hist

    val frequencies = for {
      x <- state.state
    } yield Word.toInt(l => l.take(n_controls))(x._1) -> x._2.norm2
    println(frequencies.map({ case (i, d) => (i, trunc(d)) }).groupBy(_._1))

    val combinedFrequencies = frequencies.groupBy(_._1).mapValues { l => l.foldLeft(0.0) { case (s, (k, v)) => s + v } }
    println(highToLow(combinedFrequencies))

    val sinesSquaredOfProportions = for {
      x <- state.state
    } yield (trunc(math.pow(math.sin(math.Pi * Word.toInt(l => l.take(n_controls))(x._1) / math.pow(2, n_controls)), 2)) -> x._2.norm2)
    println(sinesSquaredOfProportions.groupBy(_._1))

    val estimates = sinesSquaredOfProportions.groupBy(_._1).mapValues { l => l.foldLeft(0.0) { case (s, (k, v)) => s + v } }
    println(highToLow(estimates))

    val top2: ListMap[Double, Double] = highToLow(estimates).take(2).asInstanceOf[ListMap[Double, Double]]
    val p1 = top2.head._2
    val p2 = top2.tail.head._2
    val s = top2.head._1
    val e = top2.tail.head._1

    println(s"guess: ${s + (e - s) * p2/(p1+p2)}")
  }

}
