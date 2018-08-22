package quantum.algorithm

import quantum.domain.Gate._
import quantum.domain.QState._
import quantum.domain.{Gate, Labeled, QState}
import quantum.domain.Symbol.{S0, S1, Std, Word}

import scala.language.reflectiveCalls

object Grover {

  val noSideEffect: Any => Unit = { _ => }

  def iterate[A](n: Int, a: A)(f: A => A)(implicit sideEffect: A => Unit = noSideEffect): A = {
    sideEffect(a)
    if (n <= 0) a
    else iterate(n-1, f(a))(f)(sideEffect)
  }

  /**
   * Grover's algorithm
   */
  def grover(f: Int => Int, width: Int)(implicit sideEffect: QState[_ <: Labeled] => Unit = noSideEffect) = {
    val Hn: Gate[Word[Std], Word[Std]] = liftWord(H) _
    val zeroes = pure(Word.fromInt(0, width))
    val one = pure(Word.fromInt(1, 1))
    val inv = U(f)
    val refl = {
      val s = zeroes >>= Hn
      (s >< s) * 2.0 - I[Word[Std]]
    }

    val r = (math.Pi * math.sqrt(math.pow(2, width)) / 4).toInt
    // zeroes * one >>= lift12(Hn, Hn) >>= repeat(r)(inv >=> lift1(refl))
    val init = zeroes * one >>= lift12(Hn, Hn)
    iterate(r, init)(_ >>= (inv >=> lift1(refl)))(sideEffect)
  }

  def invS(qs: List[Int])(s: QState[Word[Std]]): QState[Word[Std]] = {
    val size = qs.size
    var state = s
    for (j <- (0 to size - 1)) {
      state = state >>= wire(qs(j), H) >>= wire(qs(j), X)
    }
    state = state >>= controlledL((0 to size - 2).map(qs).toSet, qs(size - 1), rot(math.Pi))
    for (j <- (0 to size - 1)) {
      state = state >>= wire(qs(j), X) >>= wire(qs(j), H)
    }
    -state
  }

  def invL(qs: List[Int])(s: Word[Std]): QState[Word[Std]] = invS(qs)(pure(s))

  def inv(s: Word[Std]): QState[Word[Std]] = {
    invL((0 to s.letters.size - 1).toList)(s)

//    val size = s.letters.size
//    var state = pure(s)
//    for (j <- (0 to size - 1)) {
//      state = state >>= wire(j, H) >>= wire(j, X)
//    }
//    state = state >>= controlledL((0 to size - 2).toSet, size - 1, rot(math.Pi))
//    for (j <- (0 to size - 1)) {
//      state = state >>= wire(j, X) >>= wire(j, H)
//    }
//    -state
  }

  def oracleL(f: Int => Boolean)(q_in: List[Int], q_out: Int)(s: Word[Std]): QState[Word[Std]] = {
    val letters = s.letters

    val sub: List[Std] = letters.indices.collect { case i if q_in.contains(i) => letters(i) }.toList
    val x = Word.toInt(Word(sub))

    val a = letters(q_out) == S1
    val fx = if (a ^ f(x))  S1 else S0

    val state = pure(Word[Std](letters.updated(q_out, fx)))
    state
  }

  def oracle(f: Int => Boolean)(s: Word[Std]): QState[Word[Std]] = {
    oracleL(f)((0 to s.letters.size - 2).toList, s.letters.size - 1)(s)
//
//    val size = s.letters.size
//    val x = Word.toInt(Word(s.letters.take(size - 1)))
//    val a = s.letters.last == S1
//    val fx = if (a ^ f(x))  S1 else S0
//    val state = pure(Word[Std](s.letters.take(size - 1) ++ List(fx)))
//    state
  }

  // convert an oracle to one that discriminates on the last bit
  def oracleLQ(f: Int => Boolean)(s: Word[Std]): QState[Word[Std]] = {
    oracleLQQ(f)((0 until s.letters.size - 1).toList, s.letters.size - 1)(s)
//    val size = s.letters.size
//    val x = Word.toInt(Word(s.letters.take(size - 1)))
//
//    // set the last bit to 1 if oracle is true
//    val fx = if (f(x)) S1 else S0
//    val state = pure(Word[Std](s.letters.take(size - 1) ++ List(fx)))
//    state
  }

  def oracleLQQ(f: Int => Boolean)(q_in: List[Int], q_out: Int)(s: Word[Std]): QState[Word[Std]] = {
    val letters = s.letters

    val sub: List[Std] = letters.indices.collect { case i if q_in.contains(i) => letters(i) }.toList
    val x = Word.toInt(Word(sub))

    val fx = if (f(x)) S1 else S0

    val state = pure(Word[Std](letters.updated(q_out, fx)))
    state
  }

  def grover(f: Int => Boolean)(width: Int): QState[Word[Std]] = {
    val r = (math.Pi * math.sqrt(math.pow(2, width)) / 4).toInt
    var state = pure(Word[Std](List.fill(width)(S0) ++ List(S1)))

    //var state = pure(Word.fromInt(0, width + 1))
    //state = state >>= wire(width, X) //last

    for (j <- (0 to width)) {
      state = state >>= wire(j, H)
    }
    //state.hist

    for (i <- 1 to r) {
      state = state  >>= oracle(f) >>= inv
      //println("Iteration " + i)
      //state.hist
    }
    state
  }

  def run(n: Int)(implicit sideEffect: QState[_ <: Labeled] => Unit = noSideEffect): Int = {
    def f(x: Int) = if (x == n) 1 else 0
    val bits = (math.log(n) / math.log(2)).toInt + 1
    val s = grover(f, bits)(sideEffect)
    println("final state: " + s.toString)
    val m = Word.toInt(s.measure(_._1).outcome)
    println("measurement: " + m)
    m
  }
}
