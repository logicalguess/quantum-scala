package quantum.domain

import quantum.domain.Labeled.Tensor
import quantum.domain.Symbol._

case class QState[A <: Labeled](state: (A, Complex)*)(implicit ord: Ordering[A] = null) {
  private val rand = new scala.util.Random()
  private val _m = state.toMap

  def apply(a: A): Complex = _m.getOrElse(a, Complex.zero)

  private def map[B <: Labeled](f: A => B)(implicit ord1: Ordering[B] = null): QState[B] = {
    QState(state.map { case (a, z) => (f(a), z) }: _*).collect
  }

  private def mapV(f: Complex => Complex): QState[A] = {
    QState(state.map { case (a, z) => (a, f(z)) }: _*)
  }

  // Collect like terms and sum their coefficients
  private def collect: QState[A] = {
    QState(state.groupBy(_._1).toList.map {
      case (a, azs) => (a, azs.map(_._2).foldLeft(Complex.zero)(Complex.plus))
    }: _*)
  }

  def flatMap[B <: Labeled](f: A => QState[B]): QState[B] = {
    QState(state.flatMap { case (a, z) => f(a).mapV(_ * z).state }: _*).collect
  }

  def >>=[B <: Labeled](f: A => QState[B]): QState[B] = this.flatMap(f)

  def unary_- : QState[A] = this * -1.0

  def *(z: Complex): QState[A] = this.mapV(_ * z)

  def /(z: Complex): QState[A] = this.mapV(_ / z)

  def +(that: QState[A]): QState[A] = QState(this.state ++ that.state: _*).collect

  def -(that: QState[A]): QState[A] = this + -that

  // Inner product
  def inner(that: QState[A]): Complex = {
    this.state.map { case (l, v) => v.conj * that(l) }.foldLeft(Complex.zero)(Complex.plus)
  }

  def <>(that: QState[A]): Complex = this.inner(that)

  // Outer product
  def outer[B <: Labeled](that: QState[B]): B => QState[A] = {
    (b: B) => this * that(b).conj
  }

  def ><[B <: Labeled](that: QState[B]): B => QState[A] = this.outer(that)

  // Tensor products
  def *[B <: Labeled](that: QState[B]): QState[Tensor[A, B]] = {
    for {
      x <- this
      y <- that
    } yield Tensor(x, y)
  }

  def *:[B <: Symbol](that: QState[B])(implicit ev: A =:= Word[B]): QState[Word[B]] = {
    for {
      x <- that
      y <- this
    } yield Word(x :: ev(y).letters)
  }


  def transform[B <: Symbol](t: List[B] => List[B])(implicit ev: A =:= Word[B]): QState[Word[B]] = {
    for {
      x <- this
    } yield Word(t(ev(x).letters))
  }

  private def filter(f: A => Boolean): QState[A] = {
    QState(state.filter { case (a, z) => f(a) }: _*).normalize
  }

  // Make sure the sum of the squares of the coefficients is 1
  def normalize = {
    val total = math.sqrt(state.map { case (a, z) => z.norm2 }.sum)
    this / total
  }

  def toDist: List[(A, Double)] = {
    this.state.toList.map { case (a, z) => a -> z.norm2 }
  }

  def probs = {
    val probs = for  {
      x <- this.state.sortBy(_._1)
    } yield "|" + x._1 + ">  " + x._2.norm2
    probs.foreach(println)
  }

  def hist(implicit ord: Ordering[A]) {
    plotHist(this.toDist)
  }

  private def plotHist[B](values: Seq[(B, Double)])(implicit ord: Ordering[B]) {
    val maxWidth = values.map(_._1.toString.size).max
    val maxValue = values.map(_._2).max
    val hashesPerUnit = 50 / maxValue
    values.sortBy(_._1).foreach { case (a, p) => {
      val fmt = "%" + maxWidth + "s %s"
      val hashes = (hashesPerUnit * p).toInt
      println(fmt.format(a, "#" * hashes))
    }
    }
  }

  case class Measurement[A <: Labeled, B](outcome: B, newState: QState[A])

  // Measure a quantum state (or a part of one). Returns the outcome of the measurement and the new state.
  def measure[B](w: A => B = identity[A] _): Measurement[A, B] = {
    val dist = this.toDist
    val total = dist.map(_._2).sum
    val r = rand.nextDouble() * total

    def find(r: Double, s: List[(A, Double)]): A = s match {
      case (l, p) :: Nil => l
      case (l, p) :: rest if r < p => l
      case (l, p) :: rest => find(r - p, rest)
      case Nil => throw new Exception("empty state")
    }

    val outcome = w(find(r, dist))
    val newState = this.filter(s => w(s) == outcome)
    Measurement(outcome, newState)
  }

  def plotMeasurements[B](n: Int, w: A => B = identity[A] _)(implicit ord: Ordering[B]) {
    val measurements = (1 to n).map(_ => this.measure(w).outcome).groupBy(x => x).mapValues(_.size.toDouble)
    val Labeled = this.state.map { case (a, z) => w(a) }.distinct
    plotHist(Labeled.map(b => b -> measurements.getOrElse(b, 0.0)))
  }

  override def toString = {
    val filtered = state.filter { case (a, z) => z.norm2 > 0.00000001 }
    if (filtered.isEmpty) "0"
    else {
      val sorted = if (ord == null) filtered.sortBy { case (a, z) => a.toString } else filtered.sortBy { case (a, z) => a }
      sorted.map { case (a, z) => {
        val zStr = z.toString
        val zDisplay = if (zStr == "1") "" else zStr
        s"$zDisplay|$a>"
      }
      }.mkString(" + ")
    }
  }

  override def equals(obj: scala.Any): Boolean = obj.isInstanceOf[QState[A]] && this.toString.equals(obj.toString)
}

object QState {
  val rhalf: Complex = math.sqrt(0.5)
  val rquarter: Complex = math.sqrt(0.25)
  val r3quarters: Complex = math.sqrt(0.75)

  def pure[A <: Labeled](a: A): QState[A] = new QState(a -> Complex.one)

  // Some pure states
  val s0: QState[Std] = pure(S0)
  val s1: QState[Std] = pure(S1)

  val plus: QState[Std] = QState(S0 -> rhalf, S1 -> rhalf)
  val minus: QState[Std] = QState(S0 -> rhalf, S1 -> -rhalf)

  val s_+ = pure(S_+)
  val s_- = pure(S_-)
}
