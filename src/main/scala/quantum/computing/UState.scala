package quantum.computing

trait Monoid[V] {
  val empty: V
  val combine: (V, V) => V
}

trait UState[+This <: UState[This, B, V], B, V] {
  protected val bins: List[(B, V)]
  protected val m: Monoid[V]

  protected val normalizeStateRule: List[(B, V)] => List[(B, V)] = identity

  protected val combineBinsRule: List[(B, V)] => List[(B, V)] = { bs =>
    bs.groupBy(_._1).toList.map {
      case (b, vs) => (b, vs.map(_._2).foldLeft(m.empty)(m.combine))
    }
  }
  protected val updateStateRule: ((B, V), B => List[(B, V)]) => List[(B, V)]

  def create(bins: List[(B, V)]): This

  def normalize(): This = create(normalizeStateRule(bins))

  def flatMap(f: B => List[(B, V)]): This = {
    val updates: List[(B, V)] = bins.flatMap({ case bv => updateStateRule(bv, f) })
    create(normalizeStateRule(combineBinsRule(updates)))
  }

  def >>=(f: B => List[(B, V)]): This = flatMap(f)
}

case class ZState[B](bins: List[(B, Double)]) extends UState[ZState[B], B, Double] {
  val m = new Monoid[Double] {
    override val empty: Double = 0.0
    override val combine: (Double, Double) => Double = _ + _
  }
  override val updateStateRule: ((B, Double), B => List[(B, Double)]) => List[(B, Double)] = {
    case ((b, v), f) => List((b -> v)) ++ f(b)
  }

  override def create(bins: List[(B, Double)]) = ZState(bins)
}

case class OState[B](bins: List[(B, Double)]) extends UState[OState[B], B, Double] {
  val m = new Monoid[Double] {
    override val empty: Double = 0.0
    override val combine: (Double, Double) => Double = _ + _
  }

  override val updateStateRule: ((B, Double), B => List[(B, Double)]) => List[(B, Double)] = {
    case ((b, v), f) => List((b -> v)) ++ f(b)
  }

  override def create(bins: List[(B, Double)]) = OState(bins)
}

case class MState[B](bins: List[(B, Double)]) extends UState[MState[B], B, Double] {
  val m = new Monoid[Double] {
    override val empty: Double = 0.0
    override val combine: (Double, Double) => Double = _ + _
  }

  override val updateStateRule: ((B, Double), B => List[(B, Double)]) => List[(B, Double)] = {
    case ((b, v), f) => f(b).map { case (c, u) => (c, u * v) }
  }

  override def create(bins: List[(B, Double)]) = MState(bins)
}

case class PState[B](bins: List[(B, Double)]) extends UState[PState[B], B, Double] {
  val m = new Monoid[Double] {
    override val empty: Double = 1.0
    override val combine: (Double, Double) => Double = _ * _
  }

  override val updateStateRule: ((B, Double), B => List[(B, Double)]) => List[(B, Double)] = {
    case ((b, v), f) => f(b).map { case (c, u) => (c, u * v) }
    //case ((b, v), f) => List((b -> v)) ++ f(b)  // both work
  }

  override val normalizeStateRule = { bs: List[(B, Double)] =>
    val sum = bs.map(_._2).foldLeft(0.0)(_ + _)
    if (sum == 1.0) bins else bs.map {
      case (b, v) => (b, v / sum)

    }
  }

  override def create(bins: List[(B, Double)]) = PState(bins)
}

import quantum.domain.Complex

case class QState[B](bins: List[(B, Complex)]) extends UState[QState[B], B, Complex] {
  val m = new Monoid[Complex] {
    override val empty: Complex = Complex.zero
    override val combine: (Complex, Complex) => Complex = Complex.plus
  }

  override val updateStateRule: ((B, Complex), B => List[(B, Complex)]) => List[(B, Complex)] = {
    case ((b, v), f) => f(b).map { case (c, u) => (c, u * v) }
  }

  override def create(bins: List[(B, Complex)]) = QState(bins)
}
