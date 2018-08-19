package quantum.computing

trait UState[+This <: UState[This, B, V], B, V] {
  val bins: List[(B, V)]
  val zero: V
  val plus: (V, V) => V

  val normalizeRule: List[(B, V)] => List[(B, V)] = identity
  val combineRule: List[(B, V)] => List[(B, V)] = { bs =>
    bs.groupBy(_._1).toList.map {
      case (b, vs) => (b, vs.map(_._2).foldLeft(zero)(plus))
    }
  }
  val updateRule: ((B, V), B => List[(B, V)]) => List[(B, V)]

  def create(bins: List[(B, V)]): This

  def normalize(): This = create(normalizeRule(bins))

  def flatMap(f: B => List[(B, V)]): This = {
    create(normalizeRule(combineRule(bins.flatMap({ case bv => updateRule(bv, f) }))))
  }

  def >>=(f: B => List[(B, V)]): This = flatMap(f)
}

case class ZState[B](bins: List[(B, Double)]) extends UState[ZState[B], B, Double] {
  override val zero: Double = 0.0
  override val plus: (Double, Double) => Double = _ + _

  override val updateRule: ((B, Double), B => List[(B, Double)]) => List[(B, Double)] = {
    case ((b, v), f) => List((b -> v)) ++ f(b)
  }

  override def create(bins: List[(B, Double)]) = ZState(bins)
}

case class OState[B](bins: List[(B, Double)]) extends UState[OState[B], B, Double] {
  override val zero: Double = 0.0
  override val plus: (Double, Double) => Double = _ + _

  override val updateRule: ((B, Double), B => List[(B, Double)]) => List[(B, Double)] = {
    case ((b, v), f) => List((b -> v)) ++ f(b)
  }

  override def create(bins: List[(B, Double)]) = OState(bins)
}

case class PState[B](bins: List[(B, Double)]) extends UState[PState[B], B, Double] {
  override val zero: Double = 1.0
  override val plus: (Double, Double) => Double = _ * _

  override val updateRule: ((B, Double), B => List[(B, Double)]) => List[(B, Double)] = {
    case ((b, v), f) => f(b).map { case (c, u) => (c, u * v) }
  }

  override val normalizeRule = { bs: List[(B, Double)] =>
    val sum = bs.map(_._2).foldLeft(0.0)(_ + _)
    if (sum == 1.0) bins else bs.map {
      case (b, v) => (b, v / sum)

    }
  }

  override def create(bins: List[(B, Double)]) = PState(bins)
}

import quantum.domain.Complex

case class QState[B](bins: List[(B, Complex)]) extends UState[QState[B], B, Complex] {
  override val zero: Complex = Complex.zero
  override val plus: (Complex, Complex) => Complex = Complex.plus

  override val updateRule: ((B, Complex), B => List[(B, Complex)]) => List[(B, Complex)] = {
    case ((b, v), f) => f(b).map { case (c, u) => (c, u * v) }
  }

  override def create(bins: List[(B, Complex)]) = QState(bins)
}
