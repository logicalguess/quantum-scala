package quantum.computing

trait UState[S <: UState[S, B, V], B, V] {
  val bins: List[(B, V)]
  val process: ((B, V), B => List[(B, V)]) => List[(B, V)]
  val zero: V
  val plus: (V, V) => V

  def create(bins: List[(B, V)]): S

  def transform(f: B => List[(B, V)]): S = {
    create(collect(bins.flatMap({ case bv => process(bv, f) })))
  }

  private def collect(bins: List[(B, V)]): List[(B, V)] = {
    bins.groupBy(_._1).toList.map {
      case (b, vs) => (b, vs.map(_._2).foldLeft(zero)(plus))
    }
  }
}

case class ZState[B](bins: List[(B, Double)]) extends UState[ZState[B], B, Double] {
  override val zero: Double = 0.0
  override val plus: (Double, Double) => Double = _ + _

  override val process: ((B, Double), B => List[(B, Double)]) => List[(B, Double)] = {
    case ((b, v), f) => List((b -> v)) ++ f(b)
  }

  override def create(bins: List[(B, Double)]): ZState[B] = ZState(bins)
}

import quantum.domain.Complex

case class QState[B](bins: List[(B, Complex)]) extends UState[QState[B], B, Complex] {
  override val zero: Complex = Complex.zero
  override val plus: (Complex, Complex) => Complex = Complex.plus

  override val process: ((B, Complex), B => List[(B, Complex)]) => List[(B, Complex)] = {
    case ((b, v), f) => f(b).map { case (c, u) => (c, u * v) }
  }

  override def create(bins: List[(B, Complex)]): QState[B] = QState(bins)
}