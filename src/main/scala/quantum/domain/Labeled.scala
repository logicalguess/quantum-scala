package quantum.domain

import quantum.domain.Labeled.Tensor

trait Labeled {
  val label: String
  override def toString = label
}

object Labeled {

  implicit def labelOrdering[L <: Labeled] = Ordering.by[L, String](_.label)

  /**
    * Tensor product of two bases, e.g., Tensor[Std, Std] = { |00>, |01>, |10>, |11> }
    *
    * @see Similar to [[Tuple2]]
    */
  case class Tensor[+L1 <: Labeled, +L2 <: Labeled](_1: L1, _2: L2) extends Labeled {
    val label = _1.label + "," + _2.label
    //override def toString = "[" + _1.toString + "," + _2.toString + "]"
  }

  implicit def tensorOrdering[L1 <: Labeled, L2 <: Labeled](implicit ord1: Ordering[L1], ord2: Ordering[L2]): Ordering[Tensor[L1, L2]] = {
    (x: Tensor[L1, L2], y: Tensor[L1, L2]) => {
      ord1.compare(x._1, y._1) match {
        case 0 => ord2.compare(x._2, y._2)
        case a => a
      }
    }
  }
}
