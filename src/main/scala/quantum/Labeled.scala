package quantum

trait Labeled {
  val label: String
  override def toString = label
}

object Labeled {

  implicit def labelOrdering[B <: Labeled] = Ordering.by[B, String](_.label)

  // Tensor product of two bases, e.g., T[Std, Std] = { |00>, |01>, |10>, |11> }
  case class Tensor[+B1 <: Labeled, +B2 <: Labeled](_1: B1, _2: B2) extends Labeled {
    val label = _1.label + "," + _2.label
  }

  implicit def tensorOrdering[B1 <: Labeled, B2 <: Labeled](implicit ord1: Ordering[B1], ord2: Ordering[B2]): Ordering[Tensor[B1, B2]] = {
    (x: Tensor[B1, B2], y: Tensor[B1, B2]) => {
      ord1.compare(x._1, y._1) match {
        case 0 => ord2.compare(x._2, y._2)
        case a => a
      }
    }
  }

}
