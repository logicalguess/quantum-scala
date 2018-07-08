# Quantum Computing in Scala

Modeling  Quantum Computing concepts (Qbits, quantum state, circuits, gates, measurements, etc.) in Scala, 
taking advantage of the nice language features. 
A few simple examples, and Grover's algorithm are included.

## Basic Concepts

### Complex numbers

```scala
case class Complex(val re: Double, val im: Double)
```

Scala features that make the implementation elegant:

* lazy val

```scala
lazy val conj = Complex(re, -im)
lazy val norm2 = re * re + im * im
```
      
* unary operators

```scala
def unary_- = this * -1.0
 ```
    
* implicit method

```scala
implicit def toImaginary(x: Double) = new {
  def i = new Complex(0.0, x)
}
```

* implicit conversion

```scala 
implicit def toComplex(x: Double) = new Complex(x, 0.0)
```

### Labeled

```scala     
trait Labeled {
    val label: String
    override def toString = label
}
```
    
Tensor (similar to Scala's Tuple2):

```scala 
case class Tensor[+L1 <: Labeled, +L2 <: Labeled](_1: L1, _2: L2) extends Labeled {
    val label = _1.label + "," + _2.label
}
```
      
### Symbols and Words

Symbols or letters represent the units of information:

```scala 
abstract class Symbol extends Labeled
```

* spin

```scala  
// Sign { |+>, |-> }
abstract sealed class Sign(val label: String) extends Symbol
case object S_+ extends Sign("+")
case object S_- extends Sign("-")
```
      
* polarization

```scala  
// H-V polarization
abstract sealed class Polarization(val label: String) extends Symbol
case object Horizontal extends Polarization("H")
case object Vertical extends Polarization("V")
```
 
 * binary
 
```scala
// Standard { |0>, |1> }
abstract sealed class Std(val label: String) extends Symbol
case object S0 extends Std("0")
case object S1 extends Std("1")
```
 
Words (symbol strings) represent outcomes or final states:

```scala
case class Word[B <: Symbol](letters: List[B]) extends Labeled {
  val label = letters.map(_.label).mkString
}
```

## Quantum Concepts

### Quantum State

A mapping that associates a complex number (amplitude) to each possible outcome:

```scala
case class QState[A <: Labeled](state: (A, Complex)*)
```

If outcomes are repeated the amplitudes are combined:

```scala
// Collect like terms and sum their coefficients
private def collect: QState[A] = {
  QState(state.groupBy(_._1).toList.map {
    case (a, azs) => (a, azs.map(_._2).foldLeft(Complex.zero)(Complex.plus))
  }: _*)
}
```

Quantum state is monadic:

* map

```scala
private def map[B <: Labeled](f: A => B): QState[B] = {
  QState(state.map { case (a, z) => (f(a), z) }: _*).collect
}
```

* flatMap ( >>= )

```scala
def flatMap[B <: Labeled](f: A => QState[B]): QState[B] = {
  QState(state.flatMap { case (a, z) => f(a).mapV(_ * z).state }: _*).collect
}

private def mapV(f: Complex => Complex): QState[A] = {
  QState(state.map { case (a, z) => (a, f(z)) }: _*)
}
```

* pure (single certain outcome)

```scala
def pure[A <: Labeled](a: A): QState[A] = new QState(a -> Complex.one)
```

Quantum state is collapsed into one of possible outcomes when measured. 

```scala
case class Measurement[A <: Labeled, B](outcome: B, newState: QState[A])
```

In the most common case the new state is the pure representation of the outcome.

Products of quantum states:

* inner ( <>)

```scala
// Inner product
def inner(that: QState[A]): Complex = {
  this.state.map { case (l, v) => v.conj * that(l) }.foldLeft(Complex.zero)(Complex.plus)
}
```

* outer ( >< )

```scala
// Outer product
def outer[B <: Labeled](that: QState[B]): B => QState[A] = {
  (b: B) => this * that(b).conj
}
```

* tensor

```scala
// Tensor product
def *[B <: Labeled](that: QState[B]): QState[Tensor[A, B]] = {
  for {
    x <- this
    y <- that
  } yield Tensor(x, y)
}
```