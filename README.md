# Quantum Computing in Scala

Modeling  Quantum Computing concepts (Qbits, quantum state, circuits, gates, measurements, etc.) in Scala, 
taking advantage of the nice language features. 
A few simple examples, and Grover's algorithm are included.

## Building Blocks

### Complex numbers

    case class Complex(val re: Double, val im: Double)

Scala features that make the implementation elegant:

* lazy val

      lazy val conj = Complex(re, -im)
      lazy val norm2 = re * re + im * im
      
* unary operators

      def unary_- = this * -1.0
    
* implicit method

      implicit def toImaginary(x: Double) = new {
        def i = new Complex(0.0, x)
      }

* implicit conversion

        implicit def toComplex(x: Double) = new Complex(x, 0.0)

### Labeled
    
    trait Labeled {
      val label: String
      override def toString = label
    }
    
Tensor (similar to Scala's Tuple2):

      case class Tensor[+L1 <: Labeled, +L2 <: Labeled](_1: L1, _2: L2) extends Labeled {
        val label = _1.label + "," + _2.label
      }
      
### Symbols and Words

    abstract class Symbol extends Labeled

* spin

      // Sign { |+>, |-> }
      abstract sealed class Sign(val label: String) extends Symbol
      case object S_+ extends Sign("+")
      case object S_- extends Sign("-")
      
* polarization

      ```scala  
      // H-V polarization
      abstract sealed class Polarization(val label: String) extends Symbol
      case object Horizontal extends Polarization("H")
      case object Vertical extends Polarization("V")
      ```
 