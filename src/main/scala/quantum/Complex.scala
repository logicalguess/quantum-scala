package quantum

case class Complex(val re: Double, val im: Double) {
  lazy val conj = Complex(re, -im)
  lazy val norm2 = re * re + im * im

  def +(z: Complex) = Complex(re + z.re, im + z.im)
  def -(z: Complex) = Complex(re - z.re, im - z.im)
  def unary_- = this * -1.0
  def *(x: Double) = Complex(re * x, im * x)
  def *(z: Complex) = Complex(re * z.re - im * z.im, re * z.im + im * z.re)
  def /(x: Double): Complex = Complex(re / x, im / x)
  def /(z: Complex): Complex = (this * z.conj) / (z * z.conj).re
  def rot(theta: Double) = this * Complex(math.cos(theta), math.sin(theta))

  private lazy val df = new java.text.DecimalFormat("#.#######")
  override def toString = {
    val reStr = df.format(re)
    val imStr = df.format(im) + "i"
    if (math.abs(im) < 0.00001) reStr
    else if (math.abs(re) < 0.00001) imStr
    else s"$reStr + $imStr"
  }
}

object Complex {
  implicit def toImaginary(x: Double) = new {
    def i = new Complex(0.0, x)
  }
  implicit def toImaginary(n: Int) = new {
    def i = new Complex(0.0, n.toDouble)
  }
  implicit def toComplex(x: Double) = new Complex(x, 0.0)

  val i = new Complex(0.0, 1.0)
  val one = new Complex(1.0, 0.0)
  val zero = new Complex(0.0, 0.0)

  trait ComplexIsNumeric extends Numeric[Complex] {
    def compare(x: Complex, y: Complex) = ???
    def fromInt(x: Int) = new Complex(x, 0.0)
    def plus(x: Complex, y: Complex) = x + y
    def minus(x: Complex, y: Complex) = x - y
    def times(x: Complex, y: Complex) = x * y
    def negate(x: Complex) = x * -1.0
    def toDouble(x: Complex) = ???
    def toFloat(x: Complex) = ???
    def toInt(x: Complex) = ???
    def toLong(x: Complex) = ???
  }

  implicit object ComplexIsNumeric extends ComplexIsNumeric
}

