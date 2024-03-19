import scala.annotation.tailrec
import scala.math
import scala.math.Numeric.Implicits.*
import scala.math.pow
implicit class PowerInt(i: Int) {
  def ** (b: Int): Int = pow(i, b).intValue
}
object DataStructures:

  /* Définissez la classe Rational qui implémente Ordered. */
  class Rational(val numerator: Int, val denominator: Int) extends Ordered[Rational]:

    def this(x: Int) = this(x, 1)

    require(denominator != 0, "Cannot create a Rational with denominator 0")

    def negate(): Rational = Rational(-numerator, denominator)

    def invert(): Rational = Rational(denominator, numerator)

    def add(that: Rational): Rational = Rational(numerator * that.denominator + denominator * that.numerator, denominator * that.denominator)

    def sub(that: Rational): Rational = Rational(numerator * that.denominator - denominator * that.numerator, denominator * that.denominator)

    def mult(that: Rational): Rational = Rational(numerator * that.numerator, denominator * that.denominator)

    def div(that: Rational): Rational = Rational(numerator * that.denominator, denominator * that.numerator)

    def power(degree: Int): Rational =
      val newDenominator = denominator ** degree
      if newDenominator == 1 then Rational(numerator ** degree) else Rational(numerator ** degree, newDenominator)

    /* compare doit être compatible avec l'égalité: this.compare(that) retourne 0 si et seulement si this == that */
    override def equals(obj: Any): Boolean = obj match
      case that: Rational => this.numerator * that.denominator == this.denominator * that.numerator
      case _ => false

    override def compare(that: Rational): Int = this.numerator * that.denominator - that.numerator * this.denominator

    /* deux objets égaux doivent avoir le même hashCode */
    override def hashCode(): Int =
      @tailrec
      def greatestCommonDivisor(x: Int, y: Int): Int = y match
        case 0 => x
        case _ => if y > y then greatestCommonDivisor(x, y) else greatestCommonDivisor(y, x % y)
      {
        val divisor = greatestCommonDivisor(numerator, denominator);
        ((numerator / divisor) * (denominator / divisor)).hashCode()
      }

    override def toString: String = s"$numerator / $denominator"

  class RationalIsFractional extends Fractional[Rational]:

    override def div(x: Rational, y: Rational): Rational = x.div(y)

    override def compare(x: Rational, y: Rational): Int = x.compare(y)

    override def fromInt(x: Int): Rational = Rational(x, 1)

    override def minus(x: Rational, y: Rational): Rational = x.sub(y)

    override def plus(x: Rational, y: Rational): Rational = x.add(y)

    override def times(x: Rational, y: Rational): Rational = x.mult(y)

    override def parseString(str: String): Option[Rational] = ???

    override def negate(rationalNumber: Rational): Rational = rationalNumber.negate()

    override def toInt(rationalNumber: Rational): Int = rationalNumber.numerator / rationalNumber.denominator

    override def toDouble(rationalNumber: Rational): Double = rationalNumber.numerator.toDouble / rationalNumber.denominator.toDouble

    override def toFloat(rationalNumber: Rational): Float = rationalNumber.numerator.toFloat / rationalNumber.denominator.toFloat

    override def toLong(rationalNumber: Rational): Long = rationalNumber.numerator.toLong / rationalNumber.denominator.toLong

//    override def power(x: Rational, degree : Int): Rational = x.power(degree)

  implicit object RationalIsFractional extends RationalIsFractional

  class Monomial(coefficient: Rational, degree: Int):
    def eval(x: Rational): Rational =
      x * coefficient.power(degree)


  class Polynomial(list: List[Monomial]):
    def eval(x: Rational): Rational =
      val valeur = for mono <- list
        yield mono.eval(x)
      valeur.sum

    def convert(functionSymbolic: FunctionSymbolic) : Polynomial = ???
//      functionSymbolic match
//        case FunctionSymbolic.Var() => Polynomial(Monomial(Rational(1), 1))
//        case FunctionSymbolic.Constant(v: Rational) => Monomial(v, 1)
//        case FunctionSymbolic.Neg(e: FunctionSymbolic) => Polynomial(convert()
//        case FunctionSymbolic.Add(e1: FunctionSymbolic, e2: FunctionSymbolic) =>
//        case FunctionSymbolic.Sub(e1: FunctionSymbolic, e2: FunctionSymbolic) =>
//        case FunctionSymbolic.Mult(e1: FunctionSymbolic, e2: FunctionSymbolic) =>
//        case FunctionSymbolic.Div(e1: FunctionSymbolic, e2: FunctionSymbolic) =>



  enum RationalalFunction[T]:
    case Const(v: T)
    case Plus(f1: RationalalFunction[T], f2: RationalalFunction[T])

//  def eval[T](f: RationalalFunction[T])(implicit fr: Rational[T]): T = ???


  enum FunctionSymbolic:
    case Constant(v: Rational)
    case Neg(e: FunctionSymbolic)
    case Add(e1: FunctionSymbolic, e2: FunctionSymbolic)
    case Sub(e1: FunctionSymbolic, e2: FunctionSymbolic)
    case Mult(e1: FunctionSymbolic, e2: FunctionSymbolic)
    case Div(e1: FunctionSymbolic, e2: FunctionSymbolic)
    case Var()

  def eval(e: FunctionSymbolic, x: Rational): Option[Rational] = e match
    case FunctionSymbolic.Constant(v) => Some(v)
    case FunctionSymbolic.Neg(v) => eval(v, x).map(i => -i)
    case FunctionSymbolic.Add(v, u) => eval(v, x).flatMap(i => eval(u, x).map(j => i + j))
    case FunctionSymbolic.Sub(v, u) => eval(v, x).flatMap(i => eval(u, x).map(j => i - j))
    case FunctionSymbolic.Mult(v, u) => eval(v, x).flatMap(i => eval(u, x).map(j => i * j))
    case FunctionSymbolic.Div(v, u) => eval(v, x).flatMap(i => eval(u, x).withFilter(j => !j.equals(0)).map(j => i.div(j)))
    case FunctionSymbolic.Var() => Some(x)

  def evalFor(e: FunctionSymbolic, x: Rational): Option[Rational] =
    e match
      case FunctionSymbolic.Constant(v) => Some(v)
      case FunctionSymbolic.Neg(v) => for i <- eval(v, x)
        yield (-i)
      case FunctionSymbolic.Add(v, u) => for i <- eval(v, x)
                                      j <- eval(u, x)
      yield (i + j)
      case FunctionSymbolic.Sub(v, u) => for i <- eval(v, x)
                                      j <- eval(u, x)
      yield (i - j)
      case FunctionSymbolic.Mult(v, u) => for i <- eval(v, x)
                                       j <- eval(u, x)
      yield (i * j)
      case FunctionSymbolic.Div(v, u) => for i <- eval(v, x)
                                      j <- eval(u, x)
                                      if !j.equals(0)
      yield i.div(j)
