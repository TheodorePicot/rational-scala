import scala.math
import scala.math.Numeric.Implicits._
import scala.math.pow
implicit class PowerInt(i: Int) {
  def ** (b: Int): Int = pow(i, b).intValue
}
object DataStructures:

  /* Définissez la classe Rational qui implémente Ordered. */
  class Rational(val x: Int, val y: Int) extends Ordered[Rational]:

    def this(x: Int) = this(x, 1)

    require(y != 0, "Cannot create a Rational with denominator 0")

    def negate(): Rational = Rational(-x, y)

    def invert(): Rational = Rational(y, x)

    def add(that: Rational): Rational = Rational(x * that.y + y * that.x, y * that.y)

    def sub(that: Rational): Rational = Rational(x * that.y - y * that.x, y * that.y)

    def mult(that: Rational): Rational = Rational(x * that.x, y * that.y)

    def div(that: Rational): Rational = Rational(x * that.y, y * that.x)

    def power(degree: Int): Rational =
      val denum = y ** degree
      if denum == 1 then Rational(x ** degree) else Rational(x ** degree, denum)

    /* compare doit être compatible avec l'égalité: this.compare(that) retourne 0 si et seulement si this == that */
    override def equals(obj: Any): Boolean = obj match
      case that: Rational => this.x * that.y == this.y * that.x
      case _ => false

    override def compare(that: Rational): Int = this.x * that.y - that.x * this.y

    /* deux objets égaux doivent avoir le même hashCode */
    override def hashCode(): Int =
      def gcd(x: Int, y: Int): Int = y match
        case 0 => x
        case _ => if y > y then gcd(x, y) else gcd(y, x % y)
      {
        val d = gcd(x, y);
        ((x / d) * (y / d)).hashCode()
      }

  class RationalIsFractional extends Fractional[Rational]:

    override def div(x: Rational, y: Rational): Rational = x.div(y)

    override def compare(x: Rational, y: Rational): Int = x.compare(y)

    override def fromInt(x: Int): Rational = Rational(x, 1)

    override def minus(x: Rational, y: Rational): Rational = x.min(y)

    override def plus(x: Rational, y: Rational): Rational = x.add(y)

    override def times(x: Rational, y: Rational): Rational = x.mult(y)

    override def parseString(str: String): Option[Rational] = ???

    override def negate(x: Rational): Rational = x.negate()

    override def toInt(x: Rational): Int = x.x / x.y

    override def toDouble(x: Rational): Double = x.x.toDouble / x.y.toDouble

    override def toFloat(x: Rational): Float = x.x.toFloat / x.y.toFloat

    override def toLong(x: Rational): Long = x.x.toLong / x.y.toLong

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



  enum RationalalFunction[T]:
    case Const(v: T)
    case Plus(f1: RationalalFunction[T], f2: RationalalFunction[T])

//  def eval[T](f: RationalalFunction[T])(implicit fr: Rational[T]): T = ???


  enum ArithExpr:
    case Constant(v: Rational)
    case Neg(e: ArithExpr)
    case Add(e1: ArithExpr, e2: ArithExpr)
    case Sub(e1: ArithExpr, e2: ArithExpr)
    case Mult(e1: ArithExpr, e2: ArithExpr)
    case Div(e1: ArithExpr, e2: ArithExpr)
    case Var()

  def eval(e: ArithExpr, x: Rational): Option[Rational] = e match
    case ArithExpr.Constant(v) => Some(v)
    case ArithExpr.Neg(v) => eval(v, x).map(i => -i)
    case ArithExpr.Add(v, u) => eval(v, x).flatMap(i => eval(u, x).map(j => i + j))
    case ArithExpr.Sub(v, u) => eval(v, x).flatMap(i => eval(u, x).map(j => i - j))
    case ArithExpr.Mult(v, u) => eval(v, x).flatMap(i => eval(u, x).map(j => i * j))
    case ArithExpr.Div(v, u) => eval(v, x).flatMap(i => eval(u, x).withFilter(j => !j.equals(0)).map(j => i.div(j)))
    case ArithExpr.Var() => Some(x)

  def evalFor(e: ArithExpr, x: Rational): Option[Rational] =
    e match
      case ArithExpr.Constant(v) => Some(v)
      case ArithExpr.Neg(v) => for i <- eval(v, x)
        yield (-i)
      case ArithExpr.Add(v, u) => for i <- eval(v, x)
                                      j <- eval(u, x)
      yield (i + j)
      case ArithExpr.Sub(v, u) => for i <- eval(v, x)
                                      j <- eval(u, x)
      yield (i - j)
      case ArithExpr.Mult(v, u) => for i <- eval(v, x)
                                       j <- eval(u, x)
      yield (i * j)
      case ArithExpr.Div(v, u) => for i <- eval(v, x)
                                      j <- eval(u, x)
                                      if !j.equals(0)
      yield i.div(j)
