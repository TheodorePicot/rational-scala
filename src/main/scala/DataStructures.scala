import scala.math
object DataStructures:

  /* Définissez la classe Rational qui implémente Ordered. */
  class Rational(val num: Int, val denum: Int):

    def negate(): Rational = ???

    def invert(): Rational = ???

    def add(that: Rational): Rational = ???

    def sub(that: Rational): Rational = ???

    def mult(that: Rational): Rational = ???

    def div(that: Rational): Rational = ???

    /* compare doit être compatible avec l'égalité: this.compare(that) retourne 0 si et seulement si this == that */
    override def equals(obj: Any): Boolean = obj match
      case that: Rational => ???
      case _ => false

    /* deux objets égaux doivent avoir le même hashCode */
    override def hashCode(): Int = ???

  class RationalIsFractional extends Fractional[Rational]:

    override def div(x: Rational, y: Rational): Rational = ???

    override def compare(x: Rational, y: Rational): Int = ???

    override def fromInt(x: Int): Rational = ???

    override def minus(x: Rational, y: Rational): Rational = ???

    override def plus(x: Rational, y: Rational): Rational = ???

    override def times(x: Rational, y: Rational): Rational = ???

    override def parseString(str: String): Option[Rational] = ???

    override def negate(x: Rational): Rational = ???

    override def toInt(x: Rational): Int = ???

    override def toDouble(x: Rational): Double = ???

    override def toFloat(x: Rational): Float = ???

    override def toLong(x: Rational): Long = ???

  enum RationalalFunction[T]:
    case Const(v: T)
    case Plus(f1: RationalalFunction[T], f2: RationalalFunction[T])

//  def eval[T](f: RationalalFunction[T])(implicit fr: Rational[T]): T = ???


