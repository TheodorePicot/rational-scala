import TP4Ex2.ListSet

import scala.math

object TP4Ex1:

  /* Ce trait définit le type de données abstrait d'une queue (first-in-first-out) */
  trait Queue[A]:
    def enqueue(a: A): Queue[A]

    def dequeue(): Queue[A]

    def peek(): A

    def isEmpty(): Boolean

  /* Donnez une définition concrète à ce type de données. Quelle est la complexité des différentes opérations? */
  class ListQueue[A](l: List[A] = Nil) extends Queue[A]:

    override def enqueue(a: A): Queue[A] = new ListQueue[A](l :+ a)

    override def dequeue(): Queue[A] = new ListQueue[A](l.tail)

    override def peek(): A = l.head

    override def isEmpty(): Boolean = l.isEmpty

  /* Afin d'améliorer la complexité, on peut utiliser une implémentation basée sur deux listes:
   * - front contient les premiers éléments ajoutés, de telle manière que front.head est le plus ancien élément de la
   *   queue
   * - rear contient les derniers éléments ajoutés, de telle manière que rear.head est le plus récent élément ajouté
   *   (l'ordre des élément est donc l'inverse de celui de front)
   *
   * Implémentez les différentes opérations, de manière à maintenir ces invariants.
   * Note: on peut aussi ajouter l'invariant suivant (pas obligatoire, mais il permet de simplifier certains cas)
   * - front n'est vide que si toute la queue est vide (c'est-à-dire si rear l'est aussi)
   */
  class DoubleListQueue[A](front: List[A] = Nil, rear: List[A] = Nil) extends Queue[A]:


    /*override def enqueue(a: A): Queue[A] =
      front match
        case Nil => rear match
          case Nil => new DoubleListQueue[A](a::Nil, Nil)
          case _ => new DoubleListQueue[A](rear.reverse, Nil)
        case _ => new DoubleListQueue[A](front, a::rear)*/

    override def enqueue(a: A): Queue[A] =
      front match
        case Nil => new DoubleListQueue(a :: Nil, Nil)
        case _ => new DoubleListQueue(front, a :: rear)

    /*override def dequeue(): Queue[A] =
      front match
        case Nil => rear match
          case Nil => throw new Error("pas de rear")
          case _ => new DoubleListQueue[A](rear.reverse.tail, Nil)
        case _ => new DoubleListQueue[A](front.tail, rear)*/

    override def dequeue(): Queue[A] =
      front match
        case Nil => this
        case x :: Nil => new DoubleListQueue(rear.reverse, Nil)
        case _ :: xs => new DoubleListQueue(xs, rear)

    /*override def peek(): A =
      front match
        case Nil => rear match
          case Nil => throw new Error("pas de rear")
          case _ => rear.head
        case _ => front.head*/

    override def peek(): A = front.head

    override def isEmpty(): Boolean = front.isEmpty && rear.isEmpty

object TP4Ex2:

  /* Ce trait définit le type de données abstrait d'un ensemble. */
  trait Set[A]:
    def elem(a: A): Boolean

    def add(a: A): Set[A]

    def remove(a: A): Set[A]

  /* Donnez une définition concrète à ce type, basée sur une liste. */
  class ListSet[A](l: List[A] = Nil) extends Set[A]:

    override def elem(a: A): Boolean = l.contains(a)

    override def add(a: A): Set[A] = if elem(a) then this else new ListSet[A](a :: l)

    override def remove(a: A): Set[A] = l match
      case Nil => new ListSet[A](l)
      case x :: xs => new ListSet[A](l.filter(_ != a))



  /* Nous allons définir une implémentation plus efficace spécialement pour les types qui héritent du trait Ordered
   * (sur lesquels on peut utiliser les comparaisons <, <=, >= et >) basées sur un arbre binaire de recherche.
   * Pour cela on commence par définir un type de données algébrique pour les arbres binaires, et les opérations
   * associées.
   * Rappel: un arbre t est un arbre binaire de recherche si:
   * - t un arbre vide Leaf(), ou
   * - t est une Branch(x, l, r) qui satisfait les 3 conditions suivantes:
   *   (1) toutes les valeurs dans l'arbre l sont strictement inférieures à x
   *   (2) toutes les valeurs dans l'arbre r sont strictement supérieures à x
   *   (3) l et r sont des arbres binaires de recherche
   */
  enum BinaryTree[A <: Ordered[A]]:
    case Leaf()
    case Branch(x: A, left: BinaryTree[A], right: BinaryTree[A])

  def elemBST[A <: Ordered[A]](t: BinaryTree[A], a: A): Boolean =
    t match
      case BinaryTree.Leaf() => false
      case BinaryTree.Branch(x, left, right) =>
        if x == a
        then true
        else if a < x then elemBST(left, a)
        else elemBST(right, a)

  def addBST[A <: Ordered[A]](t: BinaryTree[A], a: A): BinaryTree[A] = ???

  def removeBST[A <: Ordered[A]](t: BinaryTree[A], a: A): BinaryTree[A] = ???

  /* Utilisez le type de données BinaryTree pour implémenter la classe suivante qui implémente Set. */
  class BstSet[A <: Ordered[A]](t: BinaryTree[A] = BinaryTree.Leaf[A]()) extends Set[A]:

    override def elem(a: A): Boolean = ???

    override def add(a: A): Set[A] = ???

    override def remove(a: A): Set[A] = ???


object TP4Ex3:

  /* Définissez la classe Fraction qui implémente Ordered. */
  class Fraction(val num: Int, val denum: Int) extends Ordered[Fraction]:

    def negate(): Fraction = Fraction(-num, denum)

    def invert(): Fraction = Fraction(denum, num)

    def add(that: Fraction): Fraction = Fraction(this.num * that.denum + that.num * this.denum, this.denum * that.denum)

    def sub(that: Fraction): Fraction = add(that.negate())

    def mult(that: Fraction): Fraction = Fraction(this.num * that.denum * that.num * this.denum, this.denum * that.denum)

    def div(that: Fraction): Fraction = mult(that.invert())

    override def compare(that: Fraction): Int = this.num * that.denum - that.num * this.denum

    /* compare doit être compatible avec l'égalité: this.compare(that) retourne 0 si et seulement si this == that */
    override def equals(obj: Any): Boolean = obj match
      case that: Fraction => this.num * that.denum == that.num * this.denum
      case _ => false

    /* deux objets égaux doivent avoir le même hashCode */
    override def hashCode(): Int =
      def gcd(x: Int, y: Int): Int = y match
        case 0 => x
        case _ => if y > y then gcd(x, y) else gcd(y, x % y)
      {
        val d = gcd(num, denum); ((num / d) * (denum / d)).hashCode()
      }

  class FractionIsFractional extends Fractional[Fraction]:

    override def div(x: Fraction, y: Fraction): Fraction = x.div(y)

    override def compare(x: Fraction, y: Fraction): Int = x.compare(y)

    override def fromInt(x: Int): Fraction = Fraction(x, 1)

    override def minus(x: Fraction, y: Fraction): Fraction = x.sub(y)

    override def plus(x: Fraction, y: Fraction): Fraction = x.add(y)

    override def times(x: Fraction, y: Fraction): Fraction = x.mult(y)

    override def parseString(str: String): Option[Fraction] = ???

    override def negate(x: Fraction): Fraction = x.negate()

    override def toInt(x: Fraction): Int = x.num / x.denum

    override def toDouble(x: Fraction): Double = x.num.toDouble / x.denum.toDouble

    override def toFloat(x: Fraction): Float = x.num.toFloat / x.denum.toFloat

    override def toLong(x: Fraction): Long = x.num.toLong / x.denum.toLong

  enum FractionalFunction[T]:
    case Const(v: T)
    case Plus(f1: FractionalFunction[T], f2: FractionalFunction[T])

  def eval[T](f: FractionalFunction[T])(implicit fr: Fractional[T]): T = f match
    case FractionalFunction.Const(v) => v
    case FractionalFunction.Plus(f1, f2) => fr.plus(eval(f1), eval(f2))

  enum ArithExpr:
    case Constant(v: Double)
    case Neg(e: ArithExpr)
    case Add(e1: ArithExpr, e2: ArithExpr)
    case Sub(e1: ArithExpr, e2: ArithExpr)
    case Mult(e1: ArithExpr, e2: ArithExpr)
    case Div(e1: ArithExpr, e2: ArithExpr)
    case Var()

  def eval(e: ArithExpr, x: Double): Option[Double] = e match
    case ArithExpr.Constant(v) => Some(v)
    case ArithExpr.Neg(v) => eval(v, x).map(i => -i)
    case ArithExpr.Add(v, u) => eval(v, x).flatMap(i => eval(u, x).map(j => i + j))
    case ArithExpr.Sub(v, u) => eval(v, x).flatMap(i => eval(u, x).map(j => i - j))
    case ArithExpr.Mult(v, u) => eval(v, x).flatMap(i => eval(u, x).map(j => i * j))
    case ArithExpr.Div(v, u) => eval(v, x).flatMap(i => eval(u, x).withFilter(j => j != 0).map(j => i / j))
    case ArithExpr.Var() => Some(x)

  def evalFor(e: ArithExpr, x: Double): Option[Double] =
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
                                      if j != 0
      yield (i / j)

