import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import DataStructures.*

import scala.math.BigInt
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._

class TestsDataStructures extends AnyFlatSpec {

  "Rational" should "be defined" in {
    val r1 = Rational(2)
    r1.negate() shouldBe Rational(-2)
    val r2 = Rational(2)
    r2.invert() shouldBe Rational(1, 2)
    val r3 = Rational(1, 3)
    val r4 = Rational(1, 3)
    r3.add(r4) shouldBe Rational(2, 3)
    val r5 = Rational(2, 3)
    val r6 = Rational(1, 3)
    r5.sub(r6) shouldBe Rational(1, 3)
    val r7 = Rational(1, 3)
    val r8 = Rational(1, 3)
    r7.mult(r8) shouldBe Rational(1, 9)
    val r9 = Rational(2)
    val r10 = Rational(3)
    r9.div(r10) shouldBe Rational(2, 3)
    val r11 = Rational(1, 3)
    val r12 = Rational(3)
    r11.mult(r12) shouldBe Rational(1)
  }

  "Monomial" should "be defined" in {
    val m1 = Monomial(Rational(2), 2)
    m1.eval(Rational(2)) shouldBe Rational(8)
  }

  "Polynomial" should "be defined" in {
    val m1 = Monomial(Rational(2), 2)
    val m2 = Monomial(Rational(2), 1)
    val p1 = Polynomial(List(m1, m2))
    p1.eval(Rational(2)) shouldBe Rational(12)
  }

  "Arbitray Function" should "be defined" in {

    val f_add = FunctionSymbolic.Add(
      FunctionSymbolic.Var(),
      FunctionSymbolic.Constant(Rational(2, 4))
    ) // f = x + 2/4
    evalFor(f_add, Rational(1)) shouldEqual Some(Rational(6, 4)) // f(1) = 1 + 2/4 = 6/4

    val f_sub_simple = FunctionSymbolic.Sub(
      FunctionSymbolic.Constant(Rational(1)),
      FunctionSymbolic.Var()
    )
    evalFor(f_sub_simple, Rational(3)) shouldEqual Some(Rational(-2))

    val f_sub = FunctionSymbolic.Sub(
      FunctionSymbolic.Mult(
        FunctionSymbolic.Constant(Rational(3, 5)),
        FunctionSymbolic.Var()),
      FunctionSymbolic.Constant(Rational(4, 5))
    ) //f = 3/5x - 4/5
    println(evalFor(f_sub, Rational(2)).toString)
    evalFor(f_sub, Rational(2)) shouldEqual Some(Rational(2, 5)) // f(2) = 6/5 - 4/5 = 2/5

    val f_mult = FunctionSymbolic.Mult(FunctionSymbolic.Constant(Rational(1, 2)), FunctionSymbolic.Var()) // f = 1/2x
    evalFor(f_mult, Rational(2)) shouldEqual Some(Rational(1)) // f(2) = 1
  }

  "Convert" should "be defined" in {
    val f_add = FunctionSymbolic.Add(
      FunctionSymbolic.Var(),
      FunctionSymbolic.Constant(Rational(2, 4))
    )// f = x + 2/4
    convert(f_add) shouldEqual Polynomial(List(
      Monomial(Rational(1, 1), 1),
      Monomial(Rational(2, 4), 0)
      )
    )
  }
}