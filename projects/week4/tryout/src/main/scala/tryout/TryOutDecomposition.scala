package week4

import scala.annotation.tailrec

import Utils._

object TryOutDecomposition {

  trait Expr {
    def numValue: Int
  }

  class Number(n: Int) extends Expr {
    def numValue: Int = n
  }

  class Sum(val leftOp: Expr, val rightOp: Expr) extends Expr {
    def numValue: Int = leftOp.numValue + rightOp.numValue
  }

  /*
   * Problem: find a general way to access objects in an *extensible* class hierarchy.
   *
   * Approaches:
   * - Classification 'isSum' and access methods => quadratic explosion of methods, as classes are added
   * - Type tests and cases => unsafe, low-level
   * - OO decomposition: expensive to add new methods/operations (use Visitor pattern?).
   *     Difficult if need non-local knowledge of the inheritance tree
   *     (e.g. to simplify an algebraic expression, by adding braces.)
   *
   * Type Casts are Discouraged: (unsafe - provided for interop with Java)
   * x.isInstanceOf[T]
   * x.asInstanceOf[T]
   */

  /*
   * Solution: Functional Decomposition with Pattern Matching
   * Answers the questions that the accessor functions would answer:
   * - which subclass was used?
   * - what arguments were passed to constructor?
   */

  // Defines a trait, and 2 concrete subclasses of Expr2
  trait ExprWithPatternMatching {
    case class Number(n: Int) extends ExprWithPatternMatching
    /* implies:
    object Number(n: Int) = new Number(b)

    so can write: Number(3)  (no 'new')
     */
    case class Sum(e1: ExprWithPatternMatching, e2: ExprWithPatternMatching)
        extends ExprWithPatternMatching

    case class Prod(e1: ExprWithPatternMatching, e2: ExprWithPatternMatching)
        extends ExprWithPatternMatching
  }

  object ExprTest extends ExprWithPatternMatching {
    // Pattern matching:
    //
    // The 'expression problem':
    // Eval can be all in trait (via pattern matching) - helps if adding lot of new operations
    // OR can be in each sub-type - helps if adding lot of new sub-types
    def eval(e: ExprWithPatternMatching): Int = e match {
      case Number(n)    => n
      case Sum(e1, e2)  => eval(e1) + eval(e2)
      case Prod(e1, e2) => eval(e1) * eval(e2)
      // If no match => MatchError exception
    }

    def show(e: ExprWithPatternMatching): String = e match {
      case Number(n)    => n.toString()
      case Sum(e1, e2)  => s"(${show(e1)} + ${show(e2)})"
      case Prod(e1, e2) => s"(${show(e1)} * ${show(e2)})"
    }
  }

  def go(): Unit = {
    Utils.banner("Week 4 - Decomposition")

    var onePlus2 = (new Sum(new Number(1), new Number(2)))
    println(s"1 + 2 = ${onePlus2.numValue}")

    var onePlus2_patterns =
      (ExprTest.Sum(ExprTest.Number(1), ExprTest.Number(2)))
    println(
      s"1 + 2 = ${ExprTest.show(onePlus2_patterns)} = ${ExprTest.eval(onePlus2_patterns)}"
    )

    var sumOfProd = ExprTest.Sum(
      ExprTest.Prod(ExprTest.Number(2), ExprTest.Number(3)),
      ExprTest.Number(4)
    )
    println(
      s"(2 * 3) + 4 = ${ExprTest.show(sumOfProd)} = ${ExprTest.eval(sumOfProd)}"
    )

    var prodOfSum =
      ExprTest.Prod(
        ExprTest.Sum(ExprTest.Number(3), ExprTest.Number(4)),
        ExprTest.Number(2)
      )
    println(
      s"(3 + 4) * 2 = ${ExprTest.show(prodOfSum)} = ${ExprTest.eval(prodOfSum)}"
    )
  }
}
