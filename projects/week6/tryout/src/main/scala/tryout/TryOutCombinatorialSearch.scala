package tryout

import Utils._
import scala.collection.immutable.AbstractSeq

object TryOutCombinatorialSearch {

  /**
    * Find all pairs of +ve ints (i,j) 1 <= j < i < n, such that i + j is prime
    */
  def findPairsThatSumToPrime(n: Int) = {
    // IndexedSeq <= Seq <= Vector, Range
    val pairs: IndexedSeq[(Int, Int)] =
      (1 until n).map(i => (1 until i) map (j => (i, j))).flatten

    pairs.filter { case (x, y) => expensivePrime(x + y) }
  }

  /** Refactored via law:
    * (xs map f).flatten => xs flatMap f
    */
  def findPairsThatSumToPrime2(n: Int) = {
    // IndexedSeq <= Seq <= Vector, Range
    val pairs: IndexedSeq[(Int, Int)] =
      (1 until n).flatMap(i => (1 until i) map (j => (i, j)))

    pairs.filter { case (x, y) => expensivePrime(x + y) }
  }

  /**
    * Refactored to use for-expression (less abstract than filter,map)
    */
  def findPairsThatSumToPrime3(n: Int) = {
    // IndexedSeq <= Seq <= Vector, Range
    val pairs: IndexedSeq[(Int, Int)] =
      (1 until n).flatMap(i => (1 until i) map (j => (i, j)))

    // <- is a generator
    // if ... is a filter
    // yield returns an expression, whose value is returned by an iteration
    // For multiple lines, use {} instead of ()
    for (p <- pairs if (expensivePrime(p._1 + p._2))) yield p
  }

  /**
    * Refactored to use for-expression (less abstract than filter,map)
    */
  def findPairsThatSumToPrime4(n: Int) = {
    for {
      i <- 1 until n
      j <- 1 until i
      if (expensivePrime(i + j))
    } yield (i, j)
  }

  def expensivePrime(n: Int): Boolean = {
    require(n >= 1)

    // note: forall returns true for an empty collection!
    (2 until n).forall(n % _ != 0)
  }

  def tryFindPairsThatSumToPrime() = {
    println(s"findPairsThatSumToPrime(7) = ${findPairsThatSumToPrime(7)}")
    println(s"findPairsThatSumToPrime2(7) = ${findPairsThatSumToPrime2(7)}")
    println(s"findPairsThatSumToPrime3(7) = ${findPairsThatSumToPrime3(7)}")
    println(s"findPairsThatSumToPrime4(7) = ${findPairsThatSumToPrime4(7)}")
  }

  def scalarProductViaFor(xs: AbstractSeq[Int], ys: AbstractSeq[Int]): Int = {
    (for {
      (x, y) <- xs zip ys
    } yield (x * y)).sum
  }

  def tryScalarProduct() = {
    val v1 = 1 to 3
    val v2 = 4 to 6

    val expected = 1 * 4 + 2 * 5 + 3 * 6

    assert(expected == scalarProductViaFor(v1, v2))
  }

  def go(): Unit = {
    Utils.banner("week 6 - combinatorial search")

    tryFindPairsThatSumToPrime()
    tryScalarProduct()
  }
}
