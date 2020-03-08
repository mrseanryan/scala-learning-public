package tryout

import Utils._
import scala.collection.immutable.AbstractSeq

object TryOutOtherCollections {

  /** Collections
    *
    * List
    * - accessing head is much faster than middle or end
    * - constant time for head
    *
    * Vector
    * - more evenly balanced access performance than List
    * - a tree, where each node has up to 32 entries
    * - grows in depth, very slowly: depth (accesses) = log(base 32) N
    * - good for bulk operations (in chunks of 32, which aligns with CPU caches)
    * -- map, fold, filter
    *
    * Operations:
    * - all operations as list, except :: (cons)
    * x +: xs (create a new vector of element x, sequence xs)
    * xs :+ x (create a new vector of sequence xs, element x)
    *
    * Adding an element to a vector, creates a new vector (immutable!)
    * - adds new parents up to new root, for each level
    * - complexity: log(base 32) N
    *
    * Collection hierarchy:
    * Iterable <= Seq <= List, Vector, Range, (Array*, String*)
    *          <= Set
    *          <= Map
    *
    * * [from Java] Array, String can be *implicitly* converted to Seq
    *
    */
  def tryVectors() = {
    val v1 = Vector(1, 2, 3, 4).map(x => x * x)
    println(s"${v1} = ${sequenceAsString(v1)}")
  }

  /**
    * Range
    */
  def tryRanges(): Unit = {
    val r1: Range = 1 until 5
    println(s"${r1} = ${sequenceAsString(r1)}")

    val r2: Range = 1 to 5
    println(s"${r2} = ${sequenceAsString(r2)}")

    var r3 = 1 to 10 by 3
    println(s"${r3} = ${sequenceAsString(r3)}")

    var r4 = 6 to 1 by -1
    println(s"${r4} = ${sequenceAsString(r4)}")
  }

  def tryOps() = {
    val odds = 1 to 10 by 2
    val evens = 2 to 10 by 2

    assert(odds.exists(x => x == 1))
    assert(!odds.exists(x => x == 2))
    assert(evens.exists(x => x == 2))

    assert(odds.forall(x => x % 2 == 1))
    assert(evens.forall(x => x % 2 == 0))

    val alls = odds.zip(evens)
    val (odds2, evens2) = alls.unzip

    println(s"odds2.flatMap(x => Vector(-1, x, -3)) = ${odds2
      .flatMap(x => Vector(-1, x, -3))}")

    assert(odds2.sum == odds.sum)
    assert(evens2.sum == evens.sum)

    assert(alls.flatMap(x => Vector(x._1, x._2)).sum == odds.sum + evens.sum)
  }

  def sequenceAsString[T](seq: AbstractSeq[T]): String = {
    seq.map(x => s"${x},").reduce((a, b) => a + b)
  }

  def scalarProduct(xs: AbstractSeq[Int], ys: AbstractSeq[Int]): Int =
    (xs zip ys).map(xy => xy._1 * xy._2).sum

  // Pattern matching function value = { case ... }
  def scalarProductViaPatternMatching(
      xs: AbstractSeq[Int],
      ys: AbstractSeq[Int]
  ): Int =
    (xs zip ys).map { case (x, y) => x * y }.sum

  def tryScalarProduct() = {
    val v1 = 1 to 3
    val v2 = 4 to 6

    val expected = 1 * 4 + 2 * 5 + 3 * 6

    assert(expected == scalarProduct(v1, v2))
    assert(expected == scalarProductViaPatternMatching(v1, v2))
  }

  def expensivePrime(n: Int): Boolean = {
    require(n >= 1)

    // note: forall returns true for an empty collection!
    (2 until n).forall(x => n % x != 0)
  }

  def tryPrime() = {
    assert(expensivePrime(1))
    assert(expensivePrime(2))
    assert(expensivePrime(3))
    assert(!expensivePrime(4))
    assert(expensivePrime(5))
    assert(!expensivePrime(6))
    assert(expensivePrime(7))
  }

  def go(): Unit = {
    Utils.banner("week 6 - Collections")

    tryVectors()
    tryRanges()
    tryOps()
    tryScalarProduct()
    tryPrime()
  }

  def sum(x: Int, y: Int): Int = {
    x + y
  }
}
