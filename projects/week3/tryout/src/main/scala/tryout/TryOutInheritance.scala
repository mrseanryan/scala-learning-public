package week3

import scala.annotation.tailrec

import Utils._

object TryOutInheritance {
  def sum(x: Int, y: Int): Int = {
    x + y
  }

  /*
  Traits are like 'interfaces', but:
  - with implementation
  - no members (no value parameters)

  (is redundant here, but adding as a working example)
   */
  trait Describable {
    def getDescription(): String
  }

  abstract class IntSet extends Describable {
    def contains(x: Int): Boolean
    def incl(x: Int): IntSet
    override def toString(): String = getDescription()
    def union(other: IntSet): IntSet
  }

  // Binary tree implementation
  //
  // Uses Empty node to avoid nulls (Null Object pattern)
  //
  // A persistent data structure (old structure (old root) is maintained, on incl())

  // *object* Empty = a singleton (saves memory)
  object Empty extends IntSet with Describable {
    def contains(x: Int): Boolean = false
    override def getDescription(): String = "."
    def incl(x: Int): IntSet = new NonEmpty(x)
    def union(other: IntSet): IntSet = other
  }

  class NonEmpty(elem: Int, left: IntSet = Empty, right: IntSet = Empty)
      extends IntSet {
    def contains(x: Int): Boolean = {
      if (x < elem) left.contains(x)
      else if (x > elem) right.contains(x)
      else true
    }

    override def getDescription(): String = {
      s"{${left}-${elem}-${right}}"
    }

    // Root is same
    def incl(x: Int): IntSet = {
      if (x < elem) new NonEmpty(elem, left.incl(x), right)
      else if (x > elem) new NonEmpty(elem, left, right.incl(x))
      else this
    }

    // Root can change
    def union(other: IntSet): IntSet = {
      ((left union right) union other) incl elem
    }
  }

  def go(): Unit = {
    Utils.banner("Inheritance")

    val t1 = new NonEmpty(3);
    println("t1: " + t1.toString())
    println()

    val t1_incl_4 = t1 incl 4
    println("t1_incl_4: " + t1_incl_4.toString())
    println()

    val t3 = new NonEmpty(3, new NonEmpty(1), new NonEmpty(4))
    println("t3: " + t3.toString())
    println()

    val t4 = new NonEmpty(4, new NonEmpty(3), new NonEmpty(5))
    println("t4: " + t4.toString())
    println()

    val t3_union_t4 = t3 union t4
    println("t3_union_t4: " + t3_union_t4.toString())
    println()
  }
}
