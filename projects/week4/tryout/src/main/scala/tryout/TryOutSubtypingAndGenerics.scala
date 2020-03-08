package week4

import scala.annotation.tailrec

import Utils._

object TryOutSubtypingAndGenerics {
  def sum(x: Int, y: Int): Int = {
    x + y
  }

  class IntSet {}

  // Upper bound
  // S <: T means S is a subtype of T
  def assertAllPos[S <: IntSet](r: S): S = ???

  // Lower bound
  // S >: NonEmpty means S is a supertype of NonEmpty
  //
  // S could be one of: NonEmpty, IntSet (or AnyRef or Any)

  // Lower and Upper bound
  // [S >: NonEmpty <: IntSet]

  // covariance: subtyping relationship varies with the type parameter
  // List[NonEmpty] <: List[IntSet]
  //
  // Arrays: cannot alow covariance, as then can assign sub-type to array of super-type
  //
  // == in Java ==
  // Arrays *are* covariant (can have runtime exceptions!)
  //
  // NonEmpty[] a;
  // IntSet[] b = a; //!!! danger
  // b[0] = Empty // !!! bad assign - *runtime* exception - ArrayStackException
  //
  // == in Scala ==
  // Arrays are *not* covariant.
  //
  // class NonEmpty extends IntSet {}
  // val a: Array[NonEmpty] = Array(new NonEmpty())
  // val b: Array[IntSet] = a // type mismatch! - does not compile!

  // =================================================

  // Variance: generics and types
  //
  // in general:
  // - a type that accepts *mutations* of its elements (e.g. an array), should NOT be covariant.
  //
  // - but *immutable* types can be covariant - e.g. List

  /* Covariance and contravariance
   * types: C[T], A, B where A <: B
   *
   * 3 possible relationships between C[A] and C[B]:
   * - C[A] <: C[B] = C is covariant - 'specialising'
   * - C[A] >: C[B] = C is contravariant - 'generalizing'
   * - neither C[A] nor C[B] is a subtype of the other = nonvariant
   *
   * Scala allow you to declare the variance of a type:
   * class C[+A] = C is covariant
   * class C[-A] = C is contravariant
   * class C[A] = C is nonvariant (the default)
   *
   * Subtyping between function types:
   * if  A2 <: A1 and B1 <: B2, then:
   *   A1 => B1 <: A2 => B2
   *  (the 1st fun can be substituted for the 2nd fun) [Liskov substitution principle]
   * So in general:
   * - Functions are contravariant ('generalizing') in their argument types,
   * and covariant ('specialising') in their return types.
   * - Invariant types can appear in either parameter or return types.
   * - Covariant type parameters may appear in lower bounds of method type parameters.
   */
  // More precise Function1 definition:
  trait Function1[-T, +U] {
    def apply(x: T): U
    // def apply2(x: U): T - does not compile!
  }

  // Array:
  // Java has this - in Scala will not compile!
  // class Array[+T] {
  //   def update(x: T) = ??? // can have runtime exception, if x is not < T
  // }

  def go(): Unit = {
    Utils.banner("Week 4 - Sub-typing and generics")
  }
}
