package week4

import scala.annotation.tailrec

import Utils._

object TryOutList {

  /* Lists vs Arrays:
   * - Lists are immutable
   * - Lists are recursive
   *
   * - both are homogenous (just 1 type, with inheritance)
   * */

  def go(): Unit = {
    Utils.banner("Week 4 - Lists")

    // All are equivalent:
    val fruit = List("apples", "oranges", "bananas")
    val fruit2 = "apples" :: "oranges" :: "bananas" :: Nil
    val fruit3 = ("apples" :: ("oranges" :: ("bananas" :: Nil)))
    val fruit4 = Nil.::("bananas").::("oranges").::("apples") // what compiler outputs
    // :: is like 'prepend'

    val nums = List(5, 3, 4, 2, 1)

    val diag3d = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
    val empty = List

    /** Can use Lists with pattern matching:
      * - head_value :: tail_value
      * - List(p1, ..., pn)
      * - p1 :: ... :: pn :: Nil
      *
      * - 1 :: 2 :: xs - a list that starts with 1, 2
      * - x :: Nil - a list of length 1
      * - List(x) - (same) a list of length 1
      * - List() - empty
      * - Nil - (same) empty
      * - List(2 :: xs) - a list containing only a list that starts with 2
      *
      * x :: y :: List(xs, ys) :: zs - zs could be empty, so this can match a list of length >= 3 (!)
      */
    // Sorting a list via insertion sort
    // worst case = n^2 operations!
    def insertionSort(xs: List[Int]): List[Int] = xs match {
      case List()  => List()
      case y :: ys => insert(y, insertionSort(ys))
    }

    def insert(x: Int, xs: List[Int]): List[Int] = xs match {
      case List() => List(x)
      case y :: ys => {
        if (x <= y)
          x :: xs
        else
          y :: (insert(x, ys))
      }
    }

    println(s"sort nums = ${insertionSort(nums)}")
  }
}
