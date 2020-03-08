package week5

import scala.annotation.tailrec

import Utils._

object TryOutSorting {
  def sum(a: Int, b: Int): Int = a + b

  /* Merge Sort

  - if list 0 or 1 elem -> return the list

  - split list in 2
  - sort the 2
  - merge the 2 sorted sub-lists, into a single sorted list
   */
  def mergeSort(list: List[Int]): List[Int] = {
    list match {
      case List()  => List()
      case List(x) => List(x)
      case x :: xs => {
        val mid = xs.length / 2

        val left = mergeSort(x :: xs.take(mid))
        val right = mergeSort(xs.drop(mid))
        merge2(left, right)
      }
    }
  }

  def mergeSort2[T](list: List[T], mergeFun: ((List[T], List[T]) => List[T]))(
      lt: (T, T) => Boolean
  ): List[T] = {
    val mid = list.length / 2
    if (mid == 0) list
    else {
      val (left, right) = list splitAt (mid)
      mergeFun(
        mergeSort2[T](left, mergeFun)(lt),
        mergeSort2[T](right, mergeFun)(lt)
      )
    }
  }

  def mergeSortSimpler[T](list: List[T])(
      lt: (T, T) => Boolean // Having the fun param last, increases chance that compiler can correctly infer the types for the caller.
  ): List[T] = {
    def mergeFun(left: List[T], right: List[T]): List[T] = {
      (left, right) match {
        case (Nil, right) => right
        case (left, Nil)  => left
        case (l :: ls, r :: rs) => {
          if (lt(l, r)) {
            l :: mergeFun(ls, right)
          } else {
            r :: mergeFun(left, rs)
          }
        }
      }
    }

    val mid = list.length / 2
    if (mid == 0) list
    else {
      val (left, right) = list splitAt (mid)
      mergeFun(
        mergeSortSimpler[T](left)(lt),
        mergeSortSimpler[T](right)(lt)
      )
    }
  }

  def mergeSortSimpler2[T](list: List[T])(
      ord: Ordering[T] // Having the fun param last, increases chance that compiler can correctly infer the types for the caller.
  ): List[T] = {
    def mergeFun(left: List[T], right: List[T]): List[T] = {
      (left, right) match {
        case (Nil, right) => right
        case (left, Nil)  => left
        case (l :: ls, r :: rs) => {
          if (ord.lt(l, r)) {
            l :: mergeFun(ls, right)
          } else {
            r :: mergeFun(left, rs)
          }
        }
      }
    }

    val mid = list.length / 2
    if (mid == 0) list
    else {
      val (left, right) = list splitAt (mid)
      mergeFun(
        mergeSortSimpler2[T](left)(ord),
        mergeSortSimpler2[T](right)(ord)
      )
    }
  }

  def mergeSortSimpler3[T](list: List[T])(
      // Having the fun param last, increases chance that compiler can correctly infer the types for the caller.
      // implicit: compiler figures out which Ordering.X to pass !
      implicit ord: Ordering[T]
  ): List[T] = {
    def mergeFun(left: List[T], right: List[T]): List[T] = {
      (left, right) match {
        case (Nil, right) => right
        case (left, Nil)  => left
        case (l :: ls, r :: rs) => {
          if (ord.lt(l, r)) {
            l :: mergeFun(ls, right)
          } else {
            r :: mergeFun(left, rs)
          }
        }
      }
    }

    val mid = list.length / 2
    if (mid == 0) list
    else {
      val (left, right) = list splitAt (mid)
      mergeFun(mergeSortSimpler3[T](left), mergeSortSimpler3[T](right))
    }
  }

  def mergeLt(left: List[Int], right: List[Int])(
      lt: (Int, Int) => Boolean
  ): List[Int] = {
    merge(left, right)
  }

  // assume: left, right already sorted
  def merge(left: List[Int], right: List[Int]): List[Int] = {
    left match {
      case List() => right
      case x :: xs => {
        right match {
          case List() => left
          case y :: ys => {
            if (x > y) {
              y :: merge(left, ys)
            } else {
              x :: merge(xs, right)
            }
          }
        }
      }
    }
  }

  def merge2(left: List[Int], right: List[Int]): List[Int] = {
    merge2[Int](left, right)((l: Int, r: Int) => l < r)
  }

  // assume: left, right already sorted
  def merge2[T](
      left: List[T],
      right: List[T]
  )(lt: (T, T) => Boolean): List[T] = {
    left match {
      case Nil => right // Nil = List()
      case x :: xs1 => {
        right match {
          case Nil => left // Nil = List()
          case y :: ys1 => {
            if (lt(x, y)) x :: merge2(xs1, right)(lt)
            else
              y :: merge2(left, ys1)(lt)
          }
        }
      }
    }
  }

  def mergeViaPairs(left: List[Int], right: List[Int]): List[Int] = {
    mergeViaPairs[Int](left, right)((l: Int, r: Int) => l < r)
  }

  // assume: left, right already sorted
  def mergeViaPairs[T](
      left: List[T],
      right: List[T]
  )(lt: (T, T) => Boolean): List[T] = {
    (left, right) match {
      case (Nil, right) => right
      case (left, Nil)  => left
      case (l :: ls, r :: rs) => {
        if (lt(l, r)) {
          l :: merge2(ls, right)(lt)
        } else {
          r :: merge2(left, rs)(lt)
        }
      }
    }
  }

  def go(): Unit = {
    Utils.banner("Week 5 - sorting (merge sort)")
  }
}
