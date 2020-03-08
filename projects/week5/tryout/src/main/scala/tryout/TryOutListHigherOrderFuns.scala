package week5

import Utils._
import scala.annotation.tailrec

object TryOutListHigherOrderFuns {
  def sum(a: Int, b: Int): Int = a + b

  def map[T, U](list: List[T], f: T => U): List[U] = {
    list match {
      case Nil     => Nil
      case x :: xs => f(x) :: map(xs, f)
    }
  }

  def filter[T](list: List[T], f: T => Boolean): List[T] = {
    list match {
      case Nil => Nil
      case x :: xs => {
        if (f(x))
          x :: filter(xs, f)
        else
          filter(xs, f)
      }
    }
  }

  def posElems(list: List[Int]): List[Int] = {
    filter(list, (x: Int) => x >= 0)
  }

  def squareList(list: List[Int]): List[Int] = {
    map(list, (x: Int) => x * x)
  }

  def pack[T](list: List[T]): List[List[T]] = {
    list match {
      case Nil => Nil
      case c :: cs => {
        val (first, rest) = list.span(x => x == c)
        first :: pack(rest)
      }
    }
  }

  def runLengthEncode[T](list: List[T]): List[(T, Int)] = {
    pack(list).map(l => (l.head, l.length))
  }

  def reduce[T](list: List[T], op: (T, T) => T, default: T): T = {
    list match {
      case Nil => default
      case x :: xs => {
        op(x, reduce(xs, op, default))
      }
    }
  }

  def sum(list: List[Int]): Int = {
    reduce(list, (x: Int, y: Int) => x + y, 0)
  }

  def prod(list: List[Int]): Int = {
    reduce(list, (x: Int, y: Int) => x * y, 1)
  }

  def sumViaReduceLeft(list: List[Int]): Int = {
    (0 :: list) reduceLeft (_ + _)
  }

  def prodViaReduceLeft(list: List[Int]): Int = {
    (1 :: list) reduceLeft (_ * _)
  }

  def sumViaFoldLeft(list: List[Int]): Int = {
    (list foldLeft 0)(_ + _)
  }

  def prodViaFoldLeft(list: List[Int]): Int = {
    (list foldLeft 1)(_ * _)
  }

  // processes as a tree leaning to the left (accumulator/default first)
  def cFoldLeft[U](z: U)(list: List[U], op: (U, U) => U): U = {
    list match {
      case Nil     => z
      case x :: xs => cFoldLeft(op(z, x))(xs, op)
    }
  }

  def cReduceLeft[U](list: List[U], op: (U, U) => U): U = {
    list match {
      case Nil =>
        throw new Error("cReduceLeft Nil") // there should at least be the 'accumulator' (default value)
      case x :: xs => cFoldLeft(x)(xs, op)
    }
  }

  def sumViaCFoldLeft(list: List[Int]): Int = {
    cFoldLeft(0)(list, _ + _)
  }

  def prodViaCFoldLeft(list: List[Int]): Int = {
    cFoldLeft(1)(list, _ * _)
  }

  // processes as a tree leaning to the right (op first)
  def cFoldRight[U](z: U)(list: List[U], op: (U, U) => U): U = {
    list match {
      case Nil     => z
      case x :: xs => op(x, cFoldRight(z)(xs, op))
    }
  }

  def cReduceRight[U](list: List[U], op: (U, U) => U): U = {
    list match {
      case Nil =>
        throw new Error("cReduceRight Nil") // there should at least be the 'accumulator' (default value)
      case x :: xs => op(x, cFoldRight(x)(xs, op))
    }
  }

  def sumViaCFoldRight(list: List[Int]): Int = {
    cFoldRight(0)(list, _ + _)
  }

  def prodViaCFoldRight(list: List[Int]): Int = {
    cFoldRight(1)(list, _ * _)
  }

  def cMapViaFoldRight[T, U](xs: List[T], f: T => U): List[U] = {
    (xs foldRight (List[U]()))((a, b) => f(a) :: b)
  }

  def cLengthViaFoldRight[T, U](xs: List[T]): Int = {
    (xs foldRight 0)((a, b) => b + 1)
  }

  def doubleViacMap(xs: List[Int]): List[Int] = {
    cMapViaFoldRight(xs, (x: Int) => x * 2)
  }

  /**
    * List filters:
    *
    * filter, filterNot
    * partition = (filter, filterNot)
    *
    * takeWhile, dropWhile
    * span = (takeWhile, dropWhile)
    */
  /** Fold Left and Fold Right
    *
    * - for ops that are communatative and associative, either can be used
    * - but some ops need either left or right foldering
    *
    * - e.g. concat must be foldRight - op-first (concat is only on a list!)
    */
  def cconcat[T](xs: List[T], ys: List[T]): List[T] = {
    (xs foldRight ys)(_ :: _)
  }

  // def cconcatViaLeft[T](xs: List[T], ys: List[T]): List[T] = {
  //   (xs foldLeft ys)(_ :: _) // does not compile
  // }

  def go(): Unit = {
    Utils.banner("Week 5 - List HOFs")

    val list = List(-3, -2, -1, 0, 1, 2, 3)
    val listBy2 = map(list, (x: Int) => x * 2)
    println(s"${list} * 2 = ${listBy2}")

    val listSquared = squareList(list)
    println(s"${list} ^ 2 = ${listSquared}")

    val listPos = posElems(list)
    println(s"${list} +ve = ${listPos}")

    println(s"${list}.filterNot(x => x > 0) = ${list.filterNot(x => x > 0)}")
    println(s"${list}.partition(x => x > 0) = ${list.partition(x => x > 0)}")

    println(s"${list}.takeWhile(x => x < 0) = ${list.takeWhile(x => x < 0)}")
    println(s"${list}.dropWhile(x => x < 0) = ${list.dropWhile(x => x < 0)}")
    println(s"${list}.span(x => x < 0) = ${list.span(x => x < 0)}")

    val dupeList = List('a', 'a', 'a', 'b', 'c', 'c', 'a')
    println(s"pack(${dupeList}) = ${pack(dupeList)}")

    val dupeList2 = List(1, 1, 1, 2, 3, 3, 1)
    println(s"pack(${dupeList2}) = ${pack(dupeList2)}")

    println(s"runLengthEncode(${dupeList}) = ${runLengthEncode(dupeList)}")
    println(s"runLengthEncode(${dupeList2}) = ${runLengthEncode(dupeList2)}")

    println(s"sum(${list}) = ${sum(list)}")
    println(s"sum(${dupeList2}) = ${sum(dupeList2)}")

    println(s"prod(${list}) = ${prod(list)}")
    println(s"prod(${dupeList2}) = ${prod(dupeList2)}")

    println(s"sumViaReduceLeft(${list}) = ${sumViaReduceLeft(list)}")
    println(s"sumViaReduceLeft(${dupeList2}) = ${sumViaReduceLeft(dupeList2)}")

    println(s"prodViaReduceLeft(${list}) = ${prodViaReduceLeft(list)}")
    println(
      s"prodViaReduceLeft(${dupeList2}) = ${prodViaReduceLeft(dupeList2)}"
    )

    println(s"sumViaFoldLeft(${list}) = ${sumViaFoldLeft(list)}")
    println(s"sumViaFoldLeft(${dupeList2}) = ${sumViaFoldLeft(dupeList2)}")

    println(s"prodViaFoldLeft(${list}) = ${prodViaFoldLeft(list)}")
    println(
      s"prodViaFoldLeft(${dupeList2}) = ${prodViaFoldLeft(dupeList2)}"
    )

    println(s"sumViaCFoldLeft(${dupeList2}) = ${sumViaCFoldLeft(dupeList2)}")
    println(s"prodViaCFoldLeft(${dupeList2}) = ${prodViaCFoldLeft(dupeList2)}")

    println(s"sumViaCFoldRight(${dupeList2}) = ${sumViaCFoldRight(dupeList2)}")
    println(
      s"prodViaCFoldRight(${dupeList2}) = ${prodViaCFoldRight(dupeList2)}"
    )

    println(s"doubleViacMap(${dupeList2}) = ${doubleViacMap(dupeList2)}")
    println(
      s"cLengthViaFoldRight(${dupeList2}) = ${cLengthViaFoldRight(dupeList2)}"
    )
  }
}
