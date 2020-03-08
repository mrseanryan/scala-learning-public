package week5

import Utils._

object TryOutCustomListFuns {
  def sum(a: Int, b: Int): Int = a + b

  def listMethods(): Unit = {
    val list = List(1, 2, 3, 4, 5)
    println(s"list = ${list}")

    basicListMethods(list)
    creatingListMethods(list)
    findingListMethods(list)
  }

  def basicListMethods(list: List[Int]) = {
    Utils.banner("basic list methods")

    println(s"list.length = ${list.length}")
    println(s"list.last = ${list.last}")
    println(s"customLast(list) = ${customLast(list)}")

    println(s"list.init = ${list.init}")
    println(s"customInit(list) = ${customInit(list)}")

    println(s"list.take(3) = ${list.take(3)}")
    println(s"list.take(7) = ${list.take(7)}")
    println(s"list.drop(2) = ${list.drop(2)}")
    println(s"list.drop(8) = ${list.drop(8)}")
    println(s"list(2) = ${list(2)}")
    println(s"list.apply(2) = ${list.apply(2)}")
    println(
      s"list.applyOrElse(7, x => x * -1) = ${list.applyOrElse(7, (x: Int) => x * -1)}"
    )
  }

  def creatingListMethods(list: List[Int]) = {
    Utils.banner("creating list methods")

    val list2 = List(10, 11, 12)
    println(s"list2 = ${list2}")

    println(s"list ++ list2 = ${list ++ list2}")
    println(s"list ::: list2 = ${list ::: list2}")
    println(s"customConcat(list, list2) = ${customConcat(list, list2)}")

    println(s"list.reverse = ${list.reverse}")
    println(s"customReverse(list) = ${customReverse(list)}")
    println(s"customReverse2(list) = ${customReverse2(list)}")

    println(s"customRemove(list, 0) = ${customRemove(list, 0)}")
    println(s"customRemove(list, 3) = ${customRemove(list, 3)}")
    println(s"customRemove(list, 4) = ${customRemove(list, 4)}")

    println(
      s"customFlatten2(List(List(1,1),2,List(3, List(5,8)))) = ${customFlatten2(
        List(List(1, 1), 2, List(3, List(5, 8)))
      )}"
    )
    println(
      s"customFlatten3(List(List(1,1),2,List(3, List(5,8)))) = ${customFlatten3(
        List(List(1, 1), 2, List(3, List(5, 8)))
      )}"
    )

    println(
      s"(list ++ list2).updated(7,-2) = ${(list ++ list2).updated(7, -2)}"
    )
  }

  def findingListMethods(list: List[Int]) = {
    Utils.banner("finding list methods")

    println(s"list.indexOf(3) = ${list.indexOf(3)}")
    println(s"list.contains(3) = ${list.contains(3)}")
    println(s"list.contains(-1) = ${list.contains(-1)}")
  }

  // Complexity = O(|xs|) = O(xs.length)
  def customLast[T](xs: List[T]): T = {
    xs match {
      case List()  => throw new Error("Empty list")
      case List(x) => x
      case y :: ys => customLast(ys)
    }
  }

  // All except last
  // Complexity = O(|xs|) = O(xs.length)
  def customInit[T](xs: List[T]): List[T] = {
    xs match {
      case List()  => throw new Error("Empty list")
      case List(x) => List()
      case y :: ys => y :: customInit(ys)
    }
  }

  // Complexity = O(|list|) = O(list.length)
  def customConcat[T](list: List[T], list2: List[T]): List[T] = {
    list match {
      case List()  => list2
      case x :: xs => x :: customConcat(xs, list2)
    }
  }

  // Complexity = O(|list|^2)
  def customReverse[T](list: List[T]): List[T] = {
    list match {
      case List()  => List()
      case List(x) => List(x)
      case x :: xs =>
        xs.last :: customReverse(x :: xs.init) // O(last) + O(customReverse) = O(n) + O(n) = O(n^2)
    }
  }

  // Complexity = O(|list|^2)
  def customReverse2[T](list: List[T]): List[T] = {
    list match {
      case List() => List()
      case x :: xs =>
        customReverse(xs) ++ List(x) // O(customReverse) + O(++) = O(n) + O(n) = O(n^2)
    }
  }

  def customRemove[T](list: List[T], n: Int): List[T] = {
    require(n >= 0)

    list.take(n) ++ list.drop(n + 1)
  }

  // ref: https://stackoverflow.com/questions/13059590/scala-flatten-list
  def customFlatten2[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    // case head :: Nil => List(head)
    case head :: tail =>
      (head match {
        case l: List[T] => customFlatten2(l)
        case i          => List(i)
      }) ::: customFlatten2(tail)
  }

  // ref: https://stackoverflow.com/questions/13059590/scala-flatten-list
  def customFlatten3(xs: List[Any]): List[Any] = xs match {
    case Nil                     => Nil
    case (head: List[_]) :: tail => customFlatten3(head) ++ customFlatten3(tail)
    case head :: tail            => head :: customFlatten3(tail)
  }

  def go(): Unit = {
    Utils.banner("Week 5 - custom list funs")

    listMethods()
  }
}
