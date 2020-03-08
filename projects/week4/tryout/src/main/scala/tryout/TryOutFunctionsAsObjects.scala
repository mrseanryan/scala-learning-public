package week4

import scala.annotation.tailrec

import list._
import Utils._

object TryOutFunctionsAsObjects {

  def square(x: Int) = x * x

  def squareAsObject = {
    class AnnonFun extends Function1[Int, Int] {
      def apply(x: Int) = x * x
    }

    new AnnonFun
  }

  def squareAsObjectViaAnonClass = new Function1[Int, Int] {
    def apply(x: Int) = x * x
  }

  object List {
    def apply[T](): List[T] = Nil

    def apply[T](x1: T): List[T] = new Cons(x1)

    def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2))
  }

  def go(): Unit = {
    Utils.banner("Week 4 - Functions as Objects")

    println(s"square(3) = ${square(3)}")
    println(s"squareAsObject(4) = ${squareAsObject(4)}")
    println(s"squareAsObjectViaAnonClass(5) = ${squareAsObjectViaAnonClass(5)}")

    println(s"List() = ${List()}")
    println(s"List(1) = ${List(1)}")
    println(s"List(1,2) = ${List(1, 2)}")
  }
}
