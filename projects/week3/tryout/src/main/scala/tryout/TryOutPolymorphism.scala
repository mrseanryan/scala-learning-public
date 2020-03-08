package week3

import scala.annotation.tailrec

import Utils._

object TryOutPolymorphism {
  def sum(x: Int, y: Int): Int = {
    x + y
  }

  trait Describable {
    def getDescription(): String
    override def toString(): String = getDescription()
  }

  // a generic trait (with type parameter T)
  trait List[T] extends Describable {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
  }

  class Cons[T](val head: T, val tail: List[T] = new Nil[T]) extends List[T] {
    def isEmpty: Boolean = false
    override def getDescription(): String = s"${head}, ${tail}"
  }

  class Nil[T] extends List[T] {
    def isEmpty = true
    // Nothing is a subtype of any other type
    def head: Nothing = throw new NoSuchElementException("Nil.head")
    def tail: Nothing = throw new NoSuchElementException("Nil.tail")
    override def getDescription(): String = s"."
  }

  def singleton[T](elem: T): List[T] = new Cons(elem, new Nil[T])

  def len[T](list: List[T]): Int = {
    @tailrec
    def len_inner(thisList: List[T], acc: Int): Int = {
      if (thisList.isEmpty)
        acc
      else
        len_inner(thisList.tail, acc + 1)
    }

    len_inner(list, 0)
  }

  @tailrec
  def nth[T](list: List[T], index: Int): T = {
    if (list.isEmpty)
      throw new IndexOutOfBoundsException("Index out of range")

    if (index == 0)
      list.head
    else
      nth(list.tail, index - 1)
  }

  def go(): Unit = {
    Utils.banner("Polymorphism - with lists")

    println("singleton(1) = " + singleton[Int](1).toString())
    println("singleton(true) = " + singleton(true).toString())

    val list123 = new Cons(1, new Cons(2, new Cons(3)))
    println("Cons(1, new Cons(2, new Cons(3))) = " + list123.toString())

    try {
      println("nth(list123, -1) = ", nth(list123, -1))
    } catch {
      case e: IndexOutOfBoundsException => println(e.getMessage())
    }
    println("nth(list123, 0) = ", nth(list123, 0))
    println("nth(list123, 1) = ", nth(list123, 1))
    println("nth(list123, 2) = ", nth(list123, 2))
    try {
      println("nth(list123, 3) = ", nth(list123, 3))
    } catch {
      case e: IndexOutOfBoundsException => println(e.getMessage())
    }
  }
}
