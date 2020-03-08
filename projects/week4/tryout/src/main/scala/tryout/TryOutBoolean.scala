package week4

import scala.annotation.tailrec

import Utils._

object TryOutBoolean {
  def sum(x: Int, y: Int): Int = {
    x + y
  }

  object True extends Boolean {
    def ifThenElse[T](t: => T, e: => T) = t
  }

  object False extends Boolean {
    def ifThenElse[T](t: => T, e: => T) = e
  }

  abstract class Boolean {
    def ifThenElse[T](t: => T, e: => T): T

    def &&(x: => Boolean): Boolean = ifThenElse(x, False)
    def ||(x: => Boolean): Boolean = ifThenElse(True, x)
    def unary_! = ifThenElse(False, True)

    def ==(x: => Boolean): Boolean = ifThenElse(x, !x)
    def !=(x: => Boolean): Boolean =
      !(this == x) // ifThenElse(x.unary_!, x)

    // false < true
    def <(x: => Boolean): Boolean = ifThenElse(False, x)
  }

  def go(): Unit = {
    Utils.banner("Week 4 - Boolean")
  }
}
