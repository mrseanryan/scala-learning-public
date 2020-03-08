package week4

import scala.annotation.tailrec

import Utils._

object TryOutNat {
  def sum(x: Int, y: Int): Int = {
    x + y
  }

  // Peano numbers
  // Non-negative integers
  //
  // Pure OO - no primitive types!
  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat
    def +(that: Nat): Nat
    def -(that: Nat): Nat

    override def toString(): String = {
      def inner(that: Nat, acc: Int): Int = {
        if (that.isZero)
          acc
        else
          inner(that.predecessor, acc + 1)
      }

      inner(this, 0).toString()
    }
  }

  object Zero extends Nat {
    def isZero: Boolean = true
    def predecessor: Nat = noNegativeNumbers
    def successor: Nat = new NatNumber(this)
    def +(that: Nat): Nat = that
    def -(that: Nat): Nat = if (that.isZero) this else noNegativeNumbers

    private def noNegativeNumbers =
      throw new NoSuchElementException("No -ve numbers")
  }

  class NatNumber(pre: Nat) extends Nat {
    // require(value > 0, "value must be +ve")

    def isZero: Boolean = false

    def predecessor: Nat = pre

    def successor: Nat = new NatNumber(this)

    def +(that: Nat): Nat = new NatNumber(pre + that)

    def -(that: Nat): Nat = if (that.isZero) this else pre - that.predecessor
  }

  def go(): Unit = {
    Utils.banner("Week 4 - Nat")

    val one = new NatNumber(Zero)
    val two = new NatNumber(one)
    val three = new NatNumber(two)

    println(s"one = ${one}")
    println(s"two = ${two}")
    println(s"three = ${three}")

    println(s"one + one = ${one + one}")
    println(s"one + three = ${one + three}")

    println(s"three - two = ${three - two}")
  }
}
