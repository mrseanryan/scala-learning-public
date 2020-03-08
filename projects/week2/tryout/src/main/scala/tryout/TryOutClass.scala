package tryout

import scala.annotation.tailrec

object TryOutClass {
  def sum(x: Int, y: Int): Int = {
    x + y
  }

  class Rational(x: Int, y: Int) {
    // contract:
    require(y != 0, "denom must not be zero!")

    // constructor overload:
    def this(x: Int) = this(x, 1)

    private def abs(x: Int): Int = if (x < 0) -x else x

    // Euclid's algorithm
    @tailrec
    private def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b) // last action is call-self = tail recursion which is efficient as iteration (const stack size)

    // for perf, could move gcd() call to numer, denom - if they are NOT frequently called
    private val g = abs(gcd(x, y))

    // for perf, could use val instead of def (if called often)
    def numer = x / g
    def denom = y / g

    def add(other: Rational): Rational = {
      new Rational(
        numer * other.denom + other.numer * denom,
        denom * other.denom
      )
    }

    def + = add(_)

    def - = sub(_)

    def isLessThan(other: Rational): Boolean =
      numer * other.denom < other.numer * denom

    def < = isLessThan(_)

    def max(other: Rational): Rational =
      if (isLessThan(other)) other else this

    def neg(): Rational =
      new Rational(
        -numer,
        denom
      )

    def unary_- = neg

    def sub(other: Rational): Rational = {
      new Rational(
        numer * other.denom - other.numer * denom,
        denom * other.denom
      )
    }

    def subViaNeg(other: Rational): Rational = {
      add(other.neg())
    }

    override def toString(): String = s"{$numer}/{$denom}"
  }
}

// operator precedence (lowest to highest)
//
// (like C++)
/*
  (all letters)
  |
  ^
  &
  < >
  = !
  :
  + -
 * / %
  (all other special characters)
 */
// exercise:  a + b ^? c ?^ d less a ==> b | c
// equivalent to: (((a + b) ^? (c ?^ d)) less ((a ==> b) | c))
