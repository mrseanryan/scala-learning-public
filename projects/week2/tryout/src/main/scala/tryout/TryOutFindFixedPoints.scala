package tryoutFindFixedPoints

import scala.annotation.tailrec

object TryOutFindFixedPoints {
  def sum(x: Int, y: Int): Int = {
    x + y
  }

  def abs(x: Double): Double = if (x < 0) -x else x

  // 'Average Damping'
  // Get the average of this and next guess, to avoid oscillations
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  // var tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double) =
    x == y
  // abs((x - y) / x) / x < tolerance

  def fixedPointIter(f: Double => Double)(firstGuess: Double): Double = {
    def iterate(guess: Double): Double = {
      val next = averageDamp(f)(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }

    iterate(firstGuess)
  }

  def lineFun(x: Double): Double = 1 + (x / 2)

  // If y is the sqrt of x,
  // then sqrt(x) is a fixed point of the fun y = x / y
  def sqrt(x: Double): Double = fixedPointIter(y => x / y)(1.0)
}
