package tryoutCurrying

import scala.annotation.tailrec

object TryOutCurrying {
  def sum(x: Int, y: Int): Int = {
    x + y
  }

  // -----------------------------------------------------------
  // 1. prod fun - calc product of the values of a fun, for the points on a given internal
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  def series(f: Int => Int, unit: Int, combine: (Int, Int) => Int)(
      a: Int,
      b: Int
  ): Int = {
    if (a > b) unit
    else combine(f(a), series(f, unit, combine)(a + 1, b))
  }

  // -----------------------------------------------------------
  // 2. factorial in terms of prod
  def fact(a: Int): Int = {
    product(a => a)(1, a)
  }

  // -----------------------------------------------------------
  // 3. generalize both sum and product

  def productSeries(f: Int => Int)(a: Int, b: Int) = {
    series(f, 1, (a, b) => a * b)(a, b)
  }

  def factSeries(a: Int): Int = {
    productSeries(a => a)(1, a)
  }

  def sumSeries(f: Int => Int)(a: Int, b: Int) = {
    series(f, 0, (a, b) => a + b)(a, b)
  }

  def sumCubesSeries(a: Int, b: Int) = sumSeries(x => x * x * x)(a, b)
}
