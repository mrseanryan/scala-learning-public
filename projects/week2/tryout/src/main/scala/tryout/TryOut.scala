package tryout

import scala.annotation.tailrec

object TryOut extends App {
  println("Let's try out Scala!")

  def sum(x: Int, y: Int): Int = {
    x + y
  }

  // Use given F to sum all integers between a and b
  def sumWithF(f: Int => Int, a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sumWithF(f, a + 1, b)
  }

  // Use given F to sum all integers between a and b
  def sumWithFTailRecursive(f: Int => Int, a: Int, b: Int): Int = {
    @tailrec
    def loop(x: Int, acc: Int): Int = {
      if (x > b) acc
      else loop(x + 1, acc + f(x))
    }

    loop(a, 0)
  }

  def sumCurried(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = {
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    }

    sumF // returning the function
  }

  // (Int => Int) -> (Int, Int) -> Int
  def sumCurriedShorter(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sumCurriedShorter(f)(a + 1, b)
  }

  def id(x: Int): Int = x
  def cube(x: Int): Int = x * x * x
  def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)

  def sumInts(a: Int, b: Int): Int = sumWithF(id, a, b)
  def sumCubes(a: Int, b: Int): Int = sumWithF(cube, a, b)
  def sumFacts(a: Int, b: Int): Int = sumWithF(fact, a, b)

  def sumCubesAnon(a: Int, b: Int): Int = sumWithF(x => x * x * x, a, b)
  def sumCubesAnonTailRec(a: Int, b: Int): Int =
    sumWithFTailRecursive(x => x * x * x, a, b)
  def sumCubesCurried = sumCurried(x => x * x * x)

  def sumFactsAnon(a: Int, b: Int): Int =
    sumWithF(x => { if (x == 0) 1 else x * fact(x - 1) }, a, b)
  def sumFactsTailRec(a: Int, b: Int): Int = sumWithFTailRecursive(fact, a, b)
  def sumFactsCurried = sumCurried(fact)
  def sumFactsCurriedShorter(a: Int, b: Int): Int =
    sumCurriedShorter(fact)(a, b)

  println("sumInts(1, 3) = ", sumInts(1, 3))
  println("sumCubes(1, 3) = ", sumCubes(1, 3))
  println("sumFacts(1, 3) = ", sumFacts(1, 3))
}
