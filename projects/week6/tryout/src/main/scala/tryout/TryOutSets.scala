package tryout

import Utils._
import scala.collection.immutable.AbstractSeq

object TryOutSets {

  /**
    * Sets (vs sequences)
    *
    * - sets are unordered
    * - sets do not have duplicate elements
    * - the fundamental operation is contains
    */
  def tryoutSets() = {
    val fruit = Set("apple", "banana", "mango")

    val app = fruit filter (_.startsWith("app"))

    val oneToSix = (1 to 6).toSet

    val oneToSixPlusTwo = oneToSix.map(_ + 2)

    // ref: https://www.scala-lang.org/api/current/scala/collection/Iterable.html
  }

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0)
        Set(List())
      else {
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if (isSafe(col, queens))
        } yield col :: queens
      }
    }

    placeQueens(n)
  }

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val rows = 1 to queens.length
    val rowsAndQueens = rows zip queens

    rowsAndQueens.forall {
      case (r, c) => !hasConflict(col, c, r)
    }
  }

  def hasConflict(col: Int, colOther: Int, rowOther: Int): Boolean = {
    col == colOther || math.abs(col - colOther) == rowOther
  }

  def queensAndPrint(n: Int) = {
    printQueens(queens(n), n)
  }

  def tryoutQueens() = {
    queensAndPrint(4)
    queensAndPrint(8)
    queensAndPrint(10)
  }

  def printQueens(result: Set[List[Int]], n: Int) = {
    println(s"results: ${result}\n${result.size}")

    result.foreach(r => println(queensToString(r) + "\n"))

    println(s"${result.size} solutions for n=${n}")
  }

  def queensToString(result: List[Int]): String = {
    val lines =
      for (col <- result.reverse)
        yield Vector.fill(result.length)("* ").updated(col, "Q ").mkString

    // mkString is like 'join'
    "\n" + (lines mkString "\n")
  }

  def go(): Unit = {
    Utils.banner("week 6 - sets")

    tryoutSets()
    tryoutQueens()
  }
}
