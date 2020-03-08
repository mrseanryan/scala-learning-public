package week5

import scala.annotation.tailrec

import Utils._

object TryOutTuple {
  case class CTuple[+T1, +T2](val _1: T1, val _2: T2) {
    override def toString(): String = s"(${_1}, ${_2})"
  }

  def go(): Unit = {
    Utils.banner("Week 5 - tuples")

    val t1 = new CTuple('a', 'b')
    println(s"t1 = ${t1}")
    println(s"t1._1 = ${t1._1}")
    println(s"t1._2 = ${t1._2}")

    val CTuple(t1one, t1two) = t1 // TODO how to avoid CTuple() here?

    val t2 = ('a', 'c') //creates a Tuple2
    val (t2one, t2two) = t2 //Tuple2 can be omitted

    // destructuring via pattern matching.
    // CTuple must be a *case* class!
    t1 match {
      case CTuple(t1a, t1b) => {
        println(s"t1a = ${t1a.toString()}")
        println(s"t1b = ${t1b.toString()}")
      }
    }
  }
}
