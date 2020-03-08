package tryout

import Utils._
import scala.collection.immutable.AbstractSeq

object TryOutMaps {

  def tryoutMaps() = {
    Utils.banner("maps: Roman Numerals")

    val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
    println(s"romanNumerals = ${romanNumerals}")

    println(s"""romanNumerals("V") = ${romanNumerals("V")}""")

    Utils.banner("maps: Roman Capitals")

    val capitals = Map(
      "Italy" -> "Rome",
      "Ireland" -> "Dublin",
      "Netherlands" -> "Amsterdam",
      "Nigeria" -> "Abuja",
      "Uruguay" -> "Montevideo",
      "US" -> "Washington"
    )
    println(s"capitals = ${capitals}")

    Utils.banner("maps: myMap(key) [throws on mising key]")
    println(s"""capitals("Ireland") = ${capitals("Ireland")}""")

    Utils.banner("maps: get()")
    println(s"capitals.get('France') = ${capitals.get("France")}") // get returns Option[String] => Some[String] or None
    println(s"capitals.get('Ireland') = ${capitals.get("Ireland")}")

    Utils.banner("maps: getOrElse()")
    println(s"""capitals.getOrElse("India", "Unknown getOrElse") = ${capitals
      .getOrElse("India", "Unknown getOrElse")}""")

    Utils.banner("maps: withDefaultValue()")
    // Create new map with default for missing keys.
    // This is a 'total function' (no exceptions for missing keys)
    // note: normally, a map is a 'partial function' (since it throws exceptions for missing keys)
    val capitalsWithDefault = capitals.withDefaultValue("<unknown all map>")
    println(
      s"""capitalsWithDefault("India") = ${capitalsWithDefault("India")}"""
    )
    println(
      s"""capitalsWithDefault.getOrElse("India", "Unknown getOrElse") = ${capitalsWithDefault
        .getOrElse("India", "Unknown getOrElse")}"""
    )

    Utils.banner("maps: groupBy()")
    // groupBy
    println(
      s"""capitals.keys.groupBy(k => k.head) = ${capitals.keys.groupBy(k =>
        k.head
      )}"""
    )

    Utils.banner("maps: sorting")
    // sorting
    val countries = capitals.keys.toList

    println(s"sorted = ${countries.sorted}")

    val sortedByLength = countries.sortWith(_.length < _.length)
    println(s"sortedByLength = ${sortedByLength}")
  }

  def tryPolynomials() = {
    Utils.banner("polynomials via maps")
    val x3_minus_2x_plus_5 = new Poly(0 -> 5.0, 1 -> -2.0, 3 -> 1.0) // omitted: 2 -> 0.0
    val _2x3_plus_0p3x2_plus_2x_plus_10 =
      new Poly(3 -> 2, 2 -> 0.3, 1 -> 3, 0 -> 10)

    println(s"x3_minus_2x_plus_5 = ${x3_minus_2x_plus_5}")
    println(
      s"_2x3_plus_0p3x2_plus_2x_plus_10 = ${_2x3_plus_0p3x2_plus_2x_plus_10}"
    )

    println(
      s"x3_minus_2x_plus_5 + _2x3_plus_0p3x2_plus_2x_plus_10 = ${x3_minus_2x_plus_5} ${_2x3_plus_0p3x2_plus_2x_plus_10} = ${x3_minus_2x_plus_5 + _2x3_plus_0p3x2_plus_2x_plus_10}"
    )

    println(
      s"x3_minus_2x_plus_5 ++ _2x3_plus_0p3x2_plus_2x_plus_10 = ${x3_minus_2x_plus_5} ${_2x3_plus_0p3x2_plus_2x_plus_10} = ${x3_minus_2x_plus_5 ++ _2x3_plus_0p3x2_plus_2x_plus_10}"
    )

    println(
      s"x3_minus_2x_plus_5 _plusViaFoldLeft _2x3_plus_0p3x2_plus_2x_plus_10 = ${x3_minus_2x_plus_5} ${_2x3_plus_0p3x2_plus_2x_plus_10} = ${x3_minus_2x_plus_5 _plusViaFoldLeft _2x3_plus_0p3x2_plus_2x_plus_10}"
    )
  }

  class Poly(val _termsExpToCoeff: Map[Int, Double]) {
    // The * here means 'a sequence'
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    def termsExpToCoeff = _termsExpToCoeff.withDefaultValue(0.0)

    // only makes a new map *once*
    def +(other: Poly) = {
      val allExp = termsExpToCoeff.keySet ++ other.termsExpToCoeff.keySet

      val newExpToCoeff = for {
        exp <- allExp,
      } yield {
        val newCoeff = termsExpToCoeff(exp) + other.termsExpToCoeff(exp)
        (exp -> newCoeff)
      }

      new Poly(newExpToCoeff.toMap)
    }

    // Simpler than + (via map)
    // only makes a new map *once*
    def ++(other: Poly) =
      new Poly(termsExpToCoeff ++ other.termsExpToCoeff map adjust)

    def adjust(otherTerms: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = otherTerms
      exp -> (coeff + termsExpToCoeff(exp))
    }

    // I thought this was less efficient than + or ++ since makes a new map each exp (see foldLeftAddTerm)
    //
    // but Odersky says that this more efficient, as avoids the intermediate map
    def _plusViaFoldLeft(other: Poly): Poly = {
      new Poly(
        (other.termsExpToCoeff foldLeft foldLeftDefault)(
          foldLeftAddTerm
        )
      )
    }

    def foldLeftDefault(): Map[Int, Double] = {
      termsExpToCoeff
    }

    def foldLeftAddTerm(
        terms: Map[Int, Double],
        term: (Int, Double)
    ): Map[Int, Double] = {
      val (exp, coeff) = term

      // Makes a new map
      terms + (exp -> (coeff + terms(exp)))
    }

    override def toString(): String = {
      (
        for {
          exp <- termsExpToCoeff.keys.toList.sortWith((a, b) => b < a)
          if (termsExpToCoeff(exp) != 0)
        } yield s"${coeffToString(termsExpToCoeff(exp))}${expToString(exp)}"
      ) mkString " "
    }

    private def coeffToString(coeff: Double): String = {
      coeff match {
        case 1 => "+ "
        case c => {
          if (c < 0) {
            s"- ${-c}"
          } else {
            s"+ ${c}"
          }
        }
      }
    }

    private def expToString(exp: Int): String = {
      exp match {
        case 0 => ""
        case 1 => "x"
        case e => s"x^${e}"
      }
    }
  }

  def go(): Unit = {
    Utils.banner("week 6 - maps")

    tryoutMaps()
    tryPolynomials()
  }
}
