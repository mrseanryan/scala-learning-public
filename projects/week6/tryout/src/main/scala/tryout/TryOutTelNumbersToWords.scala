package tryout

import Utils._
import scala.collection.immutable.AbstractSeq
import scala.io.Source

/** TO READ
  *
  * Lutz Prechelt - An Empirical Comparison of Seven Programming Languages
  * IEEE Computer 33(10): 23-29 - year 2000
  *
  */
object TryOutTelNumbersToWords {
  var mnemonics = Map(
    '2' -> "ABC",
    '3' -> "DEF",
    '4' -> "GHI",
    '5' -> "JKL",
    '6' -> "MNO",
    '7' -> "PQRS",
    '8' -> "TUV",
    '9' -> "WXYZ"
  )

  def translate(phoneNumber: String): Seq[String] = {
    var letters = for {
      n <- phoneNumber
    } yield mnemonics(n)

    // generate all possible words for those letters:
    val generatedWords = generateFromLetters(letters)

    // filter via dictionary
    //
    val words = getDictionaryWords()

    generatedWords.filter(w => words.contains(w))
  }

  // Map from 'A' to 2 etc.
  val charCode: Map[Char, Char] = {
    for {
      (n, letters) <- mnemonics
      l <- letters
    } yield {
      (l -> n)
    }
  }

  // Map from word to numbers - e.g. Java -> 5282
  def wordCode(word: String): String = {
    word.toUpperCase map charCode.withDefaultValue('_')
  }

  // .toList to allow multiple iterations! (else appears empty)
  def getDictionaryWords() =
    Source
      .fromResource("linuxwords.txt")
      .getLines()
      .filter(w => w.forall(l => l.isLetter)) //only letters can be encoded
      .map(w => w.toUpperCase())
      .toList

  // From number, get all the words that match
  // e.g. 5282 -> Java, Kata, Lava ...
  def wordsForNum: Map[String, Seq[String]] = {
    val words = getDictionaryWords()

    words groupBy wordCode withDefaultValue Seq()
  }

  def encode(phoneNumber: String): Set[List[String]] = {
    if (phoneNumber.isEmpty()) {
      Set(List())
    } else {
      (for {
        i <- 1 to phoneNumber.length
        word <- wordsForNum(phoneNumber take i)
        rest <- encode(phoneNumber drop i)
      } yield word :: rest).toSet
    }
  }

  def encodePretty(phoneNumber: String): Seq[String] = {
    encode(phoneNumber)
      .map(_ mkString " ")
      .toSeq
  }

  def encodeNext(phoneNumber: String, i: Int): Set[List[String]] = {
    if (i == phoneNumber.length())
      Set(List())
    else
      encode(phoneNumber.substring(i, phoneNumber.length()))
  }

  def generateFromLetters(letters: Seq[String]): Seq[String] = {
    letters match {
      case Seq() => Seq("")
      case x: Seq[String] => {
        for {
          c <- x.head
          otherLetters <- generateFromLetters(x.tail)
        } yield {
          c.toString() + otherLetters
        }
      }
    }
  }

  def tryTranslate() = {
    testTranslate("72252")
    testTranslate("8649")

    testWordsForNum("72252")
    testWordsForNum("8649")

    // needs multiple words
    testEncode("7225247386")
  }

  def testTranslate(phoneNumber: String) = {
    println(s"translate: ${phoneNumber} -> ${translate(phoneNumber)}")
  }

  def testWordsForNum(phoneNumber: String) = {
    println(s"wordsForNum: ${phoneNumber} -> ${wordsForNum(phoneNumber)}")
  }

  def testEncode(phoneNumber: String) = {
    println(s"encodePretty: ${phoneNumber} -> ${encodePretty(phoneNumber)}")
  }

  def go(): Unit = {
    Utils.banner("week 6 - tel numbers to words")

    tryTranslate()
  }
}
