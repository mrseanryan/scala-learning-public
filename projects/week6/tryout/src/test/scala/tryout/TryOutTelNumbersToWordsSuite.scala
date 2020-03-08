package tryout

import org.junit._
import org.junit.Assert.assertEquals

class TryOutTelNumbersToWordsSuite {
  import TryOutTelNumbersToWords._

  @Test def `generateFromLetters test`: Unit = {
    val actual = generateFromLetters(Seq("AB", "CD", "EF"))
    val expected = Seq("ACE", "ACF", "ADE", "ADF", "BCE", "BCF", "BDE", "BDF")

    assertEquals("generateFromLetters", expected, actual)
  }

  @Test def `wordCode test`: Unit = {
    var actual = wordCode("JAVA")
    var expected = "5282"

    assertEquals("wordCode JAVA", expected, actual)

    var actual2 = wordCode("Java")
    assertEquals("wordCode Java", expected, actual2)
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
