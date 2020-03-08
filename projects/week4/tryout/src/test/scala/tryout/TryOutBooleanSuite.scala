package week4

import org.junit._
import org.junit.Assert.assertEquals

class TryOutBooleanSuite {
  import TryOutBoolean._

  // ------ sum tests -----------------------------------------------------

  @Test def `sum: 1 + 1`: Unit =
    assertEquals("1 + 1", 2, sum(1, 1))

  @Test def `sum: 10 - 11`: Unit =
    assertEquals("10, -11", -1, sum(10, -11))

  // ------ Boolean tests -----------------------------------------------------

  @Test def `T == T`: Unit = {
    assertEquals("T == T", True == True, True)
    assertEquals("T != F", True == False, False)
    assertEquals("F < T", False < True, True)

    assertEquals("T && T", True && True, True)
    assertEquals("F || T", False || True, True)

    assertEquals("T.unary_!", True.unary_!, False)
    assertEquals("!T", !True, False)

    assertEquals("!F", !False, True)
  }

  // ------ config -----------------------------------------------------

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
