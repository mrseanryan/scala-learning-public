package week5

import org.junit._
import org.junit.Assert.assertEquals

class TryOutCustomListFunsSuite {
  import TryOutCustomListFuns._

  // ------ sum tests -----------------------------------------------------

  @Test def `sum: 1 + 1`: Unit =
    assertEquals("1 + 1", 2, sum(1, 1))

  @Test def `sum: 10 - 11`: Unit =
    assertEquals("10, -11", -1, sum(10, -11))

  // ------ config -----------------------------------------------------

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
