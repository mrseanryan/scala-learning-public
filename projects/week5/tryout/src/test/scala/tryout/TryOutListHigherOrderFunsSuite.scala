package week5

import org.junit._
import org.junit.Assert.assertEquals

class TryOutListHigherOrderFunsSuite {
  import TryOutListHigherOrderFuns._

  // ------ sum tests -----------------------------------------------------

  @Test def `sum: 1 + 1`: Unit =
    assertEquals("1 + 1", 2, sum(1, 1))

  // ------ sort tests -----------------------------------------------------

  // xxx

  // ------ config -----------------------------------------------------

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
