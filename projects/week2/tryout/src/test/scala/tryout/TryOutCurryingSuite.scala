package tryoutCurrying

import org.junit._
import org.junit.Assert.assertEquals

class TryOutCurryingSuite {
  import TryOutCurrying._

  // ------ sum tests -----------------------------------------------------

  @Test def `sum: 1 + 1`: Unit =
    assertEquals("1 + 1", 2, sum(1, 1))

  @Test def `sum: 10 - 11`: Unit =
    assertEquals("10, -11", -1, sum(10, -11))

  // ------ tests -----------------------------------------------------
  // 1. prod fun - calc product of the values of a fun, for the points on a given internal
  // 2. factorial in terms of prod

  @Test def `fact(3):` : Unit =
    assertEquals("fact(3)", 6, fact(3))

  @Test def `fact(5):` : Unit =
    assertEquals("fact(5)", 120, fact(5))

  // -----------------------------------------------------------
  // 3. generatlize both sum and product

  @Test def `sumCubesSeries(1,3):` : Unit =
    assertEquals(
      "sumCubesSeries(1,3)",
      36,
      sumCubesSeries(1, 3)
    )

  @Test def `sumCubesSeries(2,4):` : Unit =
    assertEquals(
      "sumCubesSeries(2,4)",
      99,
      sumCubesSeries(2, 4)
    )

  @Test def `factSeries(3):` : Unit =
    assertEquals("factSeries(3)", 6, factSeries(3))

  @Test def `factSeries(5):` : Unit =
    assertEquals("factSeries(5)", 120, factSeries(5))

  // ------ config -----------------------------------------------------

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
