package tryoutFindFixedPoints

import org.junit._
import org.junit.Assert.assertEquals

class TryOutFindFixedPointsSuite {
  import TryOutFindFixedPoints._

  // ------ sum tests -----------------------------------------------------

  @Test def `sum: 1 + 1`: Unit =
    assertEquals("1 + 1", 2, sum(1, 1))

  @Test def `sum: 10 - 11`: Unit =
    assertEquals("10, -11", -1, sum(10, -11))

  // ------ tests -----------------------------------------------------

  @Test def `fixedPointIterativeTest`: Unit = {
    val tolerance = 0.001
    val actual = fixedPointIter(lineFun)(1)
    val expected = 2

    assert(
      abs(expected - actual) < tolerance,
      s"Actual {actual} is too different from expected {expected}"
    )
  }

  @Test def `sqrtTest`: Unit = {
    val tolerance = 0.001
    val actual = sqrt(25)
    val expected = 5

    assert(
      abs(expected - actual) < tolerance,
      s"Actual {actual} is too different from expected {expected}"
    )
  }

  // ------ config -----------------------------------------------------

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
