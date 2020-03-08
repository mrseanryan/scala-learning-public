package tryout

import org.junit._
import org.junit.Assert.assertEquals

class TryOutClassSuite {
  import TryOutClass._

  // ------ tests -----------------------------------------------------

  @Test def `sum: 1 + 1`: Unit =
    assertEquals("1 + 1", 2, sum(1, 1))

  @Test def `sum: 10 - 11`: Unit =
    assertEquals("10, -11", -1, sum(10, -11))

  @Test def `Rational test`: Unit = {
    val rat = new Rational(1, 2)
    assertEquals("rat.num", 1, rat.numer)
    assertEquals("rat.denom", 2, rat.denom)
  }

  @Test def `Rational.add test`: Unit = {
    val rat1 = new Rational(1, 2)
    val rat2 = new Rational(1, 3)

    val actual = rat1.add((rat2))
    assertEquals("rat.add num", 5, actual.numer)
    assertEquals("rat.add denom", 6, actual.denom)
  }

  @Test def `Rational.add test 2`: Unit = {
    val rat1 = new Rational(2, 3)
    val rat2 = new Rational(1, 7)

    val actual = rat1.add((rat2))
    assertEquals("rat.add num", 17, actual.numer)
    assertEquals("rat.add denom", 21, actual.denom)
  }

  @Test def `Rational + test`: Unit = {
    val rat1 = new Rational(1, 2)
    val rat2 = new Rational(1, 3)

    val actual = rat1 + rat2
    assertEquals("rat.add num", 5, actual.numer)
    assertEquals("rat.add denom", 6, actual.denom)
  }

  @Test def `Rational.sub test`: Unit = {
    val rat1 = new Rational(1, 2)
    val rat2 = new Rational(1, 4)

    val actual = rat1.sub((rat2))
    assertEquals("rat.sub num", 1, actual.numer)
    assertEquals("rat.sub denom", 4, actual.denom)
  }

  @Test def `Rational.subViaNeg test`: Unit = {
    val rat1 = new Rational(1, 2)
    val rat2 = new Rational(1, 4)

    val actual = rat1.subViaNeg((rat2))
    assertEquals("rat.subViaNeg num", 1, actual.numer)
    assertEquals("rat.subViaNeg denom", 4, actual.denom)
  }

  @Test def `Rational.sub via - test`: Unit = {
    val rat1 = new Rational(1, 2)
    val rat2 = new Rational(1, 4)

    val actual = rat1 - rat2
    assertEquals("rat.subViaNeg num", 1, actual.numer)
    assertEquals("rat.subViaNeg denom", 4, actual.denom)
  }

  @Test def `Rational isLessThan test`: Unit = {
    val rat1 = new Rational(1, 2)
    val rat2 = new Rational(1, 4)

    val actual = rat1 isLessThan rat2
    assertEquals(s"rat $rat1 < $rat2", false, actual)

    val actual2 = rat2 isLessThan rat1
    assertEquals(s"rat $rat2 < $rat1", true, actual2)
  }

  @Test def `Rational < test`: Unit = {
    val rat1 = new Rational(1, 2)
    val rat2 = new Rational(1, 4)

    val actual = rat1 < rat2
    assertEquals(s"rat $rat1 < $rat2", false, actual)

    val actual2 = rat2 < rat1
    assertEquals(s"rat $rat2 < $rat1", true, actual2)
  }

  @Test def `Rational unary - test`: Unit = {
    val rat1 = new Rational(1, 2)

    val actual = -rat1
    assertEquals("rat unary - num", -1, actual.numer)
    assertEquals("rat unary - denom", 2, actual.denom)
  }

  @Test def `Rational neg test`: Unit = {
    val rat1 = new Rational(1, 2)

    val actual = rat1.neg()
    assertEquals("rat neg - num", -1, actual.numer)
    assertEquals("rat neg - denom", 2, actual.denom)
  }

  @Test def `Rational calc 3 test`: Unit = {
    val rat1 = new Rational(1, 3)
    val rat2 = new Rational(5, 7)
    val rat3 = new Rational(3, 2)

    val actual = rat1.subViaNeg(rat2).subViaNeg(rat3)
    assertEquals("rat.subViaNeg num", -79, actual.numer)
    assertEquals("rat.subViaNeg denom", 42, actual.denom)
  }

  @Test def `Rational denom zero`: Unit = {
    try {
      new Rational(1, 0)
      Assert.fail("No exception has been thrown")
    } catch {
      case e: IllegalArgumentException => ()
    }
  }
  // ------ config -----------------------------------------------------

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
