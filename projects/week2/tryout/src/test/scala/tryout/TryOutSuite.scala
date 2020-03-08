package tryout

import org.junit._
import org.junit.Assert.assertEquals

class TryOutSuite {
  import TryOut._

  // ------ sum tests -----------------------------------------------------

  @Test def `sum: 1 + 1`: Unit =
    assertEquals("1 + 1", 2, sum(1, 1))

  @Test def `sum: 10 - 11`: Unit =
    assertEquals("10, -11", -1, sum(10, -11))

  // ------ sumInts tests -----------------------------------------------------

  @Test def `sumInts(1,3):` : Unit =
    assertEquals("sumInts(1,3)", 6, sumInts(1, 3))

  @Test def `sumInts(2,4):` : Unit =
    assertEquals("sumInts(2,4)", 9, sumInts(2, 4))

  // ------ sumCubes tests -----------------------------------------------------

  @Test def `sumCubes(1,3):` : Unit =
    assertEquals("sumCubes(1,3)", 36, sumCubes(1, 3))

  @Test def `sumCubes(2,4):` : Unit =
    assertEquals("sumCubes(2,4)", 99, sumCubes(2, 4))

  // ------ sumCubesAnon tests -----------------------------------------------------

  @Test def `sumCubesAnon(1,3):` : Unit =
    assertEquals("sumCubesAnon(1,3)", 36, sumCubesAnon(1, 3))

  @Test def `sumCubesAnon(2,4):` : Unit =
    assertEquals("sumCubesAnon(2,4)", 99, sumCubesAnon(2, 4))

  // ------ sumCubesAnon tests -----------------------------------------------------

  @Test def `sumCubesAnonTailRec(1,3):` : Unit =
    assertEquals("sumCubesAnonTailRec(1,3)", 36, sumCubesAnonTailRec(1, 3))

  @Test def `sumCubesAnonTailRec(2,4):` : Unit =
    assertEquals("sumCubesAnonTailRec(2,4)", 99, sumCubesAnonTailRec(2, 4))

  // ------ sumCubesCurried tests -----------------------------------------------------

  @Test def `sumCubesCurried(1,3):` : Unit =
    assertEquals(
      "sumCubesCurried(1,3)",
      36,
      sumCubesCurried(1, 3)
    )

  @Test def `sumCubesCurried(2,4):` : Unit =
    assertEquals(
      "sumCubesCurried(2,4)",
      99,
      sumCubesCurried(2, 4)
    )

  // ------ sumFacts tests -----------------------------------------------------

  @Test def `sumFacts(1,3):` : Unit =
    assertEquals("sumFacts(1,3)", 9, sumFacts(1, 3))

  @Test def `sumFacts(2,4):` : Unit =
    assertEquals("sumFacts(2,4)", 32, sumFacts(2, 4))

  // ------ sumFactsAnon tests -----------------------------------------------------

  @Test def `sumFactsAnon(1,3):` : Unit =
    assertEquals("sumFactsAnon(1,3)", 9, sumFactsAnon(1, 3))

  @Test def `sumFactsAnon(2,4):` : Unit =
    assertEquals("sumFactsAnon(2,4)", 32, sumFactsAnon(2, 4))

  // ------ sumFactsTailRec tests -----------------------------------------------------

  @Test def `sumFactsTailRec(1,3):` : Unit =
    assertEquals("sumFactsTailRec(1,3)", 9, sumFactsTailRec(1, 3))

  @Test def `sumFactsTailRec(2,4):` : Unit =
    assertEquals("sumFactsTailRec(2,4)", 32, sumFactsTailRec(2, 4))

  // ------ sumFactsCurried tests -----------------------------------------------------

  @Test def `sumFactsCurried(1,3):` : Unit =
    assertEquals("sumFactsCurried(1,3)", 9, sumFactsCurried(1, 3))

  @Test def `sumFactsCurried(2,4):` : Unit =
    assertEquals("sumFactsCurried(2,4)", 32, sumFactsCurried(2, 4))

  // ------ sumFactsCurriedShorter tests -----------------------------------------------------

  @Test def `sumFactsCurriedShorter(1,3):` : Unit =
    assertEquals("sumFactsCurriedShorter(1,3)", 9, sumFactsCurriedShorter(1, 3))

  @Test def `sumFactsCurriedShorter(2,4):` : Unit =
    assertEquals(
      "sumFactsCurriedShorter(2,4)",
      32,
      sumFactsCurriedShorter(2, 4)
    )

  // ------ config -----------------------------------------------------

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
