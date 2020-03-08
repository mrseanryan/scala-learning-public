package tryout

import org.junit._
import org.junit.Assert.assertEquals

class TryOutSetsSuite {
  import TryOutSets._

  @Test def `isSafe test - 3 rows`: Unit = {
    val queens = List(3, 0, 2)
    assertEquals("isSafe at 0", false, isSafe(0, queens))
    assertEquals("isSafe at 1", true, isSafe(1, queens))
    assertEquals("isSafe at 2", false, isSafe(2, queens))
    assertEquals("isSafe at 3", false, isSafe(3, queens))
  }

  @Test def `isSafe test - 2 rows`: Unit = {
    val queens = List(2, 0)
    assertEquals("isSafe at 0", false, isSafe(0, queens))
    assertEquals("isSafe at 1", false, isSafe(1, queens))
    assertEquals("isSafe at 2", false, isSafe(2, queens))
    assertEquals("isSafe at 3", false, isSafe(3, queens))
  }

  @Test def `hasConflict test`: Unit = {
    // queens = List(3, 0, 2)

    // vertical conflict:
    (0 to 3).foreach(col => {
      (0 to 3).foreach(row => {
        assertEquals(
          s"hasConflict at ${col} - row ${row}",
          hasConflict(col, col, row),
          true
        )
      })
    })

    // diagonal conflict
    assertEquals("hasConflict at 2 - with row 0", true, hasConflict(2, 3, 1))
    assertEquals("hasConflict at 2 - with row 1", true, hasConflict(2, 0, 2))
  }

  @Test def `queens test - n = 3`: Unit = {
    val expected = Set()
    val actual = queens(3)

    assertEquals("results for n=3", expected, actual)
  }

  @Test def `queens test - n = 4`: Unit = {
    val expected = Set(List(2, 0, 3, 1), List(1, 3, 0, 2))
    val actual = queens(4)

    assertEquals("results for n=4", expected, actual)
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
