package week5

import org.junit._
import org.junit.Assert.assertEquals

class TryOutSortingSuite {
  import TryOutSorting._

  // ------ sort tests -----------------------------------------------------

  @Test def `mergeSort: List(5, 45, 3, 67, 8, 0)` : Unit = {

    val list = List(5, 45, 3, 67, 8, 0)
    val expected = list.sortBy(a => a)

    val actual = mergeSort(list)

    assertEquals(s"mergeSort(${list})", expected, actual)
  }

  @Test def `mergeSort2: List(5, 45, 3, 67, 8, 0)` : Unit = {

    val list = List(5, 45, 3, 67, 8, 0)
    val expected = list.sortBy(a => a)

    val actual = mergeSort2[Int](list, merge2)((x: Int, y: Int) => x < y)

    assertEquals(s"mergeSort2(${list})", expected, actual)
  }

  @Test def `mergeSort2 via pairs: List(5, 45, 3, 67, 8, 0)` : Unit = {

    val list = List(5, 45, 3, 67, 8, 0)
    val expected = list.sortBy(a => a)

    val actual = mergeSort2(list, mergeViaPairs)((x: Int, y: Int) => x < y)

    assertEquals(s"mergeSort2(${list}, mergeViaPairs)", expected, actual)
  }

  @Test def `mergeSort2 Chars: List('a','z','v','c','e')` : Unit = {

    val list = List('a', 'z', 'v', 'c', 'e')
    val expected = list.sortBy(a => a)

    def mergeViaPairs2(left: List[Char], right: List[Char]): List[Char] = {
      mergeViaPairs[Char](left, right)((l: Char, r: Char) => l < r)
    }

    val actual =
      mergeSort2[Char](list, mergeViaPairs2)((x: Char, y: Char) => x < y)

    assertEquals(s"mergeSort2(${list}, mergeViaPairs2)", expected, actual)
  }

  @Test def `mergeSortSimpler Chars: List('a','z','v','c','e')` : Unit = {

    val list = List('a', 'z', 'v', 'c', 'e')
    val expected = list.sortBy(a => a)

    val actual =
      mergeSortSimpler(list)((x, y) => x < y)

    assertEquals(s"mergeSortSimpler(${list})", expected, actual)
  }

  @Test def `mergeSortSimpler String: List("pears", "apple", "mango", "banana")`
      : Unit = {

    val list = List("pears", "apple", "mango", "banana")
    val expected = list.sortBy(a => a)

    val actual =
      mergeSortSimpler(list)((x, y) => (x compareTo y) < 0)

    assertEquals(s"mergeSortSimpler(${list})", expected, actual)
  }

  @Test def `mergeSortSimpler2 String: List("pears", "apple", "mango", "banana")`
      : Unit = {

    val list = List("pears", "apple", "mango", "banana")
    val expected = list.sortBy(a => a)

    val actual =
      mergeSortSimpler2(list)(Ordering.String)

    assertEquals(s"mergeSortSimpler2(${list})", expected, actual)
  }

  @Test def `mergeSortSimpler3 String: List("pears", "apple", "mango", "banana")`
      : Unit = {

    val list = List("pears", "apple", "mango", "banana")
    val expected = list.sortBy(a => a)

    val actual =
      mergeSortSimpler3(list) // compiler figures out to use Ordering.String

    assertEquals(s"mergeSortSimpler3(${list})", expected, actual)
  }

  // ------ config -----------------------------------------------------

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
