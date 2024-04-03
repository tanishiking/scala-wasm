package sample

import org.junit.Assert._
import org.junit.Test

class SampleTest {
  @Test
  def testSquare(): Unit = {
    assertEquals(16, Main.square(4))

    // Uncomment to get a test failure
    // assertEquals(16, Main.square(5))
  }

  @Test
  def testException(): Unit = {
    assertThrows(
      classOf[IllegalArgumentException],
      { () =>
        throwIllegalArgument()
      }
    )

    // Uncomment to get a test failure
    /*assertThrows(
      classOf[UnsupportedOperationException],
      { () =>
        throwIllegalArgument()
      }
    )*/
  }

  def throwIllegalArgument(): Unit =
    throw new IllegalArgumentException()
}
