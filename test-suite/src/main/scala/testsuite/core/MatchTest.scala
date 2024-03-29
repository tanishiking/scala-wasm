package testsuite.core

import testsuite.Assert.ok

object MatchTest {
  def main(): Unit = {
    testFib()
    testString()
    testNull()
  }

  private def testFib(): Unit = {
    def test(x: Int, expected: Int) = ok(fib(x) == expected)
    def fib(x: Int): Int = x match {
      case 0     => 0
      case 1 | 2 => 1
      case _     => fib(x - 1) + fib(x - 2)
    }
    test(0, 0)
    test(1, 1)
    test(2, 1)
    test(3, 2)
    test(4, 3)
    test(5, 5)
    test(6, 8)
  }

  private def testString(): Unit = {
    def test(x: String, expected: String) = ok(animalSound(x) == expected)
    def animalSound(animal: String) = animal match {
      case "dog" => "bow-wow"
      case "cat" => "meow"
      case _     => "unknown"
    }
    test("dog", "bow-wow")
    test("cat", "meow")
    test("???", "unknown")
  }

  private def testNull(): Unit = {
    def strTest(x: String) = x match {
      case null  => "null"
      case "foo" => "bar"
      case "bar" => "babar"
      case x     => x
    }
    ok(strTest(null) == "null")
    ok(strTest("a") == "a")
  }

  // TODO: Add test case that no default case _
  // we can't test with node v22 at this moment due to the lack of try_table
}
