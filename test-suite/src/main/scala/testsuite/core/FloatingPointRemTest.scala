package testsuite.core

import testsuite.Assert._

/** Tests for `Float_%` and `Double_%`, which do not have Wasm equivalent.
  *
  * They are specified by https://262.ecma-international.org/#sec-numeric-types-number-remainder
  */
object FloatingPointRemTest {
  def main(): Unit = {
    testFloat()
    testDouble()
  }

  def testFloat(): Unit = {
    def test(expected: Float, x: Float, y: Float): Unit =
      assertSame(expected, x % y)

    // If n is NaN, return NaN
    test(Float.NaN, Float.NaN, Float.NaN)
    test(Float.NaN, Float.NaN, Float.PositiveInfinity)
    test(Float.NaN, Float.NaN, Float.NegativeInfinity)
    test(Float.NaN, Float.NaN, +0.0f)
    test(Float.NaN, Float.NaN, -0.0f)
    test(Float.NaN, Float.NaN, 2.1f)
    test(Float.NaN, Float.NaN, 5.5f)
    test(Float.NaN, Float.NaN, -151.189f)

    // If d is NaN, return NaN
    test(Float.NaN, Float.NaN, Float.NaN)
    test(Float.NaN, Float.PositiveInfinity, Float.NaN)
    test(Float.NaN, Float.NegativeInfinity, Float.NaN)
    test(Float.NaN, +0.0f, Float.NaN)
    test(Float.NaN, -0.0f, Float.NaN)
    test(Float.NaN, 2.1f, Float.NaN)
    test(Float.NaN, 5.5f, Float.NaN)
    test(Float.NaN, -151.189f, Float.NaN)

    // If n is PositiveInfinity, return NaN
    test(Float.NaN, Float.PositiveInfinity, Float.PositiveInfinity)
    test(Float.NaN, Float.PositiveInfinity, Float.NegativeInfinity)
    test(Float.NaN, Float.PositiveInfinity, +0.0f)
    test(Float.NaN, Float.PositiveInfinity, -0.0f)
    test(Float.NaN, Float.PositiveInfinity, 2.1f)
    test(Float.NaN, Float.PositiveInfinity, 5.5f)
    test(Float.NaN, Float.PositiveInfinity, -151.189f)

    // If n is NegativeInfinity, return NaN
    test(Float.NaN, Float.NegativeInfinity, Float.PositiveInfinity)
    test(Float.NaN, Float.NegativeInfinity, Float.NegativeInfinity)
    test(Float.NaN, Float.NegativeInfinity, +0.0f)
    test(Float.NaN, Float.NegativeInfinity, -0.0f)
    test(Float.NaN, Float.NegativeInfinity, 2.1f)
    test(Float.NaN, Float.NegativeInfinity, 5.5f)
    test(Float.NaN, Float.NegativeInfinity, -151.189f)

    // If d is PositiveInfinity, return n
    test(+0.0f, +0.0f, Float.PositiveInfinity)
    test(-0.0f, -0.0f, Float.PositiveInfinity)
    test(2.1f, 2.1f, Float.PositiveInfinity)
    test(5.5f, 5.5f, Float.PositiveInfinity)
    test(-151.189f, -151.189f, Float.PositiveInfinity)

    // If d is NegativeInfinity, return n
    test(+0.0f, +0.0f, Float.NegativeInfinity)
    test(-0.0f, -0.0f, Float.NegativeInfinity)
    test(2.1f, 2.1f, Float.NegativeInfinity)
    test(5.5f, 5.5f, Float.NegativeInfinity)
    test(-151.189f, -151.189f, Float.NegativeInfinity)

    // If d is +0.0, return NaN
    test(Float.NaN, +0.0f, +0.0f)
    test(Float.NaN, -0.0f, +0.0f)
    test(Float.NaN, 2.1f, +0.0f)
    test(Float.NaN, 5.5f, +0.0f)
    test(Float.NaN, -151.189f, +0.0f)

    // If d is -0.0, return NaN
    test(Float.NaN, +0.0f, -0.0f)
    test(Float.NaN, -0.0f, -0.0f)
    test(Float.NaN, 2.1f, -0.0f)
    test(Float.NaN, 5.5f, -0.0f)
    test(Float.NaN, -151.189f, -0.0f)

    // If n is +0.0, return n
    test(+0.0f, +0.0f, 2.1f)
    test(+0.0f, +0.0f, 5.5f)
    test(+0.0f, +0.0f, -151.189f)

    // If n is -0.0, return n
    test(-0.0f, -0.0f, 2.1f)
    test(-0.0f, -0.0f, 5.5f)
    test(-0.0f, -0.0f, -151.189f)

    // Non-special values
    // { val l = List(2.1f, 5.5f, -151.189f); for (n <- l; d <- l) println(s"    test(${n % d}f, ${n}f, ${d}f)") }
    test(0.0f, 2.1f, 2.1f)
    test(2.1f, 2.1f, 5.5f)
    test(2.1f, 2.1f, -151.189f)
    test(1.3000002f, 5.5f, 2.1f)
    test(0.0f, 5.5f, 5.5f)
    test(5.5f, 5.5f, -151.189f)
    test(-2.0890021f, -151.189f, 2.1f)
    test(-2.6889954f, -151.189f, 5.5f)
    test(-0.0f, -151.189f, -151.189f)
  }

  def testDouble(): Unit = {
    def test(expected: Double, n: Double, d: Double): Unit =
      assertSame(expected, n % d)

    // If n is NaN, return NaN
    test(Double.NaN, Double.NaN, Double.NaN)
    test(Double.NaN, Double.NaN, Double.PositiveInfinity)
    test(Double.NaN, Double.NaN, Double.NegativeInfinity)
    test(Double.NaN, Double.NaN, +0.0)
    test(Double.NaN, Double.NaN, -0.0)
    test(Double.NaN, Double.NaN, 2.1)
    test(Double.NaN, Double.NaN, 5.5)
    test(Double.NaN, Double.NaN, -151.189)

    // If d is NaN, return NaN
    test(Double.NaN, Double.NaN, Double.NaN)
    test(Double.NaN, Double.PositiveInfinity, Double.NaN)
    test(Double.NaN, Double.NegativeInfinity, Double.NaN)
    test(Double.NaN, +0.0, Double.NaN)
    test(Double.NaN, -0.0, Double.NaN)
    test(Double.NaN, 2.1, Double.NaN)
    test(Double.NaN, 5.5, Double.NaN)
    test(Double.NaN, -151.189, Double.NaN)

    // If n is PositiveInfinity, return NaN
    test(Double.NaN, Double.PositiveInfinity, Double.PositiveInfinity)
    test(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity)
    test(Double.NaN, Double.PositiveInfinity, +0.0)
    test(Double.NaN, Double.PositiveInfinity, -0.0)
    test(Double.NaN, Double.PositiveInfinity, 2.1)
    test(Double.NaN, Double.PositiveInfinity, 5.5)
    test(Double.NaN, Double.PositiveInfinity, -151.189)

    // If n is NegativeInfinity, return NaN
    test(Double.NaN, Double.NegativeInfinity, Double.PositiveInfinity)
    test(Double.NaN, Double.NegativeInfinity, Double.NegativeInfinity)
    test(Double.NaN, Double.NegativeInfinity, +0.0)
    test(Double.NaN, Double.NegativeInfinity, -0.0)
    test(Double.NaN, Double.NegativeInfinity, 2.1)
    test(Double.NaN, Double.NegativeInfinity, 5.5)
    test(Double.NaN, Double.NegativeInfinity, -151.189)

    // If d is PositiveInfinity, return n
    test(+0.0, +0.0, Double.PositiveInfinity)
    test(-0.0, -0.0, Double.PositiveInfinity)
    test(2.1, 2.1, Double.PositiveInfinity)
    test(5.5, 5.5, Double.PositiveInfinity)
    test(-151.189, -151.189, Double.PositiveInfinity)

    // If d is NegativeInfinity, return n
    test(+0.0, +0.0, Double.NegativeInfinity)
    test(-0.0, -0.0, Double.NegativeInfinity)
    test(2.1, 2.1, Double.NegativeInfinity)
    test(5.5, 5.5, Double.NegativeInfinity)
    test(-151.189, -151.189, Double.NegativeInfinity)

    // If d is +0.0, return NaN
    test(Double.NaN, +0.0, +0.0)
    test(Double.NaN, -0.0, +0.0)
    test(Double.NaN, 2.1, +0.0)
    test(Double.NaN, 5.5, +0.0)
    test(Double.NaN, -151.189, +0.0)

    // If d is -0.0, return NaN
    test(Double.NaN, +0.0, -0.0)
    test(Double.NaN, -0.0, -0.0)
    test(Double.NaN, 2.1, -0.0)
    test(Double.NaN, 5.5, -0.0)
    test(Double.NaN, -151.189, -0.0)

    // If n is +0.0, return n
    test(+0.0, +0.0, 2.1)
    test(+0.0, +0.0, 5.5)
    test(+0.0, +0.0, -151.189)

    // If n is -0.0, return n
    test(-0.0, -0.0, 2.1)
    test(-0.0, -0.0, 5.5)
    test(-0.0, -0.0, -151.189)

    // Non-special values
    // { val l = List(2.1, 5.5, -151.189); for (n <- l; d <- l) println(s"    test(${n % d}, $n, $d)") }
    test(0.0, 2.1, 2.1)
    test(2.1, 2.1, 5.5)
    test(2.1, 2.1, -151.189)
    test(1.2999999999999998, 5.5, 2.1)
    test(0.0, 5.5, 5.5)
    test(5.5, 5.5, -151.189)
    test(-2.0889999999999866, -151.189, 2.1)
    test(-2.688999999999993, -151.189, 5.5)
    test(-0.0, -151.189, -151.189)
  }
}
