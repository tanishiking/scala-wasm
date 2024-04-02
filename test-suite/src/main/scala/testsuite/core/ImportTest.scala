package testsuite.core

import scala.scalajs.js
import scala.scalajs.js.annotation._

import testsuite.Assert._

object ImportTest {
  @js.native
  @JSImport("node:querystring")
  def escape(str: String): String = js.native

  def main(): Unit = {
    assertSame("foo%26ba%2Bbar%3Dfoobabar", escape("foo&ba+bar=foobabar"))
  }
}
