package testsuite.core

import scala.scalajs.js

import testsuite.Assert._

object JSImportMetaTest {
  def main(): Unit = {
    val meta = js.`import`.meta
    assertSame("object", js.typeOf(meta))
    assertSame("string", js.typeOf(meta.url))
  }
}
