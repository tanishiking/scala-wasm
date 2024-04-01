package testsuite.core

import scala.scalajs.js

import testsuite.Assert._

object JSImportCallTest {
  def main(): Unit = {
    /* We cannot actually wait for the result of an asynchronous computation
     * in our tests, so we only test that we indeed get a Promise.
     */
    val promise = js.`import`[js.Any]("node:fs")
    ok((promise: Any).isInstanceOf[js.Promise[_]])
  }
}
