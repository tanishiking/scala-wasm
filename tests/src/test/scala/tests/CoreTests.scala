package tests

import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

/** The `tests/test` command compiles the Scala sources under `test-suite` into Wasm with static
  * initializers (see `cli/src/main/scala/Main.scala`), where these static initializers should point
  * to each main method in the classes of test-suites.
  *
  * These static initializers will be invoked from the start function in Wasm, which in turn will be
  * invoked on instantiation of the Wasm module.
  *
  * Once we compile the test-suites into Wasm and put them under the `./target` directory,
  * `tests/test` will instantiate those Wasm modules. If the test suites have an assertion failure,
  * the Wasm module will execute `unreachable` and fail during instantiation.
  */
class CoreTests extends munit.FunSuite {
  cli.TestSuites.suites.map { suite =>
    test(suite.className) {
      CoreTests.load(s"./target/${suite.className}/main.wasm").toFuture.map { _ =>
        ()
      }
    }
  }

}

object CoreTests {
  @js.native @JSImport("../../../../loader.mjs")
  def load(wasmFile: String): js.Promise[js.Dynamic] = js.native
}
