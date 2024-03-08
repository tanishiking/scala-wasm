package tests

import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

class CoreTests extends munit.FunSuite {
  cli.TestSuites.suites.map { suite =>
    test(suite.className) {
      CoreTests.load(s"./target/${suite.className}.wasm").toFuture.map { exports =>
        val testFunction =
          exports
            .selectDynamic(suite.methodName)
            .asInstanceOf[js.Function0[Int]]
        assert(testFunction() == 1)
      }
    }
  }

}

object CoreTests {
  @js.native @JSImport("../../../../loader.mjs")
  def load(wasmFile: String): js.Promise[js.Dynamic] = js.native
}
