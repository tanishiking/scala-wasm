package tests

import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

class CoreTests extends munit.FunSuite {
  cli.TestSuites.suites.map { suite =>
    test(suite.className) {
      val file = s"./target/${suite.className}.wasm"
      val wasmBuffer = FS.readFileSync(file)
      val wasmModule =
        js.Dynamic.global.WebAssembly.instantiate(wasmBuffer).asInstanceOf[js.Promise[js.Dynamic]]
      wasmModule.toFuture.map { module =>
        val testFunction =
          module.instance.exports
            .selectDynamic(suite.methodName)
            .asInstanceOf[js.Function0[Int]]
        assert(testFunction() == 1)
      }
    }
  }

}

private object FS {
  @js.native @JSImport("fs")
  def readFileSync(file: String): js.typedarray.Uint8Array = js.native
}
