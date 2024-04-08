package tests

import scala.concurrent.Future

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

import wasm.WebAssemblyLinkerImpl

import org.scalajs.linker._
import org.scalajs.linker.interface._

import org.scalajs.logging._

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
  import CoreTests._

  private val classpath = {
    val cpEnvVar = js.Dynamic.global.process.env.SCALAJS_CLASSPATH

    (cpEnvVar: Any) match {
      case cpEnvVar: String if cpEnvVar != "" =>
        cpEnvVar.split(';').toList
      case _ =>
        throw new IllegalArgumentException("The classpath was not provided.")
    }
  }

  private val globalIRCache = StandardImpl.irFileCache()

  private val linkerConfig = StandardConfig()
    .withESFeatures(_.withESVersion(ESVersion.ES2016)) // to be able to link `**`
    .withSemantics(_.optimized) // because that's the only thing we actually support at the moment
    .withModuleKind(ModuleKind.ESModule)
    .withOptimizer(false)
    .withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))

  TestSuites.suites.foreach { suite =>
    test(suite.className) {
      testOneSuite(suite)
    }
  }

  private def testOneSuite(suite: TestSuites.TestSuite): Future[Unit] = {
    val className = suite.className
    val methodName = suite.methodName

    val cache = globalIRCache.newCache
    val irFilesFuture = NodeIRContainer.fromClasspath(classpath).map(_._1).flatMap(cache.cached _)

    val logger = new ScalaConsoleLogger(Level.Error)

    val linker = WebAssemblyLinkerImpl.linker(linkerConfig)
    val moduleInitializers = List(ModuleInitializer.mainMethod(className, methodName))

    val outputDirRelToPwd = s"./target/$className/"
    createDir(outputDirRelToPwd)
    val output = NodeOutputDirectory(outputDirRelToPwd)

    val outputDirRelToMyJSFile = s"../../../../target/$className/"

    for {
      irFiles <- irFilesFuture
      linkerResult <- linker.link(irFiles, moduleInitializers, output, logger)
      runResult <- js.`import`[js.Any](s"$outputDirRelToMyJSFile/main.mjs").toFuture
    } yield {
      ()
    }
  }
}

object CoreTests {
  def createDir(dir: String): Unit =
    mkdirSync(dir, js.Dynamic.literal(recursive = true))

  @js.native
  @JSImport("node:fs")
  def mkdirSync(path: String, options: js.Object = js.native): Unit = js.native
}
