package cli

import scala.scalajs.js
import scala.scalajs.js.annotation._

import wasm.WebAssemblyLinkerImpl

import org.scalajs.linker.NodeOutputDirectory
import org.scalajs.linker.interface._

import org.scalajs.logging._

import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

import scala.concurrent.Future

object Main {
  def main(args: Array[String]): Unit = {
    val modeEnvVar = js.Dynamic.global.process.env.SCALAJS_MODE
    val cpEnvVar = js.Dynamic.global.process.env.SCALAJS_CLASSPATH

    val classpath = (cpEnvVar: Any) match {
      case cpEnvVar: String if cpEnvVar != "" =>
        cpEnvVar.split(';').toList
      case _ =>
        throw new IllegalArgumentException("The classpath was not provided.")
    }

    val linkerConfig = StandardConfig()
      .withESFeatures(_.withESVersion(ESVersion.ES2016)) // to be able to link `**`
      .withSemantics(_.optimized) // because that's the only thing we actually support at the moment
      .withOptimizer(false)

    val logger = new ScalaConsoleLogger(Level.Info)

    val mode = (modeEnvVar: Any) match {
      case modeEnvVar if modeEnvVar == "testsuite" => "testsuite"
      case _                                       => "compile"
    }

    val result =
      if (mode == "testsuite") {
        for {
          irFiles <- new CliReader(classpath).irFiles
          _ <- Future.sequence {
            TestSuites.suites.map { case TestSuites.TestSuite(className, methodName) =>
              val linker = WebAssemblyLinkerImpl.linker(linkerConfig)
              val moduleInitializer = ModuleInitializer.mainMethod(className, methodName)
              val outputDir = s"./target/$className/"
              createDir(outputDir)
              val output = NodeOutputDirectory(outputDir)
              linker.link(
                irFiles,
                List(moduleInitializer),
                output,
                logger
              )
            }
          }
        } yield {
          println("Module successfully initialized")
          ()
        }
      } else {
        val linker = WebAssemblyLinkerImpl.linker(linkerConfig)
        val outputDir = "./target/sample/"
        createDir(outputDir)
        val output = NodeOutputDirectory(outputDir)

        for {
          irFiles <- new CliReader(classpath).irFiles
          _ <- linker.link(irFiles, Nil, output, logger)
        } yield {
          println("Module successfully initialized")
          ()
        }
      }

    result.recover { case th: Throwable =>
      System.err.println("Module initialization failed:")
      th.printStackTrace()
      js.Dynamic.global.process.exit(1)
    }
  }

  def createDir(dir: String): Unit =
    mkdirSync(dir, js.Dynamic.literal(recursive = true))

  @js.native
  @JSImport("node:fs")
  def mkdirSync(path: String, options: js.Object = js.native): Unit = js.native
}
