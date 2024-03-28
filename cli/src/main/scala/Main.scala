package cli

import scala.scalajs.js

import wasm.Compiler

import org.scalajs.linker.NodeOutputDirectory
import org.scalajs.linker.interface.ModuleInitializer
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

    val mode = (modeEnvVar: Any) match {
      case modeEnvVar if modeEnvVar == "testsuite" => "testsuite"
      case _                                       => "compile"
    }

    val output = NodeOutputDirectory("./target/")

    val result =
      if (mode == "testsuite") {
        for {
          irFiles <- new CliReader(classpath).irFiles
          _ <- Future.sequence {
            TestSuites.suites.map { case TestSuites.TestSuite(className, methodName) =>
              val moduleInitializer = ModuleInitializer.mainMethod(className, methodName)
              Compiler.compileIRFiles(
                irFiles,
                List(moduleInitializer),
                output,
                className
              )
            }
          }
        } yield {
          println("Module successfully initialized")
          ()
        }
      } else {
        for {
          irFiles <- new CliReader(classpath).irFiles
          _ <- Compiler.compileIRFiles(irFiles, Nil, output, "output")
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
}
