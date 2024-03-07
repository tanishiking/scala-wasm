package cli

import scala.scalajs.js

import wasm.Compiler

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

    val result =
      if (mode == "testsuite") {
        val className = TestSuites.suites.map(_.className)
        val moduleInitializers = className
          .map { clazz =>
            ModuleInitializer.mainMethod(clazz, "main")
          }
          .zip(className)

        for {
          irFiles <- new CliReader(classpath).irFiles
          _ <- Future.sequence {
            moduleInitializers.map { case (moduleInitializer, className) =>
              Compiler.compileIRFiles(
                irFiles,
                List(moduleInitializer),
                s"$className"
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
          _ <- Compiler.compileIRFiles(irFiles, Nil, s"output")
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
