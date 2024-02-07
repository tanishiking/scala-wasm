package cli

import scala.scalajs.js

import scala.concurrent.ExecutionContext
import wasm.Compiler

object Main {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  def main(args: Array[String]): Unit = {
    val modeEnvVar = js.Dynamic.global.process.env.SCALAJS_MODE

    val cpEnvVar = js.Dynamic.global.process.env.SCALAJS_CLASSPATH
    val classpath = (cpEnvVar: Any) match {
      case cpEnvVar: String if cpEnvVar != "" =>
        cpEnvVar.split(';').toList
      case _ =>
        throw new IllegalArgumentException("The classpath was not provided.")
    }
    println(classpath)

    val result = for {
      irFiles <- new CliReader(classpath).irFiles
      _ <- Compiler.compileIRFiles(irFiles)
    } yield {
      println("Module successfully initialized")
      ()
    }
    result.recover { case th: Throwable =>
      System.err.println("Module initialization failed:")
      th.printStackTrace()
      js.Dynamic.global.process.exit(1)
    }

  }
}
