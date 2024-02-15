package wasm

import wasm.wasm4s._
import wasm.ir2wasm.TypeTransformer
import wasm.ir2wasm.WasmBuilder
import wasm.wasm4s.WasmInstr._
import wasm.utils.TestIRBuilder._

import org.scalajs.ir
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.interface.unstable.IRFileImpl

import scala.concurrent.{ExecutionContext, Future}
import _root_.ir2wasm.Preprocessor

object Compiler {
  def compileIRFiles(irFiles: Seq[IRFile])(implicit ec: ExecutionContext): Future[Unit] = {
    // IRFileImpl.fromIRFile()
    val module = new WasmModule
    val builder = new WasmBuilder(module)
    implicit val context: WasmContext = WasmContext(module)
    println("compiling")
    Future
      .traverse(irFiles)(i => IRFileImpl.fromIRFile(i).tree)
      .map { classDefs =>
        classDefs.foreach { clazz =>
          Preprocessor.collectMethods(clazz)(context)
        }
        classDefs.foreach { clazz =>
          println(clazz.show)
          builder.transformClassDef(clazz)
        }
        val writer = new converters.WasmTextWriter()
        println(writer.write(module))
      }
  }
}
