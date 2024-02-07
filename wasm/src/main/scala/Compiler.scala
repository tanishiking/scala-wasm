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

object Compiler {
  def compileIRFiles(irFiles: Seq[IRFile])(implicit ec: ExecutionContext): Future[Unit] = {
    // IRFileImpl.fromIRFile()
    val builder = new WasmBuilder()
    implicit val context: WasmContext = WasmContext()
    println("compiling")
    Future
      .traverse(irFiles)(i => IRFileImpl.fromIRFile(i).tree)
      .map { classDefs =>
        classDefs.foreach { clazz =>
          builder.transformClassDef(clazz)
        }
        val writer = new converters.WasmTextWriter()
        println(writer.write(builder.module))
      }
  }
}
