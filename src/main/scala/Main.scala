import ir2wasm.TypeTransformer
import wasm4s._

import WasmInstr._
import utils.TestIRBuilder._
import org.scalajs.ir.Trees._
import org.scalajs.ir
import org.scalajs.ir.Types._
import ir2wasm.WasmBuilder

object Main {
  def main(args: Array[String]): Unit = {
    val basicTestClassDefs = List(
      mainTestClassDef({
        BinaryOp(BinaryOp.Int_+, IntLiteral(1), IntLiteral(1))
      })
    )

    val builder = new WasmBuilder()
    implicit val context: WasmContext = WasmContext()
    basicTestClassDefs.foreach { clazz =>
      clazz.methods.foreach { method =>
        builder.addFunction(method)
      }
    }
    val writer = new converters.WasmTextWriter()
    println(writer.write(builder.module))

  }
}
