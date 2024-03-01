package wasm
import ir2wasm.TypeTransformer
import ir2wasm.WasmBuilder
import wasm4s._

import WasmInstr._
import utils.TestIRBuilder._
import org.scalajs.ir
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

object Main {
  def main(args: Array[String]): Unit = {
    // def add(a: Int, b: Int): Int = a + b
    val addFunction = {
      val paramA = paramDef("a", IntType)
      val paramB = paramDef("b", IntType)
      MethodDef(
        MemberFlags.empty.withNamespace(MemberNamespace.PublicStatic),
        m("add", List(IntRef, IntRef), IntRef),
        NON,
        List(paramA, paramB),
        IntType,
        Some(
          BinaryOp(BinaryOp.Int_+, paramA.ref, paramB.ref)
        )
      )(EOH, NOV)
    }
    val basicTestClassDefs = List(
      classDef(
        MainTestClassName,
        kind = ir.ClassKind.ModuleClass,
        superClass = None,
        methods = List(addFunction)
      )
    )

    basicTestClassDefs.foreach(c => println(c.show))

    val module = new WasmModule
    val builder = new WasmBuilder()
    implicit val context: WasmContext = new WasmContext(module)
    basicTestClassDefs.foreach { clazz =>
      //builder.transformClassDef(clazz)
      ???
    }
    val writer = new converters.WasmTextWriter()
    println(writer.write(module))
  }
}
