package wasm

import wasm.wasm4s._
import wasm.ir2wasm.TypeTransformer
import wasm.ir2wasm.WasmBuilder
import wasm.wasm4s.WasmInstr._
import wasm.utils.TestIRBuilder._

import org.scalajs.ir
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.frontend.LinkerFrontendImpl
import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.standard.{LinkedClass, SymbolRequirement}

import org.scalajs.logging.{Level, ScalaConsoleLogger}

import scala.concurrent.{ExecutionContext, Future}
import _root_.ir2wasm.Preprocessor

object Compiler {
  def compileIRFiles(irFiles: Seq[IRFile])(implicit ec: ExecutionContext): Future[Unit] = {
    val module = new WasmModule
    val builder = new WasmBuilder()
    implicit val context: WasmContext = new WasmContext(module)
    println("compiling")

    val config = LinkerFrontendImpl.Config()
      .withOptimizer(false)
    val linkerFrontend = LinkerFrontendImpl(config)

    val symbolRequirements = SymbolRequirement.factory("none").none()
    val logger = new ScalaConsoleLogger(Level.Error)

    linkerFrontend.link(irFiles, Nil, symbolRequirements, logger)
      .map { moduleSet =>
        val onlyModule = moduleSet.modules.head

        val filteredClasses = onlyModule.classDefs.filter { c =>
          c.className == ir.Names.ObjectClass || c.className.nameString.startsWith("sample.")
        }

        filteredClasses.sortBy(_.className).foreach(showLinkedClass(_))

        Preprocessor.preprocess(filteredClasses)(context)
        filteredClasses.foreach { clazz =>
          builder.transformClassDef(clazz)
        }
        onlyModule.topLevelExports.foreach { tle =>
          builder.transformTopLevelExport(tle)
        }
        val writer = new converters.WasmTextWriter()
        println(writer.write(module))
      }
  }

  private def showLinkedClass(clazz: LinkedClass): Unit = {
    val writer = new java.io.PrintWriter(System.out)
    val printer = new LinkedClassPrinter(writer)
    printer.print(clazz)
    printer.println()
    writer.flush()
  }

  private class LinkedClassPrinter(_out: java.io.Writer) extends ir.Printers.IRTreePrinter(_out) {
    def print(clazz: LinkedClass): Unit = {
      print("linked ")
      print(clazz.kind.toString())
      print(" ")
      print(clazz.className)
      clazz.superClass.foreach { cls =>
        print(" extends ")
        print(cls)
        clazz.jsSuperClass.foreach { tree =>
          print(" (via ")
          print(tree)
          print(")")
        }
      }
      if (clazz.interfaces.nonEmpty) {
        print(" implements ")
        var rest = clazz.interfaces
        while (rest.nonEmpty) {
          print(rest.head)
          rest = rest.tail
          if (rest.nonEmpty)
            print(", ")
        }
      }
      clazz.jsNativeLoadSpec.foreach { spec =>
        print(" loadfrom ")
        print(spec)
      }
      print(" ")
      printColumn(
        clazz.fields
          ::: clazz.methods
          ::: clazz.jsConstructorDef.toList
          ::: clazz.exportedMembers
          ::: clazz.jsNativeMembers,
        "{", "", "}"
      )
    }
  }
}
