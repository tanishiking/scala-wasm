package wasm

import wasm.ir2wasm._
import wasm.wasm4s._

import org.scalajs.ir
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.frontend.LinkerFrontendImpl
import org.scalajs.linker.interface.{IRFile, ModuleInitializer}
import org.scalajs.linker.standard.{LinkedClass, SymbolRequirement}

import org.scalajs.logging.{Level, ScalaConsoleLogger}

import scala.concurrent.{ExecutionContext, Future}

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.typedarray._

object Compiler {
  def compileIRFiles(
      irFiles: Seq[IRFile],
      moduleInitializers: List[ModuleInitializer],
      outputName: String
  )(implicit ec: ExecutionContext): Future[Unit] = {
    val module = new WasmModule
    val builder = new WasmBuilder()
    implicit val context: WasmContext = new WasmContext(module)
    println("compiling...  ")

    val config = LinkerFrontendImpl
      .Config()
      .withOptimizer(false)
    val linkerFrontend = LinkerFrontendImpl(config)

    /* The symbol requirements of our back-end.
     * The symbol requirements tell the LinkerFrontend that we need these
     * symbols to always be reachable, even if no "user-land" IR requires them.
     * They are roots for the reachability analysis, together with module
     * initializers and top-level exports.
     * If we don't do this, the linker frontend will dead-code eliminate our
     * box classes.
     */
    val factory = SymbolRequirement.factory("wasm")
    val symbolRequirements = factory.multiple(
      factory.instantiateClass(SpecialNames.CharBoxClass, SpecialNames.CharBoxCtor),
      factory.instantiateClass(SpecialNames.LongBoxClass, SpecialNames.LongBoxCtor)
    )

    val logger = new ScalaConsoleLogger(Level.Info)

    for {
      patchedIRFiles <- LibraryPatches.patchIRFiles(irFiles)
      moduleSet <- linkerFrontend.link(
        patchedIRFiles,
        moduleInitializers,
        symbolRequirements,
        logger
      )
    } yield {
      val onlyModule = moduleSet.modules.head

      // Sort for stability
      val sortedClasses = onlyModule.classDefs.sortBy(_.className)

      sortedClasses.foreach(showLinkedClass(_))

      Preprocessor.preprocess(sortedClasses)(context)
      println("preprocessed")
      sortedClasses.foreach { clazz =>
        builder.transformClassDef(clazz)
      }
      onlyModule.topLevelExports.foreach { tle =>
        builder.transformTopLevelExport(tle)
      }

      context.complete()

      val textOutput = new converters.WasmTextWriter().write(module)
      FS.writeFileSync(s"./target/$outputName.wat", textOutput.getBytes().toTypedArray)

      val binaryOutput = new converters.WasmBinaryWriter(module).write()
      FS.writeFileSync(s"./target/$outputName.wasm", binaryOutput.toTypedArray)
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
        "{",
        "",
        "}"
      )
    }
  }

  private object FS {
    @js.native @JSImport("fs")
    def writeFileSync(file: String, data: Int8Array): Unit = js.native
  }
}
