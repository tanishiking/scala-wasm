package wasm

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import wasm.ir2wasm._
import wasm.wasm4s._

import org.scalajs.ir
import org.scalajs.ir.{Names => IRNames}

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._
import org.scalajs.linker.interface.unstable.OutputDirectoryImpl

import org.scalajs.logging.{Level, ScalaConsoleLogger}

import scala.concurrent.{ExecutionContext, Future}

object Compiler {
  def compileIRFiles(
      irFiles: Seq[IRFile],
      moduleInitializers: List[ModuleInitializer],
      output: OutputDirectory,
      outputName: String
  )(implicit ec: ExecutionContext): Future[Unit] = {
    val module = new WasmModule
    val builder = new WasmBuilder()
    implicit val context: WasmContext = new WasmContext(module)
    println("compiling...  ")

    val config = StandardConfig()
      .withESFeatures(_.withESVersion(ESVersion.ES2016)) // to be able to link `**`
      .withSemantics(_.optimized) // because that's the only thing we actually support at the moment
      .withOptimizer(false)

    val linkerFrontend = StandardLinkerFrontend(config)

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
      factory.instantiateClass(IRNames.ClassClass, SpecialNames.ClassCtor),
      factory.instantiateClass(SpecialNames.CharBoxClass, SpecialNames.CharBoxCtor),
      factory.instantiateClass(SpecialNames.LongBoxClass, SpecialNames.LongBoxCtor)
    )

    val logger = new ScalaConsoleLogger(Level.Info)

    val result = for {
      patchedIRFiles <- LibraryPatches.patchIRFiles(irFiles)
      moduleSet <- linkerFrontend.link(
        patchedIRFiles,
        moduleInitializers,
        symbolRequirements,
        logger
      )
    } yield {
      val onlyModule = moduleSet.modules.head

      /* Sort by ancestor count so that superclasses always appear before
       * subclasses, then tie-break by name for stability.
       */
      val sortedClasses = onlyModule.classDefs.sortWith { (a, b) =>
        val cmp = Integer.compare(a.ancestors.size, b.ancestors.size)
        if (cmp != 0) cmp < 0
        else a.className.compareTo(b.className) < 0
      }

      // sortedClasses.foreach(showLinkedClass(_))

      Preprocessor.preprocess(sortedClasses)(context)
      HelperFunctions.genGlobalHelpers()
      builder.genPrimitiveTypeDataGlobals()
      sortedClasses.foreach { clazz =>
        builder.transformClassDef(clazz)
      }
      // Array classes extend j.l.Object, so they must come after transformClassDef's
      builder.genArrayClasses()
      onlyModule.topLevelExports.foreach { tle =>
        builder.transformTopLevelExport(tle)
      }

      val classesWithStaticInit =
        sortedClasses.filter(_.hasStaticInitializer).map(_.className)

      context.complete(moduleInitializers, classesWithStaticInit)

      val outputImpl = OutputDirectoryImpl.fromOutputDirectory(output)

      val textOutput = new converters.WasmTextWriter().write(module)
      val textOutputBytes = textOutput.getBytes(StandardCharsets.UTF_8)
      val binaryOutput = new converters.WasmBinaryWriter(module).write()

      outputImpl.writeFull(s"$outputName.wat", ByteBuffer.wrap(textOutputBytes)).flatMap { _ =>
        outputImpl.writeFull(s"$outputName.wasm", ByteBuffer.wrap(binaryOutput))
      }
    }

    result.flatten
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
}
