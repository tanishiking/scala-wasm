package wasm

import scala.concurrent.{ExecutionContext, Future}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import org.scalajs.ir.Names._

import org.scalajs.logging.Logger

import org.scalajs.linker._
import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._
import org.scalajs.linker.standard._

import wasm.ir2wasm._
import wasm.ir2wasm.SpecialNames._
import wasm.wasm4s._

final class WebAssemblyLinkerBackend(
    linkerConfig: StandardConfig,
    val coreSpec: CoreSpec
) extends LinkerBackend {
  require(
    !linkerConfig.optimizer,
    "The WebAssembly backend does not support the optimizer yet."
  )

  /* The symbol requirements of our back-end.
   * The symbol requirements tell the LinkerFrontend that we need these
   * symbols to always be reachable, even if no "user-land" IR requires them.
   * They are roots for the reachability analysis, together with module
   * initializers and top-level exports.
   * If we don't do this, the linker frontend will dead-code eliminate our
   * box classes.
   */
  val symbolRequirements: SymbolRequirement = {
    val factory = SymbolRequirement.factory("wasm")

    factory.multiple(
      factory.instantiateClass(ClassClass, ClassCtor),
      factory.instantiateClass(CharBoxClass, CharBoxCtor),
      factory.instantiateClass(LongBoxClass, LongBoxCtor)
    )
  }

  // Our injected IR files are handled by WebAssemblyStandardLinkerImpl instead
  def injectedIRFiles: Seq[IRFile] = Nil

  def emit(moduleSet: ModuleSet, output: OutputDirectory, logger: Logger)(implicit
      ec: ExecutionContext
  ): Future[Report] = {

    val wasmModule = new WasmModule
    val builder = new WasmBuilder()
    implicit val context: WasmContext = new WasmContext(wasmModule)

    val onlyModule = moduleSet.modules.head
    val moduleID = onlyModule.id.id

    /* Sort by ancestor count so that superclasses always appear before
     * subclasses, then tie-break by name for stability.
     */
    val sortedClasses = onlyModule.classDefs.sortWith { (a, b) =>
      val cmp = Integer.compare(a.ancestors.size, b.ancestors.size)
      if (cmp != 0) cmp < 0
      else a.className.compareTo(b.className) < 0
    }

    // sortedClasses.foreach(cls => println(utils.LinkedClassPrinters.showLinkedClass(cls)))

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

    context.complete(onlyModule.initializers.toList, classesWithStaticInit)

    val outputImpl = OutputDirectoryImpl.fromOutputDirectory(output)

    val textOutput = new converters.WasmTextWriter().write(wasmModule)
    val textOutputBytes = textOutput.getBytes(StandardCharsets.UTF_8)
    val binaryOutput = new converters.WasmBinaryWriter(wasmModule).write()

    val filesToProduce = Set(s"$moduleID.wat", s"$moduleID.wasm")
    for {
      existingFiles <- outputImpl.listFiles()
      _ <- Future.sequence(existingFiles.filterNot(filesToProduce).map(outputImpl.delete(_)))
      _ <- outputImpl.writeFull(s"$moduleID.wat", ByteBuffer.wrap(textOutputBytes))
      _ <- outputImpl.writeFull(s"$moduleID.wasm", ByteBuffer.wrap(binaryOutput))
    } yield {
      val reportModule = new ReportImpl.ModuleImpl(
        moduleID,
        s"$moduleID.wasm",
        None,
        linkerConfig.moduleKind
      )
      new ReportImpl(List(reportModule))
    }
  }
}
