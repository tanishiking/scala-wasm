package org.scalajs.linker.backend

import scala.concurrent.{ExecutionContext, Future}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import org.scalajs.ir.Names._

import org.scalajs.logging.Logger

import org.scalajs.linker._
import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._
import org.scalajs.linker.standard._

import org.scalajs.linker.backend.webassembly._

import org.scalajs.linker.backend.wasmemitter._
import org.scalajs.linker.backend.wasmemitter.SpecialNames._

final class WebAssemblyLinkerBackend(
    linkerConfig: StandardConfig,
    val coreSpec: CoreSpec
) extends LinkerBackend {
  require(
    linkerConfig.moduleKind == ModuleKind.ESModule,
    s"The WebAssembly backend only supports ES modules; was ${linkerConfig.moduleKind}."
  )
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
      factory.instantiateClass(LongBoxClass, LongBoxCtor),

      // See genIdentityHashCode in HelperFunctions
      factory.callMethodStatically(BoxedDoubleClass, hashCodeMethodName),
      factory.callMethodStatically(BoxedStringClass, hashCodeMethodName)
    )
  }

  // Our injected IR files are handled by WebAssemblyStandardLinkerImpl instead
  def injectedIRFiles: Seq[IRFile] = Nil

  def emit(moduleSet: ModuleSet, output: OutputDirectory, logger: Logger)(implicit
      ec: ExecutionContext
  ): Future[Report] = {
    implicit val context: WasmContext = new WasmContext()

    val onlyModule = moduleSet.modules match {
      case onlyModule :: Nil =>
        onlyModule
      case modules =>
        throw new UnsupportedOperationException(
          "The WebAssembly backend does not support multiple modules. Found: " +
            modules.map(_.id.id).mkString(", ")
        )
    }
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

    Preprocessor.preprocess(sortedClasses, onlyModule.topLevelExports)(context)

    CoreWasmLib.genPreClasses()
    val classEmitter = new ClassEmitter(coreSpec)
    sortedClasses.foreach { clazz =>
      classEmitter.transformClassDef(clazz)
    }
    onlyModule.topLevelExports.foreach { tle =>
      classEmitter.transformTopLevelExport(tle)
    }
    CoreWasmLib.genPostClasses()

    val classesWithStaticInit =
      sortedClasses.filter(_.hasStaticInitializer).map(_.className)

    context.complete(
      onlyModule.initializers.toList,
      classesWithStaticInit,
      onlyModule.topLevelExports
    )

    val wasmModule = context.moduleBuilder.build()

    val outputImpl = OutputDirectoryImpl.fromOutputDirectory(output)

    val watFileName = s"$moduleID.wat"
    val wasmFileName = s"$moduleID.wasm"
    val sourceMapFileName = s"$wasmFileName.map"
    val jsFileName = OutputPatternsImpl.jsFile(linkerConfig.outputPatterns, moduleID)
    val loaderJSFileName = OutputPatternsImpl.jsFile(linkerConfig.outputPatterns, "__loader")

    val filesToProduce0 = Set(
      wasmFileName,
      loaderJSFileName,
      jsFileName
    )
    val filesToProduce1 =
      if (linkerConfig.sourceMap) filesToProduce0 + sourceMapFileName
      else filesToProduce0
    val filesToProduce =
      if (linkerConfig.prettyPrint) filesToProduce1 + watFileName
      else filesToProduce1

    def maybeWriteWatFile(): Future[Unit] = {
      if (linkerConfig.prettyPrint) {
        val textOutput = new WasmTextWriter().write(wasmModule)
        val textOutputBytes = textOutput.getBytes(StandardCharsets.UTF_8)
        outputImpl.writeFull(watFileName, ByteBuffer.wrap(textOutputBytes))
      } else {
        Future.unit
      }
    }

    def writeWasmFile(): Future[Unit] = {
      val emitDebugInfo = !linkerConfig.minify

      if (linkerConfig.sourceMap) {
        val sourceMapWriter = new SourceMapWriterAccess.ByteArrayWriterBox()

        val wasmFileURI = s"./$wasmFileName"
        val sourceMapURI = s"./$sourceMapFileName"

        val smWriter =
          sourceMapWriter.createSourceMapWriter(wasmFileURI, linkerConfig.relativizeSourceMapBase)
        val binaryOutput = new WasmBinaryWriter.WithSourceMap(
          wasmModule,
          emitDebugInfo,
          smWriter,
          sourceMapURI
        ).write()
        smWriter.complete()

        outputImpl.writeFull(wasmFileName, ByteBuffer.wrap(binaryOutput)).flatMap { _ =>
          outputImpl.writeFull(sourceMapFileName, sourceMapWriter.toByteBuffer())
        }
      } else {
        val binaryOutput = new WasmBinaryWriter(wasmModule, emitDebugInfo).write()
        outputImpl.writeFull(wasmFileName, ByteBuffer.wrap(binaryOutput))
      }
    }

    def writeLoaderFile(): Future[Unit] =
      outputImpl.writeFull(loaderJSFileName, ByteBuffer.wrap(LoaderContent.bytesContent))

    def writeJSFile(): Future[Unit] = {
      val jsFileOutput =
        buildJSFileOutput(onlyModule, loaderJSFileName, wasmFileName, context.allImportedModules)
      val jsFileOutputBytes = jsFileOutput.getBytes(StandardCharsets.UTF_8)
      outputImpl.writeFull(jsFileName, ByteBuffer.wrap(jsFileOutputBytes))
    }

    for {
      existingFiles <- outputImpl.listFiles()
      _ <- Future.sequence(existingFiles.filterNot(filesToProduce).map(outputImpl.delete(_)))
      _ <- maybeWriteWatFile()
      _ <- writeWasmFile()
      _ <- writeLoaderFile()
      _ <- writeJSFile()
    } yield {
      val reportModule = new ReportImpl.ModuleImpl(
        moduleID,
        jsFileName,
        None,
        linkerConfig.moduleKind
      )
      new ReportImpl(List(reportModule))
    }
  }

  private def buildJSFileOutput(
      module: ModuleSet.Module,
      loaderJSFileName: String,
      wasmFileName: String,
      importedModules: List[String]
  ): String = {
    val (moduleImports, importedModulesItems) = (for {
      (moduleName, idx) <- importedModules.zipWithIndex
    } yield {
      val identName = s"imported$idx"
      val escapedModuleName = "\"" + moduleName + "\""
      val moduleImport = s"import * as $identName from $escapedModuleName"
      val item = s"  $escapedModuleName: $identName,"
      (moduleImport, item)
    }).unzip

    /* TODO This is not correct for exported *vars*, since they won't receive
     * updates from mutations after loading.
     */
    val reExportStats = for {
      exportName <- module.topLevelExports.map(_.exportName)
    } yield {
      s"export let $exportName = __exports.$exportName;"
    }

    s"""
      |${moduleImports.mkString("\n")}
      |
      |import { load as __load } from './${loaderJSFileName}';
      |const __exports = await __load('./${wasmFileName}', {
      |${importedModulesItems.mkString("\n")}
      |});
      |
      |${reExportStats.mkString("\n")}
    """.stripMargin.trim() + "\n"
  }
}
