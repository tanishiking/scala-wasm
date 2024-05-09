package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

import org.scalajs.linker.backend.webassembly._

import org.scalajs.logging.Logger

import SpecialNames._

final class Emitter(config: Emitter.Config) {
  import Emitter._

  private val classEmitter = new ClassEmitter(config.coreSpec)

  val symbolRequirements: SymbolRequirement =
    Emitter.symbolRequirements(config.coreSpec)

  // Our injected IR files are handled by WebAssemblyStandardLinkerImpl instead
  def injectedIRFiles: Seq[IRFile] = Nil

  def emit(module: ModuleSet.Module, logger: Logger): Result = {
    implicit val ctx: WasmContext = new WasmContext()

    /* Sort by ancestor count so that superclasses always appear before
     * subclasses, then tie-break by name for stability.
     */
    val sortedClasses = module.classDefs.sortWith { (a, b) =>
      val cmp = Integer.compare(a.ancestors.size, b.ancestors.size)
      if (cmp != 0) cmp < 0
      else a.className.compareTo(b.className) < 0
    }

    Preprocessor.preprocess(sortedClasses, module.topLevelExports)

    CoreWasmLib.genPreClasses()
    sortedClasses.foreach { clazz =>
      classEmitter.transformClassDef(clazz)
    }
    module.topLevelExports.foreach { tle =>
      classEmitter.transformTopLevelExport(tle)
    }
    CoreWasmLib.genPostClasses()

    val classesWithStaticInit =
      sortedClasses.filter(_.hasStaticInitializer).map(_.className)

    ctx.complete(
      module.initializers.toList,
      classesWithStaticInit,
      module.topLevelExports
    )

    val wasmModule = ctx.moduleBuilder.build()

    val loaderContent = LoaderContent.bytesContent
    val jsFileContent =
      buildJSFileContent(module, module.id.id + ".wasm", ctx.allImportedModules)

    new Result(wasmModule, loaderContent, jsFileContent)
  }

  private def buildJSFileContent(
      module: ModuleSet.Module,
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
      |import { load as __load } from './${config.loaderModuleName}';
      |const __exports = await __load('./${wasmFileName}', {
      |${importedModulesItems.mkString("\n")}
      |});
      |
      |${reExportStats.mkString("\n")}
    """.stripMargin.trim() + "\n"
  }
}

object Emitter {

  /** Configuration for the Emitter. */
  final class Config private (
      val coreSpec: CoreSpec,
      val loaderModuleName: String
  )

  object Config {
    def apply(coreSpec: CoreSpec, loaderModuleName: String): Config =
      new Config(coreSpec, loaderModuleName)
  }

  final class Result(
      val wasmModule: WasmModule,
      val loaderContent: Array[Byte],
      val jsFileContent: String
  )

  /** Builds the symbol requirements of our back-end.
    *
    * The symbol requirements tell the LinkerFrontend that we need these symbols to always be
    * reachable, even if no "user-land" IR requires them. They are roots for the reachability
    * analysis, together with module initializers and top-level exports. If we don't do this, the
    * linker frontend will dead-code eliminate our box classes.
    */
  private def symbolRequirements(coreSpec: CoreSpec): SymbolRequirement = {
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

}
