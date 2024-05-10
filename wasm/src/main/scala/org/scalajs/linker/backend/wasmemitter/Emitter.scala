package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names._
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.Position

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.Types._

import org.scalajs.logging.Logger

import SpecialNames._
import VarGen._

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

    complete(
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

  private def complete(
      moduleInitializers: List[ModuleInitializer.Initializer],
      classesWithStaticInit: List[ClassName],
      topLevelExportDefs: List[LinkedTopLevelExport]
  )(implicit ctx: WasmContext): Unit = {
    /* Before generating the string globals in `genStartFunction()`, make sure
     * to allocate the ones that will be required by the module initializers.
     */
    for (init <- moduleInitializers) {
      ModuleInitializerImpl.fromInitializer(init) match {
        case ModuleInitializerImpl.MainMethodWithArgs(_, _, args) =>
          args.foreach(ctx.addConstantStringGlobal(_))
        case ModuleInitializerImpl.VoidMainMethod(_, _) =>
          () // nothing to do
      }
    }

    // string
    val (stringPool, stringPoolCount) = ctx.getFinalStringPool()
    ctx.moduleBuilder.addData(WasmData(genDataName.string, stringPool, WasmData.Mode.Passive))
    ctx.addGlobal(
      WasmGlobal(
        genGlobalName.stringLiteralCache,
        WasmRefType(genTypeName.anyArray),
        WasmExpr(
          List(
            WasmInstr.I32_CONST(stringPoolCount),
            WasmInstr.ARRAY_NEW_DEFAULT(genTypeName.anyArray)
          )
        ),
        isMutable = false
      )
    )

    genStartFunction(moduleInitializers, classesWithStaticInit, topLevelExportDefs)
    genDeclarativeElements()
  }

  private def genStartFunction(
      moduleInitializers: List[ModuleInitializer.Initializer],
      classesWithStaticInit: List[ClassName],
      topLevelExportDefs: List[LinkedTopLevelExport]
  )(implicit ctx: WasmContext): Unit = {
    import WasmInstr._

    implicit val pos = Position.NoPosition

    implicit val fctx = WasmFunctionContext(genFunctionName.start, Nil, Nil)

    import fctx.instrs

    // Initialize itables
    for (className <- ctx.getAllClassesWithITableGlobal()) {
      val classInfo = ctx.getClassInfo(className)
      val interfaces = classInfo.ancestors.map(ctx.getClassInfo(_)).filter(_.isInterface)
      val resolvedMethodInfos = classInfo.resolvedMethodInfos

      interfaces.foreach { iface =>
        val idx = ctx.getItableIdx(iface)
        instrs += WasmInstr.GLOBAL_GET(genGlobalName.forITable(className))
        instrs += WasmInstr.I32_CONST(idx)

        for (method <- iface.tableEntries)
          instrs += ctx.refFuncWithDeclaration(resolvedMethodInfos(method).tableEntryName)
        instrs += WasmInstr.STRUCT_NEW(genTypeName.forITable(iface.name))
        instrs += WasmInstr.ARRAY_SET(genTypeName.itables)
      }
    }

    locally {
      // For array classes, resolve methods in jl.Object
      val globalName = genGlobalName.arrayClassITable
      val resolvedMethodInfos = ctx.getClassInfo(ObjectClass).resolvedMethodInfos

      for {
        interfaceName <- List(SerializableClass, CloneableClass)
        // Use getClassInfoOption in case the reachability analysis got rid of those interfaces
        interfaceInfo <- ctx.getClassInfoOption(interfaceName)
      } {
        instrs += GLOBAL_GET(globalName)
        instrs += I32_CONST(ctx.getItableIdx(interfaceInfo))

        for (method <- interfaceInfo.tableEntries)
          instrs += ctx.refFuncWithDeclaration(resolvedMethodInfos(method).tableEntryName)
        instrs += STRUCT_NEW(genTypeName.forITable(interfaceName))
        instrs += ARRAY_SET(genTypeName.itables)
      }
    }

    // Initialize the JS private field symbols

    for (fieldName <- ctx.getAllJSPrivateFieldNames()) {
      instrs += WasmInstr.CALL(genFunctionName.newSymbol)
      instrs += WasmInstr.GLOBAL_SET(genGlobalName.forJSPrivateField(fieldName))
    }

    // Emit the static initializers

    for (className <- classesWithStaticInit) {
      val funcName = genFunctionName.forMethod(
        IRTrees.MemberNamespace.StaticConstructor,
        className,
        StaticInitializerName
      )
      instrs += WasmInstr.CALL(funcName)
    }

    // Initialize the top-level exports that require it

    for (tle <- topLevelExportDefs) {
      tle.tree match {
        case IRTrees.TopLevelJSClassExportDef(_, exportName) =>
          instrs += CALL(genFunctionName.loadJSClass(tle.owningClass))
          instrs += GLOBAL_SET(genGlobalName.forTopLevelExport(tle.exportName))
        case IRTrees.TopLevelModuleExportDef(_, exportName) =>
          instrs += CALL(genFunctionName.loadModule(tle.owningClass))
          instrs += GLOBAL_SET(genGlobalName.forTopLevelExport(tle.exportName))
        case IRTrees.TopLevelMethodExportDef(_, methodDef) =>
          // We only need initialization if there is a restParam
          if (methodDef.restParam.isDefined) {
            instrs += ctx.refFuncWithDeclaration(genFunctionName.forExport(tle.exportName))
            instrs += I32_CONST(methodDef.args.size)
            instrs += CALL(genFunctionName.closureRestNoData)
            instrs += GLOBAL_SET(genGlobalName.forTopLevelExport(tle.exportName))
          }
        case IRTrees.TopLevelFieldExportDef(_, _, _) =>
          // Nothing to do
          ()
      }
    }

    // Emit the module initializers

    moduleInitializers.foreach { init =>
      def genCallStatic(className: ClassName, methodName: MethodName): Unit = {
        val functionName =
          genFunctionName.forMethod(IRTrees.MemberNamespace.PublicStatic, className, methodName)
        instrs += WasmInstr.CALL(functionName)
      }

      ModuleInitializerImpl.fromInitializer(init) match {
        case ModuleInitializerImpl.MainMethodWithArgs(className, encodedMainMethodName, args) =>
          // vtable of Array[String]
          instrs += GLOBAL_GET(genGlobalName.forVTable(BoxedStringClass))
          instrs += I32_CONST(1)
          instrs += CALL(genFunctionName.arrayTypeData)

          // itable of Array[String]
          instrs += GLOBAL_GET(genGlobalName.arrayClassITable)

          // underlying array of args
          args.foreach(arg => instrs ++= ctx.getConstantStringInstr(arg))
          instrs += ARRAY_NEW_FIXED(genTypeName.anyArray, args.size)

          // array object
          val stringArrayTypeRef = IRTypes.ArrayTypeRef(IRTypes.ClassRef(BoxedStringClass), 1)
          instrs += STRUCT_NEW(genTypeName.forArrayClass(stringArrayTypeRef))

          // call
          genCallStatic(className, encodedMainMethodName)

        case ModuleInitializerImpl.VoidMainMethod(className, encodedMainMethodName) =>
          genCallStatic(className, encodedMainMethodName)
      }
    }

    // Finish the start function

    fctx.buildAndAddToContext()
    ctx.moduleBuilder.setStart(genFunctionName.start)
  }

  private def genDeclarativeElements()(implicit ctx: WasmContext): Unit = {
    // Aggregated Elements

    val funcDeclarations = ctx.getAllFuncDeclarations()

    if (funcDeclarations.nonEmpty) {
      /* Functions that are referred to with `ref.func` in the Code section
       * must be declared ahead of time in one of the earlier sections
       * (otherwise the module does not validate). It can be the Global section
       * if they are meaningful there (which is why `ref.func` in the vtables
       * work out of the box). In the absence of any other specific place, an
       * Element section with the declarative mode is the recommended way to
       * introduce these declarations.
       */
      val exprs = funcDeclarations.map { name =>
        WasmExpr(List(WasmInstr.REF_FUNC(name)))
      }
      ctx.moduleBuilder.addElement(
        WasmElement(WasmRefType.funcref, exprs, WasmElement.Mode.Declarative)
      )
    }
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
