package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.OriginalName
import org.scalajs.ir.Position

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

import org.scalajs.linker.backend.webassembly.FunctionBuilder
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Modules => wamod}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

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
    /* Sort by ancestor count so that superclasses always appear before
     * subclasses, then tie-break by name for stability.
     */
    val sortedClasses = module.classDefs.sortWith { (a, b) =>
      val cmp = Integer.compare(a.ancestors.size, b.ancestors.size)
      if (cmp != 0) cmp < 0
      else a.className.compareTo(b.className) < 0
    }

    implicit val ctx: WasmContext =
      Preprocessor.preprocess(sortedClasses, module.topLevelExports)

    // Sort for stability
    val allImportedModules: List[String] = module.externalDependencies.toList.sorted

    // Gen imports of external modules on the Wasm side
    for (moduleName <- allImportedModules) {
      val id = genGlobalID.forImportedModule(moduleName)
      val origName = OriginalName("import." + moduleName)
      ctx.moduleBuilder.addImport(
        wamod.Import(
          "__scalaJSImports",
          moduleName,
          wamod.ImportDesc.Global(id, origName, watpe.RefType.anyref, isMutable = false)
        )
      )
    }

    CoreWasmLib.genPreClasses()
    sortedClasses.foreach { clazz =>
      classEmitter.genClassDef(clazz)
    }
    module.topLevelExports.foreach { tle =>
      classEmitter.genTopLevelExport(tle)
    }
    CoreWasmLib.genPostClasses()

    complete(
      sortedClasses,
      module.initializers.toList,
      module.topLevelExports
    )

    val wasmModule = ctx.moduleBuilder.build()

    val loaderContent = LoaderContent.bytesContent
    val jsFileContent =
      buildJSFileContent(module, module.id.id + ".wasm", allImportedModules)

    new Result(wasmModule, loaderContent, jsFileContent)
  }

  private def complete(
      sortedClasses: List[LinkedClass],
      moduleInitializers: List[ModuleInitializer.Initializer],
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
    ctx.moduleBuilder.addData(
      wamod.Data(
        genDataID.string,
        OriginalName("stringPool"),
        stringPool,
        wamod.Data.Mode.Passive
      )
    )
    ctx.addGlobal(
      wamod.Global(
        genGlobalID.stringLiteralCache,
        OriginalName("stringLiteralCache"),
        watpe.RefType(genTypeID.anyArray),
        wa.Expr(
          List(
            wa.I32Const(stringPoolCount),
            wa.ArrayNewDefault(genTypeID.anyArray)
          )
        ),
        isMutable = false
      )
    )

    genStartFunction(sortedClasses, moduleInitializers, topLevelExportDefs)
    genDeclarativeElements()
  }

  private def genStartFunction(
      sortedClasses: List[LinkedClass],
      moduleInitializers: List[ModuleInitializer.Initializer],
      topLevelExportDefs: List[LinkedTopLevelExport]
  )(implicit ctx: WasmContext): Unit = {
    import org.scalajs.ir.Trees._

    implicit val pos = Position.NoPosition

    val fb =
      new FunctionBuilder(ctx.moduleBuilder, genFunctionID.start, OriginalName("start"), pos)
    val instrs: fb.type = fb

    // Initialize itables
    for (clazz <- sortedClasses if clazz.kind.isClass && clazz.hasDirectInstances) {
      val className = clazz.className
      val classInfo = ctx.getClassInfo(className)

      if (classInfo.classImplementsAnyInterface) {
        val interfaces = clazz.ancestors.map(ctx.getClassInfo(_)).filter(_.isInterface)
        val resolvedMethodInfos = classInfo.resolvedMethodInfos

        interfaces.foreach { iface =>
          val idx = ctx.getItableIdx(iface)
          instrs += wa.GlobalGet(genGlobalID.forITable(className))
          instrs += wa.I32Const(idx)

          for (method <- iface.tableEntries)
            instrs += ctx.refFuncWithDeclaration(resolvedMethodInfos(method).tableEntryName)
          instrs += wa.StructNew(genTypeID.forITable(iface.name))
          instrs += wa.ArraySet(genTypeID.itables)
        }
      }
    }

    locally {
      // For array classes, resolve methods in jl.Object
      val globalName = genGlobalID.arrayClassITable
      val resolvedMethodInfos = ctx.getClassInfo(ObjectClass).resolvedMethodInfos

      for {
        interfaceName <- List(SerializableClass, CloneableClass)
        // Use getClassInfoOption in case the reachability analysis got rid of those interfaces
        interfaceInfo <- ctx.getClassInfoOption(interfaceName)
      } {
        instrs += wa.GlobalGet(globalName)
        instrs += wa.I32Const(ctx.getItableIdx(interfaceInfo))

        for (method <- interfaceInfo.tableEntries)
          instrs += ctx.refFuncWithDeclaration(resolvedMethodInfos(method).tableEntryName)
        instrs += wa.StructNew(genTypeID.forITable(interfaceName))
        instrs += wa.ArraySet(genTypeID.itables)
      }
    }

    // Initialize the JS private field symbols

    for (fieldName <- ctx.getAllJSPrivateFieldNames()) {
      instrs += wa.Call(genFunctionID.newSymbol)
      instrs += wa.GlobalSet(genGlobalID.forJSPrivateField(fieldName))
    }

    // Emit the static initializers

    for (clazz <- sortedClasses if clazz.hasStaticInitializer) {
      val funcName = genFunctionID.forMethod(
        MemberNamespace.StaticConstructor,
        clazz.className,
        StaticInitializerName
      )
      instrs += wa.Call(funcName)
    }

    // Initialize the top-level exports that require it

    for (tle <- topLevelExportDefs) {
      // Load the (initial) exported value on the stack
      tle.tree match {
        case TopLevelJSClassExportDef(_, exportName) =>
          instrs += wa.Call(genFunctionID.loadJSClass(tle.owningClass))
        case TopLevelModuleExportDef(_, exportName) =>
          instrs += wa.Call(genFunctionID.loadModule(tle.owningClass))
        case TopLevelMethodExportDef(_, methodDef) =>
          instrs += ctx.refFuncWithDeclaration(genFunctionID.forExport(tle.exportName))
          if (methodDef.restParam.isDefined) {
            instrs += wa.I32Const(methodDef.args.size)
            instrs += wa.Call(genFunctionID.makeExportedDefRest)
          } else {
            instrs += wa.Call(genFunctionID.makeExportedDef)
          }
        case TopLevelFieldExportDef(_, _, fieldIdent) =>
          /* Usually redundant, but necessary if the static field is never
           * explicitly set and keeps its default (zero) value instead. In that
           * case this initial call is required to publish that zero value (as
           * opposed to the default `undefined` value of the JS `let`).
           */
          instrs += wa.GlobalGet(genGlobalID.forStaticField(fieldIdent.name))
      }

      // Call the export setter
      instrs += wa.Call(genFunctionID.forTopLevelExportSetter(tle.exportName))
    }

    // Emit the module initializers

    moduleInitializers.foreach { init =>
      def genCallStatic(className: ClassName, methodName: MethodName): Unit = {
        val functionName =
          genFunctionID.forMethod(MemberNamespace.PublicStatic, className, methodName)
        instrs += wa.Call(functionName)
      }

      ModuleInitializerImpl.fromInitializer(init) match {
        case ModuleInitializerImpl.MainMethodWithArgs(className, encodedMainMethodName, args) =>
          // vtable of Array[String]
          instrs += wa.GlobalGet(genGlobalID.forVTable(BoxedStringClass))
          instrs += wa.I32Const(1)
          instrs += wa.Call(genFunctionID.arrayTypeData)

          // itable of Array[String]
          instrs += wa.GlobalGet(genGlobalID.arrayClassITable)

          // underlying array of args
          args.foreach(arg => instrs ++= ctx.getConstantStringInstr(arg))
          instrs += wa.ArrayNewFixed(genTypeID.anyArray, args.size)

          // array object
          val stringArrayTypeRef = ArrayTypeRef(ClassRef(BoxedStringClass), 1)
          instrs += wa.StructNew(genTypeID.forArrayClass(stringArrayTypeRef))

          // call
          genCallStatic(className, encodedMainMethodName)

        case ModuleInitializerImpl.VoidMainMethod(className, encodedMainMethodName) =>
          genCallStatic(className, encodedMainMethodName)
      }
    }

    // Finish the start function

    fb.buildAndAddToModule()
    ctx.moduleBuilder.setStart(genFunctionID.start)
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
        wa.Expr(List(wa.RefFunc(name)))
      }
      ctx.moduleBuilder.addElement(
        wamod.Element(watpe.RefType.funcref, exprs, wamod.Element.Mode.Declarative)
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

    val (exportDecls, exportSetters) = (for {
      exportName <- module.topLevelExports.map(_.exportName)
    } yield {
      val identName = s"exported$exportName"
      val decl = s"let $identName;\nexport { $identName as $exportName };"
      val setter = s"  $exportName: (x) => $identName = x,"
      (decl, setter)
    }).unzip

    s"""
      |${moduleImports.mkString("\n")}
      |
      |import { load as __load } from './${config.loaderModuleName}';
      |
      |${exportDecls.mkString("\n")}
      |
      |await __load('./${wasmFileName}', {
      |${importedModulesItems.mkString("\n")}
      |}, {
      |${exportSetters.mkString("\n")}
      |});
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
      val wasmModule: wamod.Module,
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
