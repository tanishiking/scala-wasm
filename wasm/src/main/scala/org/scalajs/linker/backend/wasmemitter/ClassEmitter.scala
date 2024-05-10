package org.scalajs.linker.backend.wasmemitter

import scala.collection.mutable

import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{ClassKind, Position}

import org.scalajs.linker.interface.unstable.RuntimeClassNameMapperImpl
import org.scalajs.linker.standard.{CoreSpec, LinkedClass, LinkedTopLevelExport}

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.Names._
import org.scalajs.linker.backend.webassembly.Names.WasmTypeName._
import org.scalajs.linker.backend.webassembly.Types._
import org.scalajs.linker.backend.webassembly.WasmInstr._

import EmbeddedConstants._
import SWasmGen._
import VarGen._
import TypeTransformer._
import WasmContext._

class ClassEmitter(coreSpec: CoreSpec) {
  def transformClassDef(clazz: LinkedClass)(implicit ctx: WasmContext) = {
    val classInfo = ctx.getClassInfo(clazz.className)

    if (!clazz.kind.isClass && classInfo.hasRuntimeTypeInfo) {
      // Gen typeData -- for classes, we do it as part of the vtable generation
      val typeRef = IRTypes.ClassRef(clazz.className)
      val typeDataFieldValues = genTypeDataFieldValues(clazz, Nil)
      val typeDataGlobal =
        genTypeDataGlobal(typeRef, genTypeName.typeData, typeDataFieldValues, Nil)
      ctx.addGlobal(typeDataGlobal)
    }

    // Declare static fields
    for {
      field @ IRTrees.FieldDef(flags, name, _, ftpe) <- clazz.fields
      if flags.namespace.isStatic
    } {
      val global = WasmGlobal(
        genGlobalName.forStaticField(name.name),
        transformType(ftpe),
        WasmExpr(List(genZeroOf(ftpe))),
        isMutable = true
      )
      ctx.addGlobal(global)
    }

    // Generate method implementations
    for (method <- clazz.methods) {
      if (method.body.isDefined)
        genFunction(clazz, method)
    }

    clazz.kind match {
      case ClassKind.ModuleClass   => transformModuleClass(clazz)
      case ClassKind.Class         => transformClass(clazz)
      case ClassKind.HijackedClass => transformHijackedClass(clazz)
      case ClassKind.Interface     => transformInterface(clazz)

      case ClassKind.JSClass | ClassKind.JSModuleClass =>
        transformJSClass(clazz)
      case ClassKind.AbstractJSType | ClassKind.NativeJSClass | ClassKind.NativeJSModuleClass =>
        () // nothing to do
    }
  }

  def transformTopLevelExport(
      topLevelExport: LinkedTopLevelExport
  )(implicit ctx: WasmContext): Unit = {
    topLevelExport.tree match {
      case d: IRTrees.TopLevelJSClassExportDef => genDelayedTopLevelExport(d.exportName)
      case d: IRTrees.TopLevelModuleExportDef  => genDelayedTopLevelExport(d.exportName)
      case d: IRTrees.TopLevelMethodExportDef  => transformTopLevelMethodExportDef(d)
      case d: IRTrees.TopLevelFieldExportDef   => transformTopLevelFieldExportDef(d)
    }
  }

  private def genTypeDataFieldValues(
      clazz: LinkedClass,
      reflectiveProxies: List[ConcreteMethodInfo]
  )(implicit
      ctx: WasmContext
  ): List[WasmInstr] = {
    import genFieldName.typeData.{reflectiveProxies => _, _}

    val className = clazz.className
    val classInfo = ctx.getClassInfo(className)

    val kind = className match {
      case IRNames.ObjectClass         => KindObject
      case IRNames.BoxedUnitClass      => KindBoxedUnit
      case IRNames.BoxedBooleanClass   => KindBoxedBoolean
      case IRNames.BoxedCharacterClass => KindBoxedCharacter
      case IRNames.BoxedByteClass      => KindBoxedByte
      case IRNames.BoxedShortClass     => KindBoxedShort
      case IRNames.BoxedIntegerClass   => KindBoxedInteger
      case IRNames.BoxedLongClass      => KindBoxedLong
      case IRNames.BoxedFloatClass     => KindBoxedFloat
      case IRNames.BoxedDoubleClass    => KindBoxedDouble
      case IRNames.BoxedStringClass    => KindBoxedString

      case _ =>
        clazz.kind match {
          case ClassKind.Class | ClassKind.ModuleClass | ClassKind.HijackedClass => KindClass
          case ClassKind.Interface                                               => KindInterface
          case _                                                                 => KindJSType
        }
    }

    val isJSClassInstanceFuncOpt = genIsJSClassInstanceFunction(clazz)

    genTypeDataFieldValues(
      kind,
      classInfo.specialInstanceTypes,
      IRTypes.ClassRef(clazz.className),
      isJSClassInstanceFuncOpt,
      reflectiveProxies
    )
  }

  private def genIsJSClassInstanceFunction(clazz: LinkedClass)(implicit
      ctx: WasmContext
  ): Option[WasmFunctionName] = {
    import org.scalajs.ir.OriginalName.NoOriginalName

    implicit val noPos: Position = Position.NoPosition

    val hasIsJSClassInstance = clazz.kind match {
      case ClassKind.NativeJSClass => clazz.jsNativeLoadSpec.isDefined
      case ClassKind.JSClass       => clazz.jsClassCaptures.isEmpty
      case _                       => false
    }

    if (hasIsJSClassInstance) {
      val fb = new FunctionBuilder(
        ctx.moduleBuilder,
        genFunctionName.isJSClassInstance(clazz.className),
        noPos
      )
      val xParam = fb.addParam("x", WasmRefType.anyref)
      fb.setResultType(WasmInt32)
      fb.setFunctionType(ctx.isJSClassInstanceFuncTypeName)

      val instrs = fb

      if (clazz.kind == ClassKind.JSClass && !clazz.hasInstances) {
        /* We need to constant-fold the instance test, to avoid trying to
         * call $loadJSClass.className, since it will not exist at all.
         */
        instrs += I32_CONST(0) // false
      } else {
        instrs += LOCAL_GET(xParam)
        genLoadJSConstructor(instrs, clazz.className)
        instrs += CALL(genFunctionName.jsBinaryOps(IRTrees.JSBinaryOp.instanceof))
        instrs += CALL(genFunctionName.unbox(IRTypes.BooleanRef))
      }

      val func = fb.buildAndAddToModule()
      Some(func.name)
    } else {
      None
    }
  }

  private def genTypeDataFieldValues(
      kind: Int,
      specialInstanceTypes: Int,
      typeRef: IRTypes.NonArrayTypeRef,
      isJSClassInstanceFuncOpt: Option[WasmFunctionName],
      reflectiveProxies: List[ConcreteMethodInfo]
  )(implicit
      ctx: WasmContext
  ): List[WasmInstr] = {
    val nameStr = typeRef match {
      case typeRef: IRTypes.PrimRef =>
        typeRef.displayName
      case IRTypes.ClassRef(className) =>
        RuntimeClassNameMapperImpl.map(
          coreSpec.semantics.runtimeClassNameMapper,
          className.nameString
        )
    }

    val nameDataValue: List[WasmInstr] = ctx.getConstantStringDataInstr(nameStr)

    val strictAncestorsValue: List[WasmInstr] = {
      typeRef match {
        case IRTypes.ClassRef(className) =>
          val ancestors = ctx.getClassInfo(className).ancestors

          // By spec, the first element of `ancestors` is always the class itself
          assert(
            ancestors.headOption.contains(className),
            s"The ancestors of ${className.nameString} do not start with itself: $ancestors"
          )
          val strictAncestors = ancestors.tail

          val elems = for {
            ancestor <- strictAncestors
            if ctx.getClassInfo(ancestor).hasRuntimeTypeInfo
          } yield {
            GLOBAL_GET(genGlobalName.forVTable(ancestor))
          }
          elems :+ ARRAY_NEW_FIXED(genTypeName.typeDataArray, elems.size)
        case _ =>
          REF_NULL(WasmHeapType.None) :: Nil
      }
    }

    val cloneFunction = {
      val nullref = REF_NULL(WasmHeapType.NoFunc)
      typeRef match {
        case IRTypes.ClassRef(className) =>
          val classInfo = ctx.getClassInfo(className)
          // If the class is concrete and implements the `java.lang.Cloneable`,
          // `genCloneFunction` should've generated the clone function
          if (!classInfo.isAbstract && classInfo.ancestors.contains(IRNames.CloneableClass))
            REF_FUNC(genFunctionName.clone(className))
          else nullref
        case _ => nullref
      }
    }

    val isJSClassInstance = isJSClassInstanceFuncOpt match {
      case None           => REF_NULL(WasmHeapType.NoFunc)
      case Some(funcName) => REF_FUNC(funcName)
    }

    val reflectiveProxiesInstrs: List[WasmInstr] = {
      reflectiveProxies.flatMap { proxyInfo =>
        val proxyId = ctx.getReflectiveProxyId(proxyInfo.methodName)
        List(
          I32_CONST(proxyId),
          REF_FUNC(proxyInfo.tableEntryName),
          STRUCT_NEW(genTypeName.reflectiveProxy)
        )
      } :+ ARRAY_NEW_FIXED(genTypeName.reflectiveProxies, reflectiveProxies.size)
    }

    nameDataValue :::
      List(
        // kind
        I32_CONST(kind),
        // specialInstanceTypes
        I32_CONST(specialInstanceTypes)
      ) ::: (
        // strictAncestors
        strictAncestorsValue
      ) :::
      List(
        // componentType - always `null` since this method is not used for array types
        REF_NULL(WasmHeapType(genTypeName.typeData)),
        // name - initially `null`; filled in by the `typeDataName` helper
        REF_NULL(WasmHeapType.Any),
        // the classOf instance - initially `null`; filled in by the `createClassOf` helper
        REF_NULL(WasmHeapType(genTypeName.ClassStruct)),
        // arrayOf, the typeData of an array of this type - initially `null`; filled in by the `arrayTypeData` helper
        REF_NULL(WasmHeapType(genTypeName.ObjectVTable)),
        // clonefFunction - will be invoked from `clone()` method invokaion on the class
        cloneFunction,
        // isJSClassInstance - invoked from the `isInstance()` helper for JS types
        isJSClassInstance
      ) :::
      // reflective proxies - used to reflective call on the class at runtime.
      // Generated instructions create an array of reflective proxy structs, where each struct
      // contains the ID of the reflective proxy and a reference to the actual method implementation.
      reflectiveProxiesInstrs
  }

  private def genTypeDataGlobal(
      typeRef: IRTypes.NonArrayTypeRef,
      typeDataTypeName: WasmTypeName,
      typeDataFieldValues: List[WasmInstr],
      vtableElems: List[REF_FUNC]
  )(implicit ctx: WasmContext): WasmGlobal = {
    val instrs: List[WasmInstr] =
      typeDataFieldValues ::: vtableElems ::: STRUCT_NEW(typeDataTypeName) :: Nil
    WasmGlobal(
      genGlobalName.forVTable(typeRef),
      WasmRefType(typeDataTypeName),
      WasmExpr(instrs),
      isMutable = false
    )
  }

  /** @return
    *   Optionally returns the generated struct type for this class. If the given LinkedClass is an
    *   abstract class, returns None
    */
  private def transformClassCommon(
      clazz: LinkedClass
  )(implicit ctx: WasmContext): WasmStructType = {
    val className = clazz.name.name
    val typeRef = IRTypes.ClassRef(className)
    val classInfo = ctx.getClassInfo(className)

    // generate vtable type, this should be done for both abstract and concrete classes
    val vtableTypeName = genVTableType(classInfo)

    val isAbstractClass = !clazz.hasDirectInstances

    // we should't generate global vtable for abstract class because
    // - Can't generate Global vtable because we can't fill the slot for abstract methods
    // - We won't access vtable for abstract classes since we can't instantiate abstract classes, there's no point generating
    //
    // When we don't generate a vtable, we still generate the typeData

    if (!isAbstractClass) {
      // Generate an actual vtable
      val reflectiveProxies =
        classInfo.resolvedMethodInfos.valuesIterator.filter(_.methodName.isReflectiveProxy).toList
      val typeDataFieldValues = genTypeDataFieldValues(clazz, reflectiveProxies)
      val vtableElems = classInfo.tableEntries.map { methodName =>
        REF_FUNC(classInfo.resolvedMethodInfos(methodName).tableEntryName)
      }
      val globalVTable =
        genTypeDataGlobal(typeRef, vtableTypeName, typeDataFieldValues, vtableElems)
      ctx.addGlobal(globalVTable)
      genGlobalClassItable(clazz)
    } else if (classInfo.hasRuntimeTypeInfo) {
      // Only generate typeData
      val typeDataFieldValues = genTypeDataFieldValues(clazz, Nil)
      val globalTypeData =
        genTypeDataGlobal(typeRef, genTypeName.typeData, typeDataFieldValues, Nil)
      ctx.addGlobal(globalTypeData)
    }

    // Declare the struct type for the class
    val vtableField = WasmStructField(
      genFieldName.objStruct.vtable,
      WasmRefType(vtableTypeName),
      isMutable = false
    )
    val itablesField = WasmStructField(
      genFieldName.objStruct.itables,
      WasmRefType.nullable(genTypeName.itables),
      isMutable = false
    )
    val fields = classInfo.allFieldDefs.map(transformField)
    val structTypeName = genTypeName.forClass(clazz.name.name)
    val superType = clazz.superClass.map(s => genTypeName.forClass(s.name))
    val structType = WasmStructType(
      vtableField +: itablesField +: fields
    )
    val subType = WasmSubType(structTypeName, isFinal = false, superType, structType)
    ctx.mainRecType.addSubType(subType)

    // Define the `new` function and possibly the `clone` function, unless the class is abstract
    if (!isAbstractClass) {
      genNewDefaultFunc(clazz)
      if (clazz.ancestors.contains(IRNames.CloneableClass))
        genCloneFunction(clazz)
    }

    structType
  }

  private def genVTableType(classInfo: WasmClassInfo)(implicit ctx: WasmContext): WasmTypeName = {
    val typeName = genTypeName.forVTable(classInfo.name)
    val vtableFields =
      classInfo.tableEntries.map { methodName =>
        WasmStructField(
          genFieldName.forMethodTableEntry(methodName),
          WasmRefType(ctx.tableFunctionType(methodName)),
          isMutable = false
        )
      }
    val superType = classInfo.superClass match {
      case None    => genTypeName.typeData
      case Some(s) => genTypeName.forVTable(s)
    }
    val structType = WasmStructType(ctx.typeDataStructFields ::: vtableFields)
    val subType = WasmSubType(typeName, isFinal = false, Some(superType), structType)
    ctx.mainRecType.addSubType(subType)
    typeName
  }

  /** Generate type inclusion test for interfaces.
    *
    * The expression `isInstanceOf[<interface>]` will be compiled to a CALL to the function
    * generated by this method.
    *
    * TODO: Efficient type inclusion test. The current implementation generates a sparse array of
    * itables, which, although O(1), may not be optimal for large interfaces. More compressed data
    * structures could potentially improve performance in such cases.
    *
    * See https://github.com/tanishiking/scala-wasm/issues/27#issuecomment-2008252049
    */
  private def genInterfaceInstanceTest(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    implicit val pos = clazz.pos

    assert(clazz.kind == ClassKind.Interface)

    val classInfo = ctx.getClassInfo(clazz.className)

    val fb = new FunctionBuilder(
      ctx.moduleBuilder,
      genFunctionName.instanceTest(clazz.name.name),
      pos
    )
    val exprParam = fb.addParam("expr", WasmRefType.anyref)
    fb.setResultType(WasmInt32)

    val instrs = fb

    val itables = fb.addLocal("itables", WasmRefType.nullable(genTypeName.itables))
    val exprNonNullLocal = fb.addLocal("exprNonNull", WasmRefType.any)

    val itableIdx = ctx.getItableIdx(classInfo)
    instrs.block(WasmRefType.anyref) { testFail =>
      // if expr is not an instance of Object, return false
      instrs += LOCAL_GET(exprParam)
      instrs += BR_ON_CAST_FAIL(
        testFail,
        WasmRefType.anyref,
        WasmRefType(genTypeName.ObjectStruct)
      )

      // get itables and store
      instrs += STRUCT_GET(genTypeName.ObjectStruct, genFieldIdx.objStruct.itables)
      instrs += LOCAL_SET(itables)

      // Dummy return value from the block
      instrs += REF_NULL(WasmHeapType.Any)

      // if the itables is null (no interfaces are implemented)
      instrs += LOCAL_GET(itables)
      instrs += BR_ON_NULL(testFail)

      instrs += LOCAL_GET(itables)
      instrs += I32_CONST(itableIdx)
      instrs += ARRAY_GET(genTypeName.itables)
      instrs += REF_TEST(WasmRefType(genTypeName.forITable(clazz.name.name)))
      instrs += RETURN
    } // test fail

    if (classInfo.isAncestorOfHijackedClass) {
      /* It could be a hijacked class instance that implements this interface.
       * Test whether `jsValueType(expr)` is in the `specialInstanceTypes` bitset.
       * In other words, return `((1 << jsValueType(expr)) & specialInstanceTypes) != 0`.
       *
       * For example, if this class is `Comparable`,
       * `specialInstanceTypes == 0b00001111`, since `jl.Boolean`, `jl.String`
       * and `jl.Double` implement `Comparable`, but `jl.Void` does not.
       * If `expr` is a `number`, `jsValueType(expr) == 3`. We then test whether
       * `(1 << 3) & 0b00001111 != 0`, which is true because `(1 << 3) == 0b00001000`.
       * If `expr` is `undefined`, it would be `(1 << 4) == 0b00010000`, which
       * would give `false`.
       */
      val anyRefToVoidSig =
        WasmFunctionSignature(List(WasmRefType.anyref), Nil)

      instrs.block(anyRefToVoidSig) { isNullLabel =>
        // exprNonNull := expr; branch to isNullLabel if it is null
        instrs += BR_ON_NULL(isNullLabel)
        instrs += LOCAL_SET(exprNonNullLocal)

        // Load 1 << jsValueType(expr)
        instrs += I32_CONST(1)
        instrs += LOCAL_GET(exprNonNullLocal)
        instrs += CALL(genFunctionName.jsValueType)
        instrs += I32_SHL

        // return (... & specialInstanceTypes) != 0
        instrs += I32_CONST(classInfo.specialInstanceTypes)
        instrs += I32_AND
        instrs += I32_CONST(0)
        instrs += I32_NE
        instrs += RETURN
      }

      instrs += I32_CONST(0) // false
    } else {
      instrs += DROP
      instrs += I32_CONST(0) // false
    }

    fb.buildAndAddToModule()
  }

  private def genNewDefaultFunc(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    implicit val pos = clazz.pos

    val className = clazz.name.name
    val classInfo = ctx.getClassInfo(className)
    assert(clazz.hasDirectInstances)

    val structName = genTypeName.forClass(className)
    val fb = new FunctionBuilder(
      ctx.moduleBuilder,
      genFunctionName.newDefault(className),
      pos
    )
    fb.setResultType(WasmRefType(structName))

    val instrs = fb

    instrs += GLOBAL_GET(genGlobalName.forVTable(className))

    val interfaces = classInfo.ancestors.map(ctx.getClassInfo(_)).filter(_.isInterface)
    if (!interfaces.isEmpty)
      instrs += GLOBAL_GET(genGlobalName.forITable(className))
    else
      instrs += REF_NULL(WasmHeapType(genTypeName.itables))

    classInfo.allFieldDefs.foreach { f =>
      instrs += genZeroOf(f.ftpe)
    }
    instrs += STRUCT_NEW(structName)

    fb.buildAndAddToModule()
  }

  /** Generate clone function for the given class, if it is concrete and implements the Cloneable
    * interface. The generated clone function will be registered in the typeData of the class (which
    * resides in the vtable of the class), and will be invoked when the `super.clone()` method is
    * called on the class instance.
    */
  private def genCloneFunction(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    implicit val pos = clazz.pos

    val className = clazz.className
    val info = ctx.getClassInfo(className)

    val fb = new FunctionBuilder(
      ctx.moduleBuilder,
      genFunctionName.clone(className),
      pos
    )
    val fromParam = fb.addParam("from", WasmRefType(genTypeName.ObjectStruct))
    fb.setResultType(WasmRefType(genTypeName.ObjectStruct))
    fb.setFunctionType(ctx.cloneFunctionTypeName)

    val instrs = fb

    val heapType = WasmHeapType(genTypeName.forClass(className))

    val from = fb.addLocal("fromTyped", WasmRefType.nullable(heapType))
    val result = fb.addLocal("result", WasmRefType.nullable(heapType))

    instrs += LOCAL_GET(fromParam)
    instrs += REF_CAST(WasmRefType(heapType))
    instrs += LOCAL_SET(from)

    instrs += CALL(genFunctionName.newDefault(className))
    instrs += LOCAL_SET(result)
    info.allFieldDefs.foreach { field =>
      val fieldIdx = info.getFieldIdx(field.name.name)
      instrs += LOCAL_GET(result)
      instrs += LOCAL_GET(from)
      instrs += STRUCT_GET(genTypeName.forClass(className), fieldIdx)
      instrs += STRUCT_SET(genTypeName.forClass(className), fieldIdx)
    }
    instrs += LOCAL_GET(result)
    instrs += REF_AS_NOT_NULL

    fb.buildAndAddToModule()
  }

  private def genLoadModuleFunc(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    implicit val pos = clazz.pos

    assert(clazz.kind == ClassKind.ModuleClass)
    val ctor = clazz.methods
      .find(_.methodName.isConstructor)
      .getOrElse(throw new Error(s"Module class should have a constructor, ${clazz.name}"))
    val typeName = genTypeName.forClass(clazz.name.name)
    val globalInstanceName = genGlobalName.forModuleInstance(clazz.name.name)

    val ctorName = genFunctionName.forMethod(
      ctor.flags.namespace,
      clazz.name.name,
      ctor.name.name
    )

    val resultTyp = WasmRefType(typeName)

    val fb = new FunctionBuilder(
      ctx.moduleBuilder,
      genFunctionName.loadModule(clazz.className),
      pos
    )
    fb.setResultType(resultTyp)

    val instanceLocal = fb.addLocal("instance", resultTyp)

    val instrs = fb

    instrs.block(resultTyp) { nonNullLabel =>
      // load global, return if not null
      instrs += GLOBAL_GET(globalInstanceName)
      instrs += BR_ON_NON_NULL(nonNullLabel)

      // create an instance and call its constructor
      instrs += CALL(genFunctionName.newDefault(clazz.name.name))
      instrs += LOCAL_TEE(instanceLocal)
      instrs += CALL(ctorName)

      // store it in the global
      instrs += LOCAL_GET(instanceLocal)
      instrs += GLOBAL_SET(globalInstanceName)

      // return it
      instrs += LOCAL_GET(instanceLocal)
    }

    fb.buildAndAddToModule()
  }

  /** Generate global instance of the class itable. Their init value will be an array of null refs
    * of size = number of interfaces. They will be initialized in start function
    */
  private def genGlobalClassItable(
      clazz: LinkedClass
  )(implicit ctx: WasmContext): Unit = {
    val info = ctx.getClassInfo(clazz.className)
    val implementsAnyInterface = info.ancestors.exists(a => ctx.getClassInfo(a).isInterface)
    if (implementsAnyInterface) {
      val globalName = genGlobalName.forITable(clazz.className)
      ctx.addGlobalITable(clazz.className, genITableGlobal(globalName))
    }
  }

  private def genArrayClassItable()(implicit ctx: WasmContext): Unit =
    ctx.addGlobal(genITableGlobal(genGlobalName.arrayClassITable))

  private def genITableGlobal(name: WasmGlobalName)(implicit ctx: WasmContext): WasmGlobal = {
    val itablesInit = List(
      I32_CONST(ctx.itablesLength),
      ARRAY_NEW_DEFAULT(genTypeName.itables)
    )
    WasmGlobal(
      name,
      WasmRefType(genTypeName.itables),
      init = WasmExpr(itablesInit),
      isMutable = false
    )
  }

  private def transformClass(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    assert(clazz.kind == ClassKind.Class)
    transformClassCommon(clazz)
  }

  private def transformHijackedClass(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    // nothing to do
    ()
  }

  private def transformInterface(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    assert(clazz.kind == ClassKind.Interface)
    // gen itable type
    val className = clazz.name.name
    val classInfo = ctx.getClassInfo(clazz.className)
    val itableTypeName = genTypeName.forITable(className)
    val itableType = WasmStructType(
      classInfo.tableEntries.map { methodName =>
        WasmStructField(
          genFieldName.forMethodTableEntry(methodName),
          WasmRefType(ctx.tableFunctionType(methodName)),
          isMutable = false
        )
      }
    )
    ctx.mainRecType.addSubType(itableTypeName, itableType)

    if (clazz.hasInstanceTests)
      genInterfaceInstanceTest(clazz)
  }

  private def transformModuleClass(clazz: LinkedClass)(implicit ctx: WasmContext) = {
    assert(clazz.kind == ClassKind.ModuleClass)

    transformClassCommon(clazz)

    if (clazz.hasInstances) {
      val heapType = WasmHeapType(genTypeName.forClass(clazz.className))

      // global instance
      // (global name (ref null type))
      val global = WasmGlobal(
        genGlobalName.forModuleInstance(clazz.name.name),
        WasmRefType.nullable(heapType),
        WasmExpr(List(REF_NULL(heapType))),
        isMutable = true
      )
      ctx.addGlobal(global)

      genLoadModuleFunc(clazz)
    }
  }

  private def transformJSClass(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    assert(clazz.kind.isJSClass)

    // Define the globals holding the Symbols of private fields
    for (fieldDef <- clazz.fields) {
      fieldDef match {
        case IRTrees.FieldDef(flags, name, _, _) if !flags.namespace.isStatic =>
          ctx.addGlobal(
            WasmGlobal(
              genGlobalName.forJSPrivateField(name.name),
              WasmRefType.anyref,
              WasmExpr(List(REF_NULL(WasmHeapType.Any))),
              isMutable = true
            )
          )
          ctx.addJSPrivateFieldName(name.name)
        case _ =>
          ()
      }
    }

    if (clazz.hasInstances) {
      genCreateJSClassFunction(clazz)

      if (clazz.jsClassCaptures.isEmpty)
        genLoadJSClassFunction(clazz)

      if (clazz.kind == ClassKind.JSModuleClass)
        genLoadJSModuleFunction(clazz)
    }
  }

  private def genCreateJSClassFunction(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    implicit val noPos: Position = Position.NoPosition

    val jsClassCaptures = clazz.jsClassCaptures.getOrElse(Nil)

    /* We need to decompose the body of the constructor into 3 closures.
     * Given an IR constructor of the form
     *   constructor(...params) {
     *     preSuperStats;
     *     super(...superArgs);
     *     postSuperStats;
     *   }
     * We will create closures for `preSuperStats`, `superArgs` and `postSuperStats`.
     *
     * There is one huge catch: `preSuperStats` can declare `VarDef`s at its top-level,
     * and those vars are still visible inside `superArgs` and `postSuperStats`.
     * The `preSuperStats` must therefore return a struct with the values of its
     * declared vars, which will be given as an additional argument to `superArgs`
     * and `postSuperStats`. We call that struct the `preSuperEnv`.
     *
     * In the future, we should optimize `preSuperEnv` to only store locals that
     * are still used by `superArgs` and/or `postSuperArgs`.
     */

    val ctor = clazz.jsConstructorDef.get
    val allCtorParams = ctor.args ::: ctor.restParam.toList
    val ctorBody = ctor.body

    // Compute the pre-super environment
    val preSuperDecls = ctorBody.beforeSuper.collect { case varDef: IRTrees.VarDef =>
      varDef
    }

    // Build the `preSuperStats` function
    val preSuperStatsFun = {
      val preSuperEnvStructTypeName = ctx.getClosureDataStructType(preSuperDecls.map(_.vtpe))
      val preSuperEnvTyp = WasmRefType(preSuperEnvStructTypeName)

      implicit val fctx = WasmFunctionContext(
        Some(clazz.className),
        genFunctionName.preSuperStats(clazz.className),
        Some(jsClassCaptures),
        preSuperVarDefs = None,
        hasNewTarget = true,
        receiverTyp = None,
        allCtorParams,
        List(preSuperEnvTyp)
      )

      import fctx.instrs

      WasmExpressionBuilder.generateBlockStats(ctorBody.beforeSuper) {
        // Build and return the preSuperEnv struct
        for (varDef <- preSuperDecls)
          instrs += LOCAL_GET(fctx.lookupLocalAssertLocalStorage(varDef.name.name))
        instrs += STRUCT_NEW(preSuperEnvStructTypeName)
      }

      fctx.buildAndAddToContext()
    }

    // Build the `superArgs` function
    val superArgsFun = {
      implicit val fctx = WasmFunctionContext(
        Some(clazz.className),
        genFunctionName.superArgs(clazz.className),
        Some(jsClassCaptures),
        Some(preSuperDecls),
        hasNewTarget = true,
        receiverTyp = None,
        allCtorParams,
        List(WasmRefType.anyref) // a js.Array
      )

      WasmExpressionBuilder.generateIRBody(
        IRTrees.JSArrayConstr(ctorBody.superCall.args),
        IRTypes.AnyType
      )

      fctx.buildAndAddToContext()
    }

    // Build the `postSuperStats` function
    val postSuperStatsFun = {
      implicit val fctx = WasmFunctionContext(
        Some(clazz.className),
        genFunctionName.postSuperStats(clazz.className),
        Some(jsClassCaptures),
        Some(preSuperDecls),
        hasNewTarget = true,
        receiverTyp = Some(WasmRefType.anyref),
        allCtorParams,
        List(WasmRefType.anyref)
      )

      WasmExpressionBuilder.generateIRBody(
        IRTrees.Block(ctorBody.afterSuper),
        IRTypes.AnyType
      )

      fctx.buildAndAddToContext()
    }

    // Build the actual `createJSClass` function
    val createJSClassFun = {
      val fb = new FunctionBuilder(
        ctx.moduleBuilder,
        genFunctionName.createJSClassOf(clazz.className),
        clazz.pos
      )
      val classCaptureParams = jsClassCaptures.map { cc =>
        fb.addParam("cc." + cc.name.name.nameString, transformType(cc.ptpe))
      }
      fb.setResultType(WasmRefType.any)

      val instrs = fb

      val dataStructTypeName = ctx.getClosureDataStructType(jsClassCaptures.map(_.ptpe))

      // --- Internal name management of `createJSClass`

      val dataStructLocal = fb.addLocal("classCaptures", WasmRefType(dataStructTypeName))
      val jsClassLocal = fb.addLocal("jsClass", WasmRefType.any)

      var lastInnerFuncIndex = -1
      def genInnerFuncName(): WasmFunctionName = {
        lastInnerFuncIndex += 1
        WasmFunctionName(fb.functionName.name + "__c" + lastInnerFuncIndex)
      }

      // --- Actual start of instructions of `createJSClass`

      // Bundle class captures in a capture data struct -- leave it on the stack for createJSClass
      for (classCaptureParam <- classCaptureParams)
        instrs += LOCAL_GET(classCaptureParam)
      instrs += STRUCT_NEW(dataStructTypeName)
      instrs += LOCAL_TEE(dataStructLocal)

      val classCaptureParamsOfTypeAny: Map[IRNames.LocalName, WasmLocalName] = {
        jsClassCaptures
          .zip(classCaptureParams)
          .collect { case (IRTrees.ParamDef(ident, _, IRTypes.AnyType, _), param) =>
            ident.name -> param
          }
          .toMap
      }

      def genLoadIsolatedTree(tree: IRTrees.Tree): Unit = {
        tree match {
          case IRTrees.StringLiteral(value) =>
            // Common shape for all the `nameTree` expressions
            instrs ++= ctx.getConstantStringInstr(value)

          case IRTrees.VarRef(IRTrees.LocalIdent(localName))
              if classCaptureParamsOfTypeAny.contains(localName) =>
            /* Common shape for the `jsSuperClass` value
             * We can only deal with class captures of type `AnyType` in this way,
             * since otherwise we might need `adapt` to box the values.
             */
            instrs += LOCAL_GET(classCaptureParamsOfTypeAny(localName))

          case _ =>
            // For everything else, put the tree in its own function and call it
            val closureFuncName = genInnerFuncName()
            locally {
              implicit val fctx: WasmFunctionContext = WasmFunctionContext(
                enclosingClassName = None,
                closureFuncName,
                Some(jsClassCaptures),
                receiverTyp = None,
                paramDefs = Nil,
                List(WasmRefType.anyref)
              )
              WasmExpressionBuilder.generateIRBody(tree, IRTypes.AnyType)
              fctx.buildAndAddToContext()
            }
            instrs += LOCAL_GET(dataStructLocal)
            instrs += CALL(closureFuncName)
        }
      }

      /* Load super constructor; specified by
       * https://lampwww.epfl.ch/~doeraene/sjsir-semantics/#sec-sjsir-classdef-runtime-semantics-evaluation
       * - if `jsSuperClass` is defined, evaluate it;
       * - otherwise load the JS constructor of the declared superClass,
       *   as if by `LoadJSConstructor`.
       */
      clazz.jsSuperClass match {
        case None =>
          genLoadJSConstructor(instrs, clazz.superClass.get.name)
        case Some(jsSuperClassTree) =>
          genLoadIsolatedTree(jsSuperClassTree)
      }

      // Load the references to the 3 functions that make up the constructor
      instrs += ctx.refFuncWithDeclaration(preSuperStatsFun.name)
      instrs += ctx.refFuncWithDeclaration(superArgsFun.name)
      instrs += ctx.refFuncWithDeclaration(postSuperStatsFun.name)

      // Load the array of field names and initial values
      instrs += CALL(genFunctionName.jsNewArray)
      for (fieldDef <- clazz.fields if !fieldDef.flags.namespace.isStatic) {
        // Append the name
        fieldDef match {
          case IRTrees.FieldDef(_, name, _, _) =>
            instrs += GLOBAL_GET(genGlobalName.forJSPrivateField(name.name))
          case IRTrees.JSFieldDef(_, nameTree, _) =>
            genLoadIsolatedTree(nameTree)
        }
        instrs += CALL(genFunctionName.jsArrayPush)

        // Append the boxed representation of the zero of the field
        instrs += genBoxedZeroOf(fieldDef.ftpe)
        instrs += CALL(genFunctionName.jsArrayPush)
      }

      // Call the createJSClass helper to bundle everything
      if (ctor.restParam.isDefined) {
        instrs += I32_CONST(ctor.args.size) // number of fixed params
        instrs += CALL(genFunctionName.createJSClassRest)
      } else {
        instrs += CALL(genFunctionName.createJSClass)
      }

      // Store the result, locally in `jsClass` and possibly in the global cache
      if (clazz.jsClassCaptures.isEmpty) {
        // Static JS class with a global cache
        instrs += LOCAL_TEE(jsClassLocal)
        instrs += GLOBAL_SET(genGlobalName.forJSClassValue(clazz.className))
      } else {
        // Local or inner JS class, which is new every time
        instrs += LOCAL_SET(jsClassLocal)
      }

      // Install methods and properties
      for (methodOrProp <- clazz.exportedMembers) {
        val isStatic = methodOrProp.flags.namespace.isStatic
        instrs += LOCAL_GET(dataStructLocal)
        instrs += LOCAL_GET(jsClassLocal)

        val receiverTyp = if (isStatic) None else Some(WasmRefType.anyref)

        methodOrProp match {
          case IRTrees.JSMethodDef(flags, nameTree, params, restParam, body) =>
            genLoadIsolatedTree(nameTree)

            val closureFuncName = genInnerFuncName()
            locally {
              implicit val fctx: WasmFunctionContext = WasmFunctionContext(
                Some(clazz.className),
                closureFuncName,
                Some(jsClassCaptures),
                receiverTyp,
                params ::: restParam.toList,
                List(WasmRefType.anyref)
              )
              WasmExpressionBuilder.generateIRBody(body, IRTypes.AnyType)
              fctx.buildAndAddToContext()
            }
            instrs += ctx.refFuncWithDeclaration(closureFuncName)

            instrs += I32_CONST(if (restParam.isDefined) params.size else -1)
            if (isStatic)
              instrs += CALL(genFunctionName.installJSStaticMethod)
            else
              instrs += CALL(genFunctionName.installJSMethod)

          case IRTrees.JSPropertyDef(flags, nameTree, optGetter, optSetter) =>
            genLoadIsolatedTree(nameTree)

            optGetter match {
              case None =>
                instrs += REF_NULL(WasmHeapType.Func)

              case Some(getterBody) =>
                val closureFuncName = genInnerFuncName()
                locally {
                  implicit val fctx: WasmFunctionContext = WasmFunctionContext(
                    Some(clazz.className),
                    closureFuncName,
                    Some(jsClassCaptures),
                    receiverTyp,
                    Nil,
                    List(WasmRefType.anyref)
                  )
                  WasmExpressionBuilder.generateIRBody(getterBody, IRTypes.AnyType)
                  fctx.buildAndAddToContext()
                }
                instrs += ctx.refFuncWithDeclaration(closureFuncName)
            }

            optSetter match {
              case None =>
                instrs += REF_NULL(WasmHeapType.Func)

              case Some((setterParamDef, setterBody)) =>
                val closureFuncName = genInnerFuncName()
                locally {
                  implicit val fctx: WasmFunctionContext = WasmFunctionContext(
                    Some(clazz.className),
                    closureFuncName,
                    Some(jsClassCaptures),
                    receiverTyp,
                    setterParamDef :: Nil,
                    Nil
                  )
                  WasmExpressionBuilder.generateIRBody(setterBody, IRTypes.NoType)
                  fctx.buildAndAddToContext()
                }
                instrs += ctx.refFuncWithDeclaration(closureFuncName)
            }

            if (isStatic)
              instrs += CALL(genFunctionName.installJSStaticProperty)
            else
              instrs += CALL(genFunctionName.installJSProperty)
        }
      }

      // Static fields
      for (fieldDef <- clazz.fields if fieldDef.flags.namespace.isStatic) {
        // Load class value
        instrs += LOCAL_GET(jsClassLocal)

        // Load name
        fieldDef match {
          case IRTrees.FieldDef(_, name, _, _) =>
            throw new AssertionError(
              s"Unexpected private static field ${name.name.nameString} "
                + s"in JS class ${clazz.className.nameString}"
            )
          case IRTrees.JSFieldDef(_, nameTree, _) =>
            genLoadIsolatedTree(nameTree)
        }

        // Generate boxed representation of the zero of the field
        instrs += genBoxedZeroOf(fieldDef.ftpe)

        instrs += CALL(genFunctionName.installJSField)
      }

      // Class initializer
      for (classInit <- clazz.methods.find(_.methodName.isClassInitializer)) {
        assert(
          clazz.jsClassCaptures.isEmpty,
          s"Illegal class initializer in non-static class ${clazz.className.nameString}"
        )
        val namespace = IRTrees.MemberNamespace.StaticConstructor
        instrs += CALL(
          genFunctionName.forMethod(namespace, clazz.className, IRNames.ClassInitializerName)
        )
      }

      // Final result
      instrs += LOCAL_GET(jsClassLocal)

      fb.buildAndAddToModule()
    }
  }

  private def genLoadJSClassFunction(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    implicit val pos = clazz.pos

    val cachedJSClassGlobal = WasmGlobal(
      genGlobalName.forJSClassValue(clazz.className),
      WasmRefType.anyref,
      WasmExpr(List(REF_NULL(WasmHeapType.Any))),
      isMutable = true
    )
    ctx.addGlobal(cachedJSClassGlobal)

    val fb = new FunctionBuilder(
      ctx.moduleBuilder,
      genFunctionName.loadJSClass(clazz.className),
      pos
    )
    fb.setResultType(WasmRefType.any)

    val instrs = fb

    instrs.block(WasmRefType.any) { doneLabel =>
      // Load cached JS class, return if non-null
      instrs += GLOBAL_GET(cachedJSClassGlobal.name)
      instrs += BR_ON_NON_NULL(doneLabel)
      // Otherwise, call createJSClass -- it will also store the class in the cache
      instrs += CALL(genFunctionName.createJSClassOf(clazz.className))
    }

    fb.buildAndAddToModule()
  }

  private def genLoadJSModuleFunction(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    implicit val pos = clazz.pos

    val className = clazz.className
    val cacheGlobalName = genGlobalName.forModuleInstance(className)

    ctx.addGlobal(
      WasmGlobal(
        cacheGlobalName,
        WasmRefType.anyref,
        WasmExpr(List(REF_NULL(WasmHeapType.Any))),
        isMutable = true
      )
    )

    val fb = new FunctionBuilder(
      ctx.moduleBuilder,
      genFunctionName.loadModule(className),
      pos
    )
    fb.setResultType(WasmRefType.anyref)

    val instrs = fb

    instrs.block(WasmRefType.anyref) { doneLabel =>
      // Load cached instance; return if non-null
      instrs += GLOBAL_GET(cacheGlobalName)
      instrs += BR_ON_NON_NULL(doneLabel)

      // Get the JS class and instantiate it
      instrs += CALL(genFunctionName.loadJSClass(className))
      instrs += CALL(genFunctionName.jsNewArray)
      instrs += CALL(genFunctionName.jsNew)

      // Store and return the result
      instrs += GLOBAL_SET(cacheGlobalName)
      instrs += GLOBAL_GET(cacheGlobalName)
    }

    fb.buildAndAddToModule()
  }

  private def transformTopLevelMethodExportDef(
      exportDef: IRTrees.TopLevelMethodExportDef
  )(implicit ctx: WasmContext): Unit = {
    implicit val pos = exportDef.pos

    val method = exportDef.methodDef
    val exportedName = exportDef.topLevelExportName

    implicit val fctx = WasmFunctionContext(
      enclosingClassName = None,
      genFunctionName.forExport(exportedName),
      receiverTyp = None,
      method.args ::: method.restParam.toList,
      IRTypes.AnyType
    )

    WasmExpressionBuilder.generateIRBody(method.body, IRTypes.AnyType)

    val func = fctx.buildAndAddToContext()

    if (method.restParam.isEmpty) {
      ctx.addExport(WasmExport.Function(exportedName, func.name))
    } else {
      /* We cannot directly export the function. We will create a closure
       * wrapper in the start function and export that instead.
       */
      genDelayedTopLevelExport(exportedName)
    }
  }

  private def transformTopLevelFieldExportDef(
      exportDef: IRTrees.TopLevelFieldExportDef
  )(implicit ctx: WasmContext): Unit = {
    val exprt = WasmExport.Global(
      exportDef.exportName,
      genGlobalName.forStaticField(exportDef.field.name)
    )
    ctx.addExport(exprt)
  }

  /** Generates a delayed top-level export global, to be initialized in the `start` function.
    *
    * Some top-level exports need to be initialized by run-time code because they need to call
    * initializing functions:
    *
    *   - methods with a `...rest` need to be initialized with the `closureRestNoArg` helper.
    *   - JS classes need to be initialized with their `loadJSClass` helper.
    *   - JS modules need to be initialized with their `loadModule` helper.
    *
    * For all of those, we use `genDelayedTopLevelExport` to generate a Wasm global initialized with
    * `null` and to export it. We actually initialize the global in the `start` function (see
    * `genStartFunction()` in `WasmContext`).
    */
  private def genDelayedTopLevelExport(exportedName: String)(implicit ctx: WasmContext): Unit = {
    val globalName = genGlobalName.forTopLevelExport(exportedName)
    ctx.addGlobal(
      WasmGlobal(
        globalName,
        WasmRefType.anyref,
        WasmExpr(List(REF_NULL(WasmHeapType.None))),
        isMutable = true
      )
    )
    ctx.addExport(WasmExport.Global(exportedName, globalName))
  }

  private def genFunction(
      clazz: LinkedClass,
      method: IRTrees.MethodDef
  )(implicit ctx: WasmContext): Unit = {
    implicit val pos = method.pos

    val namespace = method.flags.namespace
    val className = clazz.className
    val methodName = method.methodName

    val functionName = genFunctionName.forMethod(namespace, className, methodName)

    val isHijackedClass = ctx.getClassInfo(className).kind == ClassKind.HijackedClass

    val receiverTyp =
      if (namespace.isStatic)
        None
      else if (isHijackedClass)
        Some(transformType(IRTypes.BoxedClassToPrimType(className)))
      else
        Some(transformClassType(className).toNonNullable)

    // Prepare for function context, set receiver and parameters
    implicit val fctx = WasmFunctionContext(
      Some(className),
      functionName,
      receiverTyp,
      method.args,
      method.resultType
    )

    // build function body
    val body = method.body.getOrElse(throw new Exception("abstract method cannot be transformed"))
    WasmExpressionBuilder.generateIRBody(body, method.resultType)

    fctx.buildAndAddToContext()

    if (namespace == IRTrees.MemberNamespace.Public && !isHijackedClass) {
      /* Also generate the bridge that is stored in the table entries. In table
       * entries, the receiver type is always `(ref any)`.
       *
       * TODO: generate this only when the method is actually referred to from
       * at least one table.
       */

      val fb = new FunctionBuilder(
        ctx.moduleBuilder,
        genFunctionName.forTableEntry(className, methodName),
        pos
      )
      val receiverParam = fb.addParam("<this>", WasmRefType.any)
      val argParams = method.args.map { arg =>
        fb.addParam(arg.name.name.nameString, TypeTransformer.transformType(arg.ptpe))
      }
      fb.setResultTypes(TypeTransformer.transformResultType(method.resultType))
      fb.setFunctionType(ctx.tableFunctionType(methodName))

      val instrs = fb

      // Load and cast down the receiver
      instrs += LOCAL_GET(receiverParam)
      receiverTyp match {
        case Some(Types.WasmRefType(_, WasmHeapType.Any)) =>
          () // no cast necessary
        case Some(receiverTyp: Types.WasmRefType) =>
          instrs += REF_CAST(receiverTyp)
        case _ =>
          throw new AssertionError(s"Unexpected receiver type $receiverTyp")
      }

      // Load the other parameters
      for (argParam <- argParams)
        instrs += LOCAL_GET(argParam)

      // Call the statically resolved method
      instrs += RETURN_CALL(functionName)

      fb.buildAndAddToModule()
    }
  }

  private def transformField(
      field: IRTrees.FieldDef
  )(implicit ctx: WasmContext): WasmStructField = {
    WasmStructField(
      genFieldName.forClassInstanceField(field.name.name),
      transformType(field.ftpe),
      // needs to be mutable even if it's flags.isMutable = false
      // because it's initialized by constructor
      isMutable = true // field.flags.isMutable
    )
  }
}
