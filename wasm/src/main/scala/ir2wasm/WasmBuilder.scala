package wasm
package ir2wasm

import wasm4s._
import wasm4s.WasmContext._
import wasm4s.Names._
import wasm4s.Types._
import wasm4s.WasmInstr._
import wasm4s.WasmImmediate._
import TypeTransformer._

import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{ClassKind, Position}

import org.scalajs.linker.standard.{LinkedClass, LinkedTopLevelExport}

import collection.mutable
import java.awt.Window.Type
import _root_.wasm4s.Defaults

class WasmBuilder {
  // val module = new WasmModule()

  def genPrimitiveTypeDataGlobals()(implicit ctx: WasmContext): Unit = {
    val primRefsWithTypeData = List(
      IRTypes.VoidRef,
      IRTypes.BooleanRef,
      IRTypes.CharRef,
      IRTypes.ByteRef,
      IRTypes.ShortRef,
      IRTypes.IntRef,
      IRTypes.LongRef,
      IRTypes.FloatRef,
      IRTypes.DoubleRef
    )

    for (primRef <- primRefsWithTypeData) {
      val typeDataFieldValues = genTypeDataFieldValues(kind = 1, primRef)
      val typeDataGlobal =
        genTypeDataGlobal(primRef, WasmStructType.typeData, typeDataFieldValues, Nil)
      ctx.addGlobal(typeDataGlobal)
    }
  }

  def transformClassDef(clazz: LinkedClass)(implicit ctx: WasmContext) = {
    if (!clazz.kind.isClass && clazz.hasRuntimeTypeInfo) {
      // Gen typeData -- for classes, we do it as part of the vtable generation
      val typeRef = IRTypes.ClassRef(clazz.className)
      val typeDataFieldValues = genTypeDataFieldValues(clazz)
      val typeDataGlobal =
        genTypeDataGlobal(typeRef, WasmStructType.typeData, typeDataFieldValues, Nil)
      ctx.addGlobal(typeDataGlobal)
    }

    // Declare static fields
    for {
      field @ IRTrees.FieldDef(flags, name, _, ftpe) <- clazz.fields
      if flags.namespace.isStatic
    } {
      val typ = transformType(ftpe)
      val global = WasmGlobal(
        WasmGlobalName.WasmGlobalStaticFieldName(name.name),
        typ,
        WasmExpr(List(Defaults.defaultValue(typ))),
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

  def genArrayClasses()(implicit ctx: WasmContext): Unit = {
    import WasmTypeName.WasmStructTypeName

    // The super type is always j.l.Object
    val superType = WasmTypeName.WasmVTableTypeName(IRNames.ObjectClass)

    // The vtable type is always the same as j.l.Object
    val vtableTypeName = Names.WasmTypeName.WasmVTableTypeName.ObjectVTable
    val vtableField = WasmStructField(
      Names.WasmFieldName.vtable,
      WasmRefType(WasmHeapType.Type(vtableTypeName)),
      isMutable = false
    )

    val typeRefsWithArrays: List[(WasmStructTypeName, WasmArrayType)] = List(
      (WasmStructTypeName.BooleanArray, WasmArrayType.i8Array),
      (WasmStructTypeName.CharArray, WasmArrayType.i16Array),
      (WasmStructTypeName.ByteArray, WasmArrayType.i8Array),
      (WasmStructTypeName.ShortArray, WasmArrayType.i16Array),
      (WasmStructTypeName.IntArray, WasmArrayType.i32Array),
      (WasmStructTypeName.LongArray, WasmArrayType.i64Array),
      (WasmStructTypeName.FloatArray, WasmArrayType.f32Array),
      (WasmStructTypeName.DoubleArray, WasmArrayType.f64Array),
      (WasmStructTypeName.ObjectArray, WasmArrayType.anyArray)
    )

    for ((structTypeName, underlyingArrayType) <- typeRefsWithArrays) {
      val underlyingArrayField = WasmStructField(
        WasmFieldName.arrayField,
        WasmRefType(WasmHeapType.Type(underlyingArrayType.name)),
        isMutable = false
      )

      val structType = WasmStructType(
        structTypeName,
        List(vtableField, WasmStructField.itables, underlyingArrayField),
        Some(Names.WasmTypeName.WasmStructTypeName(IRNames.ObjectClass))
      )
      ctx.addGCType(structType)
    }
  }

  def transformTopLevelExport(
      topLevelExport: LinkedTopLevelExport
  )(implicit ctx: WasmContext): Unit = {
    topLevelExport.tree match {
      case d: IRTrees.TopLevelJSClassExportDef => ???
      case d: IRTrees.TopLevelModuleExportDef  => ???
      case d: IRTrees.TopLevelMethodExportDef  => transformTopLevelMethodExportDef(d)
      case d: IRTrees.TopLevelFieldExportDef   => transformTopLevelFieldExportDef(d)
    }
  }

  private def genTypeDataFieldValues(clazz: LinkedClass)(implicit
      ctx: WasmContext
  ): List[WasmInstr] = {
    val kind = clazz.kind match {
      case ClassKind.Class | ClassKind.ModuleClass | ClassKind.HijackedClass => 0
      case ClassKind.Interface                                               => 3
      case _                                                                 => 4
    }

    genTypeDataFieldValues(kind, IRTypes.ClassRef(clazz.className))
  }

  private def genTypeDataFieldValues(kind: Int, typeRef: IRTypes.NonArrayTypeRef)(implicit
      ctx: WasmContext
  ): List[WasmInstr] = {
    val nameStr = typeRef match {
      case typeRef: IRTypes.PrimRef    => typeRef.displayName
      case IRTypes.ClassRef(className) => className.nameString
    }

    val nameDataValueItems = nameStr.toList.map(c => I32_CONST(I32(c.toInt)))
    val nameDataValueArrayNew =
      ARRAY_NEW_FIXED(
        TypeIdx(WasmTypeName.WasmArrayTypeName.i16Array),
        I32(nameDataValueItems.size)
      )
    val nameDataValue: List[WasmInstr] = nameDataValueItems :+ nameDataValueArrayNew

    val cloneFunction = {
      val nullref =
        REF_NULL(HeapType(WasmHeapType.Type(ctx.cloneFunctionTypeName)))
      typeRef match {
        case IRTypes.ClassRef(className) =>
          val classInfo = ctx.getClassInfo(className)
          // If the class implements the `java.lang.Cloneable`,
          // `HelperFunctions.genCloneFunction` should've generated the clone function
          if (classInfo.ancestors.contains(IRNames.CloneableClass))
            REF_FUNC(FuncIdx(WasmFunctionName.clone(className)))
          else nullref
        case _ => nullref
      }
    }

    nameDataValue :::
      List(
        // kind
        I32_CONST(I32(kind)),
        // componentType - always `null` since this method is not used for array types
        REF_NULL(HeapType(WasmHeapType.Type(WasmTypeName.WasmStructTypeName.typeData))),
        // name - initially `null`; filled in by the `typeDataName` helper
        REF_NULL(HeapType(WasmHeapType.Simple.Any)),
        // the classOf instance - initially `null`; filled in by the `createClassOf` helper
        REF_NULL(HeapType(WasmHeapType.ClassType)),
        // arrayOf, the typeData of an array of this type - initially `null`; filled in by the `arrayTypeData` helper
        REF_NULL(HeapType(WasmHeapType.Type(WasmTypeName.WasmVTableTypeName.ObjectVTable))),
        // clonefFunction - will be invoked from `clone()` method invokaion on the class
        cloneFunction
      )
  }

  private def genTypeDataGlobal(
      typeRef: IRTypes.NonArrayTypeRef,
      typeDataType: WasmStructType,
      typeDataFieldValues: List[WasmInstr],
      vtableElems: List[REF_FUNC]
  )(implicit ctx: WasmContext): WasmGlobal = {
    val instrs: List[WasmInstr] =
      typeDataFieldValues ::: vtableElems ::: STRUCT_NEW(typeDataType.name) :: Nil
    WasmGlobal(
      WasmGlobalName.WasmGlobalVTableName(typeRef),
      WasmRefType(WasmHeapType.Type(typeDataType.name)),
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
    val vtable = ctx.calculateVtableType(className)
    val vtableType = genVTableType(clazz, vtable.functions)
    ctx.addGCType(vtableType)

    val isAbstractClass = !clazz.hasDirectInstances

    // we should't generate global vtable for abstract class because
    // - Can't generate Global vtable because we can't fill the slot for abstract methods
    // - We won't access vtable for abstract classes since we can't instantiate abstract classes, there's no point generating
    //
    // When we don't generate a vtable, we still generate the typeData

    val typeDataFieldValues = genTypeDataFieldValues(clazz)

    if (!isAbstractClass) {
      // Generate an actual vtable
      val functions = ctx.calculateGlobalVTable(className)
      val vtableElems = functions.map(method => WasmInstr.REF_FUNC(method.name))
      val globalVTable = genTypeDataGlobal(typeRef, vtableType, typeDataFieldValues, vtableElems)
      ctx.addGlobal(globalVTable)
      genGlobalClassItable(clazz)
    } else {
      // Only generate typeData
      val globalTypeData =
        genTypeDataGlobal(typeRef, WasmStructType.typeData, typeDataFieldValues, Nil)
      ctx.addGlobal(globalTypeData)
    }

    // Declare the struct type for the class
    val vtableField = WasmStructField(
      Names.WasmFieldName.vtable,
      WasmRefType(WasmHeapType.Type(vtableType.name)),
      isMutable = false
    )
    val fields = classInfo.allFieldDefs.map(transformField)
    val structType = WasmStructType(
      Names.WasmTypeName.WasmStructTypeName(clazz.name.name),
      vtableField +: WasmStructField.itables +: fields,
      clazz.superClass.map(s => Names.WasmTypeName.WasmStructTypeName(s.name))
    )
    ctx.addGCType(structType)

    // Define the `new` function, unless the class is abstract
    if (!isAbstractClass) HelperFunctions.genNewDefault(clazz)
    structType
  }

  private def genVTableType(clazz: LinkedClass, functions: List[WasmFunctionInfo])(implicit
      ctx: WasmContext
  ): WasmStructType = {
    val vtableFields =
      functions.map { method =>
        WasmStructField(
          Names.WasmFieldName(method.name),
          WasmRefNullType(WasmHeapType.Func(method.toWasmFunctionType().name)),
          isMutable = false
        )
      }
    val superType = clazz.superClass match {
      case None    => WasmTypeName.WasmStructTypeName.typeData
      case Some(s) => WasmTypeName.WasmVTableTypeName(s.name)
    }
    WasmStructType(
      Names.WasmTypeName.WasmVTableTypeName(clazz.name.name),
      WasmStructType.typeData.fields ::: vtableFields,
      Some(superType)
    )
  }

  private def genLoadModuleFunc(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    assert(clazz.kind == ClassKind.ModuleClass)
    val ctor = clazz.methods
      .find(_.methodName.isConstructor)
      .getOrElse(throw new Error(s"Module class should have a constructor, ${clazz.name}"))
    val typeName = WasmTypeName.WasmStructTypeName(clazz.name.name)
    val globalInstanceName = WasmGlobalName.WasmModuleInstanceName.fromIR(clazz.name.name)

    val ctorName = WasmFunctionName(
      ctor.flags.namespace,
      clazz.name.name,
      ctor.name.name
    )

    val body = List(
      // global.get $module_name
      // ref.if_null
      //   ref.null $module_type
      //   call $module_init ;; should set to global
      // end
      // global.get $module_name
      GLOBAL_GET(GlobalIdx(globalInstanceName)), // [rt]
      REF_IS_NULL, // [rt] -> [i32] (bool)
      IF(WasmImmediate.BlockType.ValueType(None)),
      CALL(FuncIdx(WasmFunctionName.newDefault(clazz.name.name))),
      GLOBAL_SET(GlobalIdx(globalInstanceName)),
      GLOBAL_GET(GlobalIdx(globalInstanceName)),
      CALL(FuncIdx(ctorName)),
      // ELSE,
      END,
      GLOBAL_GET(GlobalIdx(globalInstanceName)) // [rt]
    )

    val sig =
      WasmFunctionSignature(Nil, List(WasmRefNullType(WasmHeapType.Type(typeName))))
    val loadModuleTypeName = ctx.addFunctionType(sig)
    val func = WasmFunction(
      WasmFunctionName.loadModule(clazz.name.name),
      WasmFunctionType(loadModuleTypeName, sig),
      Nil,
      WasmExpr(body)
    )
    ctx.addFunction(func)
  }

  /** Generate global instance of the class itable. Their init value will be an array of null refs
    * of size = number of interfaces. They will be initialized in start function
    */
  private def genGlobalClassItable(
      clazz: LinkedClass
  )(implicit ctx: WasmContext): Unit = {
    val info = ctx.getClassInfo(clazz.name.name)
    val interfaces = info.ancestors.map(ctx.getClassInfo(_)).filter(_.isInterface)
    if (!interfaces.isEmpty) {
      val itablesInit = List(
        I32_CONST(WasmImmediate.I32(ctx.itablesLength)),
        ARRAY_NEW_DEFAULT(WasmImmediate.TypeIdx(WasmArrayType.itables.name))
      )
      val globalITable = WasmGlobal(
        WasmGlobalName.WasmGlobalITableName(clazz.name.name),
        WasmRefType(WasmHeapType.Type(WasmArrayType.itables.name)),
        init = WasmExpr(itablesInit),
        isMutable = false
      )
      ctx.addGlobalITable(clazz.name.name, globalITable)
    }
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
    // val typeName = WasmTypeName.WasmITableTypeName(className)
    val classInfo = ctx.getClassInfo(clazz.className)
    val itableType = WasmStructType(
      Names.WasmTypeName.WasmITableTypeName(className),
      classInfo.methods.map { m =>
        WasmStructField(
          Names.WasmFieldName(m.name.simpleName),
          WasmRefNullType(WasmHeapType.Func(m.toWasmFunctionType().name)),
          isMutable = false
        )
      },
      None
    )
    ctx.addGCType(itableType)
    // typeName
    // genITable
    // generateVTable()
  }

  private def transformModuleClass(clazz: LinkedClass)(implicit ctx: WasmContext) = {
    assert(clazz.kind == ClassKind.ModuleClass)

    val structType = transformClassCommon(clazz)
    val heapType = WasmHeapType.Type(structType.name)

    // global instance
    // (global name (ref null type))
    val global = WasmGlobal(
      Names.WasmGlobalName.WasmModuleInstanceName.fromIR(clazz.name.name),
      WasmRefNullType(heapType),
      WasmExpr(List(REF_NULL(WasmImmediate.HeapType(heapType)))),
      isMutable = true
    )
    ctx.addGlobal(global)

    genLoadModuleFunc(clazz)
  }

  private def transformJSClass(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    assert(clazz.kind.isJSClass)

    // Define the globals holding the Symbols of private fields
    for (fieldDef <- clazz.fields) {
      fieldDef match {
        case IRTrees.FieldDef(flags, name, _, _) if !flags.namespace.isStatic =>
          ctx.addGlobal(
            WasmGlobal(
              WasmGlobalName.WasmGlobalJSPrivateFieldName(name.name),
              WasmAnyRef,
              WasmExpr(List(REF_NULL(HeapType(WasmHeapType.Simple.Any)))),
              isMutable = true
            )
          )
          ctx.addJSPrivateFieldName(name.name)
        case _ =>
          ()
      }
    }

    genCreateJSClassFunction(clazz)

    if (clazz.jsClassCaptures.isEmpty)
      genLoadJSClassFunction(clazz)

    if (clazz.kind == ClassKind.JSModuleClass)
      genLoadJSModuleFunction(clazz)
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
    val ctorBody = ctor.body

    // Compute the pre-super environment
    val preSuperDecls = ctorBody.beforeSuper.collect { case varDef: IRTrees.VarDef =>
      varDef
    }

    // Build the `preSuperStats` function
    val preSuperStatsFun = {
      val preSuperEnvStructType = ctx.getClosureDataStructType(preSuperDecls.map(_.vtpe))
      val preSuperEnvTyp = WasmRefType(WasmHeapType.Type(preSuperEnvStructType.name))

      implicit val fctx = WasmFunctionContext(
        Some(clazz.className),
        WasmFunctionName.preSuperStats(clazz.className),
        Some(jsClassCaptures),
        preSuperVarDefs = None,
        hasNewTarget = true,
        receiverTyp = None,
        ctor.args,
        List(preSuperEnvTyp)
      )

      import fctx.instrs

      WasmExpressionBuilder.generateBlockStats(ctorBody.beforeSuper) {
        // Build and return the preSuperEnv struct
        for (varDef <- preSuperDecls)
          instrs += LOCAL_GET(fctx.lookupLocalAssertLocalStorage(varDef.name.name))
        instrs += STRUCT_NEW(preSuperEnvStructType.name)
      }

      fctx.buildAndAddToContext()
    }

    // Build the `superArgs` function
    val superArgsFun = {
      implicit val fctx = WasmFunctionContext(
        Some(clazz.className),
        WasmFunctionName.superArgs(clazz.className),
        Some(jsClassCaptures),
        Some(preSuperDecls),
        hasNewTarget = true,
        receiverTyp = None,
        ctor.args,
        List(WasmAnyRef) // a js.Array
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
        WasmFunctionName.postSuperStats(clazz.className),
        Some(jsClassCaptures),
        Some(preSuperDecls),
        hasNewTarget = true,
        receiverTyp = Some(WasmAnyRef),
        ctor.args,
        List(WasmAnyRef)
      )

      import fctx.instrs

      // Create fields
      for (fieldDef <- clazz.fields if !fieldDef.flags.namespace.isStatic) {
        // Load instance
        instrs += LOCAL_GET(fctx.receiverStorage.idx)

        // Load name
        fieldDef match {
          case IRTrees.FieldDef(_, name, _, _) =>
            instrs += GLOBAL_GET(
              GlobalIdx(WasmGlobalName.WasmGlobalJSPrivateFieldName(name.name))
            )
          case IRTrees.JSFieldDef(_, nameTree, _) =>
            WasmExpressionBuilder.generateIRBody(nameTree, IRTypes.AnyType)
        }

        // Generate boxed representation of the zero of the field
        WasmExpressionBuilder.generateIRBody(IRTypes.zeroOf(fieldDef.ftpe), IRTypes.AnyType)

        instrs += CALL(FuncIdx(WasmFunctionName.installJSField))
      }

      WasmExpressionBuilder.generateIRBody(
        IRTrees.Block(ctorBody.afterSuper),
        IRTypes.AnyType
      )

      fctx.buildAndAddToContext()
    }

    // Build the actual `createJSClass` function
    val createJSClassFun = {
      implicit val fctx = WasmFunctionContext(
        Some(clazz.className),
        WasmFunctionName.createJSClassOf(clazz.className),
        None,
        None,
        jsClassCaptures,
        List(WasmRefType.any)
      )

      import fctx.instrs

      // Bundle class captures in a capture data struct -- leave it on the stack for createJSClass
      val dataStructType = ctx.getClosureDataStructType(jsClassCaptures.map(_.ptpe))
      val dataStructLocal = fctx.addLocal(
        "__classCaptures",
        WasmRefType(WasmHeapType.Type(dataStructType.name))
      )
      for (cc <- jsClassCaptures)
        instrs += LOCAL_GET(fctx.lookupLocalAssertLocalStorage(cc.name.name))
      instrs += STRUCT_NEW(dataStructType.name)
      instrs += LOCAL_TEE(dataStructLocal)

      /* Load super constructor; specified by
       * https://lampwww.epfl.ch/~doeraene/sjsir-semantics/#sec-sjsir-classdef-runtime-semantics-evaluation
       * - if `jsSuperClass` is defined, evaluate it;
       * - otherwise evaluate `LoadJSConstructor` of the declared superClass.
       */
      val jsSuperClassTree = clazz.jsSuperClass.getOrElse {
        IRTrees.LoadJSConstructor(clazz.superClass.get.name)
      }
      WasmExpressionBuilder.generateIRBody(jsSuperClassTree, IRTypes.AnyType)

      // Load the references to the 3 functions that make up the constructor
      instrs += ctx.refFuncWithDeclaration(preSuperStatsFun.name)
      instrs += ctx.refFuncWithDeclaration(superArgsFun.name)
      instrs += ctx.refFuncWithDeclaration(postSuperStatsFun.name)

      // Call the createJSClass helper to bundle everything
      instrs += CALL(FuncIdx(WasmFunctionName.createJSClass))

      // Store the result, locally and possibly in the global cache
      val jsClassLocal = fctx.addLocal("__jsClass", WasmRefType.any)
      if (clazz.jsClassCaptures.isEmpty) {
        // Static JS class with a global cache
        instrs += LOCAL_TEE(jsClassLocal)
        instrs += GLOBAL_SET(
          GlobalIdx(WasmGlobalName.WasmJSClassName(clazz.className))
        )
      } else {
        // Local or inner JS class, which is new every time
        instrs += LOCAL_SET(jsClassLocal)
      }

      // Install methods and properties
      for (methodOrProp <- clazz.exportedMembers) {
        val isStatic = methodOrProp.flags.namespace.isStatic
        instrs += LOCAL_GET(dataStructLocal)
        instrs += LOCAL_GET(jsClassLocal)
        instrs += I32_CONST(I32(if (isStatic) 1 else 0))

        val receiverTyp = if (isStatic) None else Some(WasmAnyRef)

        methodOrProp match {
          case IRTrees.JSMethodDef(flags, nameTree, params, restParam, body) =>
            WasmExpressionBuilder.generateIRBody(nameTree, IRTypes.AnyType)

            val closureFuncName = fctx.genInnerFuncName()
            locally {
              implicit val fctx: WasmFunctionContext = WasmFunctionContext(
                Some(clazz.className),
                closureFuncName,
                Some(jsClassCaptures),
                receiverTyp,
                params ::: restParam.toList,
                List(WasmAnyRef)
              )
              WasmExpressionBuilder.generateIRBody(body, IRTypes.AnyType)
              fctx.buildAndAddToContext()
            }
            instrs += ctx.refFuncWithDeclaration(closureFuncName)

            instrs += I32_CONST(I32(if (restParam.isDefined) params.size else -1))
            instrs += CALL(FuncIdx(WasmFunctionName.installJSMethod))

          case IRTrees.JSPropertyDef(flags, nameTree, optGetter, optSetter) =>
            WasmExpressionBuilder.generateIRBody(nameTree, IRTypes.AnyType)

            optGetter match {
              case None =>
                instrs += REF_NULL(HeapType(WasmHeapType.Simple.Func))

              case Some(getterBody) =>
                val closureFuncName = fctx.genInnerFuncName()
                locally {
                  implicit val fctx: WasmFunctionContext = WasmFunctionContext(
                    Some(clazz.className),
                    closureFuncName,
                    Some(jsClassCaptures),
                    receiverTyp,
                    Nil,
                    List(WasmAnyRef)
                  )
                  WasmExpressionBuilder.generateIRBody(getterBody, IRTypes.AnyType)
                  fctx.buildAndAddToContext()
                }
                instrs += ctx.refFuncWithDeclaration(closureFuncName)
            }

            optSetter match {
              case None =>
                instrs += REF_NULL(HeapType(WasmHeapType.Simple.Func))

              case Some((setterParamDef, setterBody)) =>
                val closureFuncName = fctx.genInnerFuncName()
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

            instrs += CALL(FuncIdx(WasmFunctionName.installJSProperty))
        }
      }

      // Final result
      instrs += LOCAL_GET(jsClassLocal)

      fctx.buildAndAddToContext()
    }
  }

  private def genLoadJSClassFunction(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    val cachedJSClassGlobal = WasmGlobal(
      WasmGlobalName.WasmJSClassName(clazz.className),
      WasmAnyRef,
      WasmExpr(List(REF_NULL(HeapType(WasmHeapType.Simple.Any)))),
      isMutable = true
    )
    ctx.addGlobal(cachedJSClassGlobal)

    val fctx = WasmFunctionContext(
      Some(clazz.className),
      WasmFunctionName.loadJSClass(clazz.className),
      None,
      Nil,
      List(WasmRefType.any)
    )

    import fctx.instrs

    fctx.block(WasmRefType.any) { doneLabel =>
      // Load cached JS class, return if non-null
      instrs += GLOBAL_GET(GlobalIdx(cachedJSClassGlobal.name))
      instrs += BR_ON_NON_NULL(doneLabel)
      // Otherwise, call createJSClass -- it will also store the class in the cache
      instrs += CALL(FuncIdx(WasmFunctionName.createJSClassOf(clazz.className)))
    }

    fctx.buildAndAddToContext()
  }

  private def genLoadJSModuleFunction(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    val className = clazz.className
    val cacheGlobalName = WasmGlobalName.WasmModuleInstanceName.fromIR(className)

    ctx.addGlobal(
      WasmGlobal(
        cacheGlobalName,
        WasmAnyRef,
        WasmExpr(List(REF_NULL(HeapType(WasmHeapType.Simple.Any)))),
        isMutable = true
      )
    )

    val fctx = WasmFunctionContext(
      WasmFunctionName.loadModule(className),
      Nil,
      List(WasmAnyRef)
    )

    import fctx.instrs

    fctx.block(WasmAnyRef) { doneLabel =>
      // Load cached instance; return if non-null
      instrs += GLOBAL_GET(GlobalIdx(cacheGlobalName))
      instrs += BR_ON_NON_NULL(doneLabel)

      // Get the JS class and instantiate it
      instrs += CALL(FuncIdx(WasmFunctionName.loadJSClass(className)))
      instrs += CALL(FuncIdx(WasmFunctionName.jsNewArray))
      instrs += CALL(FuncIdx(WasmFunctionName.jsNew))

      // Store and return the result
      instrs += GLOBAL_SET(GlobalIdx(cacheGlobalName))
      instrs += GLOBAL_GET(GlobalIdx(cacheGlobalName))
    }

    fctx.buildAndAddToContext()
  }

  private def transformTopLevelMethodExportDef(
      exportDef: IRTrees.TopLevelMethodExportDef
  )(implicit ctx: WasmContext): Unit = {
    val method = exportDef.methodDef
    val exportedName = exportDef.topLevelExportName

    if (method.restParam.isDefined) {
      throw new UnsupportedOperationException(
        s"Top-level export with ...rest param is unsupported at ${method.pos}: $method"
      )
    }

    implicit val fctx = WasmFunctionContext(
      enclosingClassName = None,
      Names.WasmFunctionName.forExport(exportedName),
      receiverTyp = None,
      method.args,
      IRTypes.AnyType
    )

    WasmExpressionBuilder.generateIRBody(method.body, IRTypes.AnyType)

    val func = fctx.buildAndAddToContext()

    ctx.addExport(WasmExport.Function(exportedName, func.name))
  }

  private def transformTopLevelFieldExportDef(
      exportDef: IRTrees.TopLevelFieldExportDef
  )(implicit ctx: WasmContext): Unit = {
    val exprt = WasmExport.Global(
      exportDef.exportName,
      WasmGlobalName.WasmGlobalStaticFieldName(exportDef.field.name)
    )
    ctx.addExport(exprt)
  }

  private def genFunction(
      clazz: LinkedClass,
      method: IRTrees.MethodDef
  )(implicit ctx: WasmContext): WasmFunction = {
    val functionName = Names.WasmFunctionName(
      method.flags.namespace,
      clazz.name.name,
      method.name.name
    )

    // Receiver type for non-constructor methods needs to be `(ref any)` because params are invariant
    // Otherwise, vtable can't be a subtype of the supertype's subtype
    // Constructor can use the exact type because it won't be registered to vtables.
    val receiverTyp =
      if (method.flags.namespace.isStatic)
        None
      else if (clazz.kind == ClassKind.HijackedClass)
        Some(transformType(IRTypes.BoxedClassToPrimType(clazz.name.name)))
      else if (method.flags.namespace.isConstructor)
        Some(WasmRefNullType(WasmHeapType.Type(WasmTypeName.WasmStructTypeName(clazz.name.name))))
      else
        Some(WasmRefType.any)

    // Prepare for function context, set receiver and parameters
    implicit val fctx = WasmFunctionContext(
      Some(clazz.className),
      functionName,
      receiverTyp,
      method.args,
      method.resultType
    )

    // build function body
    val body = method.body.getOrElse(throw new Exception("abstract method cannot be transformed"))
    WasmExpressionBuilder.generateIRBody(body, method.resultType)

    fctx.buildAndAddToContext()
  }

  private def transformField(
      field: IRTrees.FieldDef
  )(implicit ctx: WasmContext): WasmStructField = {
    WasmStructField(
      Names.WasmFieldName(field.name.name),
      transformType(field.ftpe),
      // needs to be mutable even if it's flags.isMutable = false
      // because it's initialized by constructor
      isMutable = true // field.flags.isMutable
    )
  }
}
