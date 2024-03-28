package wasm
package ir2wasm

import wasm4s._
import wasm4s.WasmContext._
import wasm4s.Names._
import wasm4s.Types._
import wasm4s.WasmInstr._
import TypeTransformer._

import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.ClassKind

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

    clazz.kind match {
      case ClassKind.ModuleClass   => transformModuleClass(clazz)
      case ClassKind.Class         => transformClass(clazz)
      case ClassKind.HijackedClass => transformHijackedClass(clazz)
      case ClassKind.Interface     => transformInterface(clazz)
      case _                       => ()
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
    import WasmImmediate._

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
    // gen functions
    clazz.methods.foreach { method =>
      genFunction(clazz, method)
    }

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

    val (gVtable, gItable) = if (!isAbstractClass) {
      // Generate an actual vtable
      val functions = ctx.calculateGlobalVTable(className)
      val vtableElems = functions.map(method => WasmInstr.REF_FUNC(method.name))
      val globalVTable = genTypeDataGlobal(typeRef, vtableType, typeDataFieldValues, vtableElems)
      ctx.addGlobal(globalVTable)

      // Generate class itable
      val globalClassITable = calculateClassITable(clazz)
      globalClassITable.foreach(ctx.addGlobal)

      (globalVTable, globalClassITable)
    } else {
      // Only generate typeData
      val globalTypeData =
        genTypeDataGlobal(typeRef, WasmStructType.typeData, typeDataFieldValues, Nil)
      ctx.addGlobal(globalTypeData)
      (globalTypeData, None)
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
    if (!isAbstractClass)
      genStructNewDefault(classInfo, Some(gVtable), gItable)

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
    import WasmImmediate._
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

  private def genStructNewDefault(
      classInfo: WasmClassInfo,
      vtable: Option[WasmGlobal],
      itable: Option[WasmGlobal]
  )(implicit ctx: WasmContext): Unit = {
    val className = classInfo.name

    val getVTable = vtable match {
      case None =>
        REF_NULL(
          WasmImmediate.HeapType(
            WasmHeapType.Type(WasmTypeName.WasmVTableTypeName(className))
          )
        )
      case Some(v) => GLOBAL_GET(WasmImmediate.GlobalIdx(v.name))
    }
    val getITable = itable match {
      case None => REF_NULL(WasmImmediate.HeapType(WasmHeapType.Type(WasmArrayType.itables.name)))
      case Some(i) => GLOBAL_GET(WasmImmediate.GlobalIdx(i.name))
    }
    val defaultFields =
      getVTable +: getITable +:
        classInfo.allFieldDefs.map { f =>
          val ty = transformType(f.ftpe)
          Defaults.defaultValue(ty)
        }

    val structName = WasmTypeName.WasmStructTypeName(className)
    val body =
      defaultFields :+ STRUCT_NEW(WasmImmediate.TypeIdx(structName))
    val sig =
      WasmFunctionSignature(Nil, List(WasmRefType(WasmHeapType.Type(structName))))
    val newDefaultTypeName = ctx.addFunctionType(sig)
    val func = WasmFunction(
      WasmFunctionName.newDefault(className),
      WasmFunctionType(newDefaultTypeName, sig),
      Nil,
      WasmExpr(body)
    )
    ctx.addFunction(func)
  }

  /** @return
    *   global instance of the class itable
    */
  private def calculateClassITable(
      clazz: LinkedClass
  )(implicit ctx: ReadOnlyWasmContext): Option[WasmGlobal] = {
    val classItables = ctx.calculateClassItables(clazz.name.name)
    if (!classItables.isEmpty) {
      val vtable = ctx.calculateVtableType(clazz.name.name)

      val itablesInit: List[WasmInstr] = classItables.itables.flatMap { iface =>
        iface.methods.map { method =>
          val func = vtable.resolve(method.name)
          REF_FUNC(WasmImmediate.FuncIdx(func.name))
        } :+ STRUCT_NEW(WasmTypeName.WasmITableTypeName(iface.name))
      } ++ List(
        ARRAY_NEW_FIXED(
          WasmImmediate.TypeIdx(WasmArrayType.itables.name),
          WasmImmediate.I32(classItables.itables.size)
        )
      )

      val globalITable = WasmGlobal(
        WasmGlobalName.WasmGlobalITableName(clazz.name.name),
        WasmRefType(WasmHeapType.Type(WasmArrayType.itables.name)),
        init = WasmExpr(itablesInit),
        isMutable = false
      )
      Some(globalITable)
    } else None
  }

  private def transformClass(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    assert(clazz.kind == ClassKind.Class)
    transformClassCommon(clazz)
  }

  private def transformHijackedClass(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    clazz.methods.foreach { method =>
      genFunction(clazz, method)
    }
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

    // Do we need receivers?
    clazz.methods.collect {
      case method if method.body.isDefined =>
        genFunction(clazz, method)
    }
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
