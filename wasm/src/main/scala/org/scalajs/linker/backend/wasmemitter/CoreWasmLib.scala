package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.{JSUnaryOp, JSBinaryOp, MemberNamespace}
import org.scalajs.ir.Types.{Type => _, ArrayType => _, _}
import org.scalajs.ir.{OriginalName, Position}

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.Instructions._
import org.scalajs.linker.backend.webassembly.Identitities._
import org.scalajs.linker.backend.webassembly.Modules._
import org.scalajs.linker.backend.webassembly.Types._

import EmbeddedConstants._
import VarGen._
import TypeTransformer._

object CoreWasmLib {
  import RefType.anyref

  private implicit val noPos: Position = Position.NoPosition

  /** Fields of the `typeData` struct definition.
    *
    * They are accessible as a public list because they must be repeated in every vtable type
    * definition.
    *
    * @see
    *   [[VarGen.genFieldName.typeData]], which contains documentation of what is in each field.
    */
  val typeDataStructFields: List[StructField] = {
    import genFieldID.typeData._
    import RefType.nullable

    def make(id: FieldID, typ: Type, isMutable: Boolean): StructField =
      StructField(id, OriginalName(id.toString()), typ, isMutable)

    List(
      make(nameOffset, Int32, isMutable = false),
      make(nameSize, Int32, isMutable = false),
      make(nameStringIndex, Int32, isMutable = false),
      make(kind, Int32, isMutable = false),
      make(specialInstanceTypes, Int32, isMutable = false),
      make(strictAncestors, nullable(genTypeID.typeDataArray), isMutable = false),
      make(componentType, nullable(genTypeID.typeData), isMutable = false),
      make(name, RefType.anyref, isMutable = true),
      make(classOfValue, nullable(genTypeID.ClassStruct), isMutable = true),
      make(arrayOf, nullable(genTypeID.ObjectVTable), isMutable = true),
      make(cloneFunction, nullable(genTypeID.cloneFunctionType), isMutable = false),
      make(
        isJSClassInstance,
        nullable(genTypeID.isJSClassInstanceFuncType),
        isMutable = false
      ),
      make(
        reflectiveProxies,
        RefType(genTypeID.reflectiveProxies),
        isMutable = false
      )
    )
  }

  /** Generates definitions that must come *before* the code generated for regular classes.
    *
    * This notably includes the `typeData` definitions, since the vtable of `jl.Object` is a subtype
    * of `typeData`.
    */
  def genPreClasses()(implicit ctx: WasmContext): Unit = {
    genPreMainRecTypeDefinitions()
    ctx.moduleBuilder.addRecTypeBuilder(ctx.mainRecType)
    genCoreTypesInRecType()

    genTags()

    genGlobalImports()
    genPrimitiveTypeDataGlobals()

    genHelperImports()
    genHelperDefinitions()
  }

  /** Generates definitions that must come *after* the code generated for regular classes.
    *
    * This notably includes the array class definitions, since they are subtypes of the `jl.Object`
    * struct type.
    */
  def genPostClasses()(implicit ctx: WasmContext): Unit = {
    genArrayClassTypes()

    genBoxedZeroGlobals()
    genArrayClassGlobals()
  }

  private def genPreMainRecTypeDefinitions()(implicit ctx: WasmContext): Unit = {
    val b = ctx.moduleBuilder

    def genUnderlyingArrayType(id: TypeID, elemType: StorageType): Unit =
      b.addRecType(id, OriginalName(id.toString()), ArrayType(FieldType(elemType, true)))

    genUnderlyingArrayType(genTypeID.i8Array, Int8)
    genUnderlyingArrayType(genTypeID.i16Array, Int16)
    genUnderlyingArrayType(genTypeID.i32Array, Int32)
    genUnderlyingArrayType(genTypeID.i64Array, Int64)
    genUnderlyingArrayType(genTypeID.f32Array, Float32)
    genUnderlyingArrayType(genTypeID.f64Array, Float64)
    genUnderlyingArrayType(genTypeID.anyArray, anyref)
  }

  private def genCoreTypesInRecType()(implicit ctx: WasmContext): Unit = {
    def genCoreType(id: TypeID, compositeType: CompositeType): Unit =
      ctx.mainRecType.addSubType(id, OriginalName(id.toString()), compositeType)

    genCoreType(
      genTypeID.cloneFunctionType,
      FunctionType(
        List(RefType(genTypeID.ObjectStruct)),
        List(RefType(genTypeID.ObjectStruct))
      )
    )

    genCoreType(
      genTypeID.isJSClassInstanceFuncType,
      FunctionType(List(RefType.anyref), List(Int32))
    )

    genCoreType(
      genTypeID.typeDataArray,
      ArrayType(FieldType(RefType(genTypeID.typeData), isMutable = false))
    )
    genCoreType(
      genTypeID.itables,
      ArrayType(FieldType(RefType.nullable(HeapType.Struct), isMutable = true))
    )
    genCoreType(
      genTypeID.reflectiveProxies,
      ArrayType(FieldType(RefType(genTypeID.reflectiveProxy), isMutable = false))
    )

    ctx.mainRecType.addSubType(
      SubType(
        genTypeID.typeData,
        OriginalName(genTypeID.typeData.toString()),
        isFinal = false,
        None,
        StructType(typeDataStructFields)
      )
    )

    genCoreType(
      genTypeID.reflectiveProxy,
      StructType(
        List(
          StructField(
            genFieldID.reflectiveProxy.func_name,
            OriginalName(genFieldID.reflectiveProxy.func_name.toString()),
            Int32,
            isMutable = false
          ),
          StructField(
            genFieldID.reflectiveProxy.func_ref,
            OriginalName(genFieldID.reflectiveProxy.func_ref.toString()),
            RefType(HeapType.Func),
            isMutable = false
          )
        )
      )
    )
  }

  private def genArrayClassTypes()(implicit ctx: WasmContext): Unit = {
    // The vtable type is always the same as j.l.Object
    val vtableTypeName = genTypeID.ObjectVTable
    val vtableField = StructField(
      genFieldID.objStruct.vtable,
      OriginalName(genFieldID.objStruct.vtable.toString()),
      RefType(vtableTypeName),
      isMutable = false
    )
    val itablesField = StructField(
      genFieldID.objStruct.itables,
      OriginalName(genFieldID.objStruct.itables.toString()),
      RefType.nullable(genTypeID.itables),
      isMutable = false
    )

    val typeRefsWithArrays: List[(TypeID, TypeID)] =
      List(
        (genTypeID.BooleanArray, genTypeID.i8Array),
        (genTypeID.CharArray, genTypeID.i16Array),
        (genTypeID.ByteArray, genTypeID.i8Array),
        (genTypeID.ShortArray, genTypeID.i16Array),
        (genTypeID.IntArray, genTypeID.i32Array),
        (genTypeID.LongArray, genTypeID.i64Array),
        (genTypeID.FloatArray, genTypeID.f32Array),
        (genTypeID.DoubleArray, genTypeID.f64Array),
        (genTypeID.ObjectArray, genTypeID.anyArray)
      )

    for ((structTypeName, underlyingArrayTypeName) <- typeRefsWithArrays) {
      val origName = OriginalName(structTypeName.toString())

      val underlyingArrayField = StructField(
        genFieldID.objStruct.arrayUnderlying,
        OriginalName(genFieldID.objStruct.arrayUnderlying.toString()),
        RefType(underlyingArrayTypeName),
        isMutable = false
      )

      val superType = genTypeID.ObjectStruct
      val structType = StructType(
        List(vtableField, itablesField, underlyingArrayField)
      )
      val subType = SubType(structTypeName, origName, isFinal = true, Some(superType), structType)
      ctx.mainRecType.addSubType(subType)
    }
  }

  private def genTags()(implicit ctx: WasmContext): Unit = {
    val exceptionSig = FunctionType(List(RefType.externref), Nil)
    val typeName = ctx.moduleBuilder.functionTypeToTypeName(exceptionSig)
    ctx.moduleBuilder.addImport(
      Import(
        "__scalaJSHelpers",
        "JSTag",
        ImportDesc.Tag(
          genTagID.exception,
          OriginalName(genTagID.exception.toString()),
          typeName
        )
      )
    )
  }

  private def genGlobalImports()(implicit ctx: WasmContext): Unit = {
    def addGlobalHelperImport(
        id: genGlobalID.JSHelperGlobalID,
        typ: Type,
        isMutable: Boolean
    ): Unit = {
      ctx.moduleBuilder.addImport(
        Import(
          "__scalaJSHelpers",
          id.toString(), // import name, guaranteed by JSHelperGlobalID
          ImportDesc.Global(id, OriginalName(id.toString()), typ, isMutable)
        )
      )
    }

    addGlobalHelperImport(genGlobalID.undef, RefType.any, isMutable = false)
    addGlobalHelperImport(genGlobalID.bFalse, RefType.any, isMutable = false)
    addGlobalHelperImport(genGlobalID.bZero, RefType.any, isMutable = false)
    addGlobalHelperImport(genGlobalID.emptyString, RefType.any, isMutable = false)
    addGlobalHelperImport(genGlobalID.idHashCodeMap, RefType.extern, isMutable = false)
  }

  private def genPrimitiveTypeDataGlobals()(implicit ctx: WasmContext): Unit = {
    import genFieldID.typeData._

    val primRefsWithTypeData = List(
      VoidRef -> KindVoid,
      BooleanRef -> KindBoolean,
      CharRef -> KindChar,
      ByteRef -> KindByte,
      ShortRef -> KindShort,
      IntRef -> KindInt,
      LongRef -> KindLong,
      FloatRef -> KindFloat,
      DoubleRef -> KindDouble
    )

    val typeDataTypeName = genTypeID.typeData

    // Other than `name` and `kind`, all the fields have the same value for all primitives
    val commonFieldValues = List(
      // specialInstanceTypes
      I32Const(0),
      // strictAncestors
      RefNull(HeapType.None),
      // componentType
      RefNull(HeapType.None),
      // name - initially `null`; filled in by the `typeDataName` helper
      RefNull(HeapType.None),
      // the classOf instance - initially `null`; filled in by the `createClassOf` helper
      RefNull(HeapType.None),
      // arrayOf, the typeData of an array of this type - initially `null`; filled in by the `arrayTypeData` helper
      RefNull(HeapType.None),
      // cloneFunction
      RefNull(HeapType.NoFunc),
      // isJSClassInstance
      RefNull(HeapType.NoFunc),
      // reflectiveProxies
      ArrayNewFixed(genTypeID.reflectiveProxies, 0)
    )

    for ((primRef, kind) <- primRefsWithTypeData) {
      val nameDataValue: List[Instr] =
        ctx.getConstantStringDataInstr(primRef.displayName)

      val instrs: List[Instr] = {
        nameDataValue ::: I32Const(kind) :: commonFieldValues :::
          StructNew(genTypeID.typeData) :: Nil
      }

      ctx.addGlobal(
        Global(
          genGlobalID.forVTable(primRef),
          OriginalName("d." + primRef.charCode),
          RefType(genTypeID.typeData),
          Expr(instrs),
          isMutable = false
        )
      )
    }
  }

  private def genBoxedZeroGlobals()(implicit ctx: WasmContext): Unit = {
    val primTypesWithBoxClasses: List[(GlobalID, ClassName, Instr)] = List(
      (genGlobalID.bZeroChar, SpecialNames.CharBoxClass, I32Const(0)),
      (genGlobalID.bZeroLong, SpecialNames.LongBoxClass, I64Const(0))
    )

    for ((globalName, boxClassName, zeroValueInstr) <- primTypesWithBoxClasses) {
      val boxStruct = genTypeID.forClass(boxClassName)
      val instrs: List[Instr] = List(
        GlobalGet(genGlobalID.forVTable(boxClassName)),
        GlobalGet(genGlobalID.forITable(boxClassName)),
        zeroValueInstr,
        StructNew(boxStruct)
      )

      ctx.addGlobal(
        Global(
          globalName,
          OriginalName(globalName.toString()),
          RefType(boxStruct),
          Expr(instrs),
          isMutable = false
        )
      )
    }
  }

  private def genArrayClassGlobals()(implicit ctx: WasmContext): Unit = {
    // Common itable global for all array classes
    val itablesInit = List(
      I32Const(ctx.itablesLength),
      ArrayNewDefault(genTypeID.itables)
    )
    ctx.addGlobal(
      Global(
        genGlobalID.arrayClassITable,
        OriginalName(genGlobalID.arrayClassITable.toString()),
        RefType(genTypeID.itables),
        init = Expr(itablesInit),
        isMutable = false
      )
    )
  }

  private def genHelperImports()(implicit ctx: WasmContext): Unit = {
    import RefType.anyref

    def addHelperImport(
        id: genFunctionID.JSHelperFunctionID,
        params: List[Type],
        results: List[Type]
    ): Unit = {
      val sig = FunctionType(params, results)
      val typeName = ctx.moduleBuilder.functionTypeToTypeName(sig)
      ctx.moduleBuilder.addImport(
        Import(
          "__scalaJSHelpers",
          id.toString(), // import name, guaranteed by JSHelperFunctionID
          ImportDesc.Func(id, OriginalName(id.toString()), typeName)
        )
      )
    }

    addHelperImport(genFunctionID.is, List(anyref, anyref), List(Int32))

    addHelperImport(genFunctionID.isUndef, List(anyref), List(Int32))

    for (primRef <- List(BooleanRef, ByteRef, ShortRef, IntRef, FloatRef, DoubleRef)) {
      val wasmType = primRef match {
        case FloatRef  => Float32
        case DoubleRef => Float64
        case _         => Int32
      }
      addHelperImport(genFunctionID.box(primRef), List(wasmType), List(anyref))
      addHelperImport(genFunctionID.unbox(primRef), List(anyref), List(wasmType))
      addHelperImport(genFunctionID.typeTest(primRef), List(anyref), List(Int32))
    }

    addHelperImport(genFunctionID.fmod, List(Float64, Float64), List(Float64))

    addHelperImport(
      genFunctionID.closure,
      List(RefType.func, anyref),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionID.closureThis,
      List(RefType.func, anyref),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionID.closureRest,
      List(RefType.func, anyref, Int32),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionID.closureThisRest,
      List(RefType.func, anyref, Int32),
      List(RefType.any)
    )

    addHelperImport(genFunctionID.makeExportedDef, List(RefType.func), List(RefType.any))
    addHelperImport(
      genFunctionID.makeExportedDefRest,
      List(RefType.func, Int32),
      List(RefType.any)
    )

    addHelperImport(genFunctionID.stringLength, List(RefType.any), List(Int32))
    addHelperImport(genFunctionID.stringCharAt, List(RefType.any, Int32), List(Int32))
    addHelperImport(genFunctionID.jsValueToString, List(RefType.any), List(RefType.any))
    addHelperImport(genFunctionID.jsValueToStringForConcat, List(anyref), List(RefType.any))
    addHelperImport(genFunctionID.booleanToString, List(Int32), List(RefType.any))
    addHelperImport(genFunctionID.charToString, List(Int32), List(RefType.any))
    addHelperImport(genFunctionID.intToString, List(Int32), List(RefType.any))
    addHelperImport(genFunctionID.longToString, List(Int64), List(RefType.any))
    addHelperImport(genFunctionID.doubleToString, List(Float64), List(RefType.any))
    addHelperImport(
      genFunctionID.stringConcat,
      List(RefType.any, RefType.any),
      List(RefType.any)
    )
    addHelperImport(genFunctionID.isString, List(anyref), List(Int32))

    addHelperImport(genFunctionID.jsValueType, List(RefType.any), List(Int32))
    addHelperImport(genFunctionID.bigintHashCode, List(RefType.any), List(Int32))
    addHelperImport(
      genFunctionID.symbolDescription,
      List(RefType.any),
      List(RefType.anyref)
    )
    addHelperImport(
      genFunctionID.idHashCodeGet,
      List(RefType.extern, RefType.any),
      List(Int32)
    )
    addHelperImport(
      genFunctionID.idHashCodeSet,
      List(RefType.extern, RefType.any, Int32),
      Nil
    )

    addHelperImport(genFunctionID.jsGlobalRefGet, List(RefType.any), List(anyref))
    addHelperImport(genFunctionID.jsGlobalRefSet, List(RefType.any, anyref), Nil)
    addHelperImport(genFunctionID.jsGlobalRefTypeof, List(RefType.any), List(RefType.any))
    addHelperImport(genFunctionID.jsNewArray, Nil, List(anyref))
    addHelperImport(genFunctionID.jsArrayPush, List(anyref, anyref), List(anyref))
    addHelperImport(
      genFunctionID.jsArraySpreadPush,
      List(anyref, anyref),
      List(anyref)
    )
    addHelperImport(genFunctionID.jsNewObject, Nil, List(anyref))
    addHelperImport(
      genFunctionID.jsObjectPush,
      List(anyref, anyref, anyref),
      List(anyref)
    )
    addHelperImport(genFunctionID.jsSelect, List(anyref, anyref), List(anyref))
    addHelperImport(genFunctionID.jsSelectSet, List(anyref, anyref, anyref), Nil)
    addHelperImport(genFunctionID.jsNew, List(anyref, anyref), List(anyref))
    addHelperImport(genFunctionID.jsFunctionApply, List(anyref, anyref), List(anyref))
    addHelperImport(
      genFunctionID.jsMethodApply,
      List(anyref, anyref, anyref),
      List(anyref)
    )
    addHelperImport(genFunctionID.jsImportCall, List(anyref), List(anyref))
    addHelperImport(genFunctionID.jsImportMeta, Nil, List(anyref))
    addHelperImport(genFunctionID.jsDelete, List(anyref, anyref), Nil)
    addHelperImport(genFunctionID.jsForInSimple, List(anyref, anyref), Nil)
    addHelperImport(genFunctionID.jsIsTruthy, List(anyref), List(Int32))
    addHelperImport(genFunctionID.jsLinkingInfo, Nil, List(anyref))

    for ((op, name) <- genFunctionID.jsUnaryOps)
      addHelperImport(name, List(anyref), List(anyref))

    for ((op, name) <- genFunctionID.jsBinaryOps) {
      val resultType =
        if (op == JSBinaryOp.=== || op == JSBinaryOp.!==) Int32
        else anyref
      addHelperImport(name, List(anyref, anyref), List(resultType))
    }

    addHelperImport(genFunctionID.newSymbol, Nil, List(anyref))
    addHelperImport(
      genFunctionID.createJSClass,
      List(anyref, anyref, RefType.func, RefType.func, RefType.func, anyref),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionID.createJSClassRest,
      List(anyref, anyref, RefType.func, RefType.func, RefType.func, anyref, Int32),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionID.installJSField,
      List(anyref, anyref, anyref),
      Nil
    )
    addHelperImport(
      genFunctionID.installJSMethod,
      List(anyref, anyref, anyref, RefType.func, Int32),
      Nil
    )
    addHelperImport(
      genFunctionID.installJSStaticMethod,
      List(anyref, anyref, anyref, RefType.func, Int32),
      Nil
    )
    addHelperImport(
      genFunctionID.installJSProperty,
      List(anyref, anyref, anyref, RefType.funcref, RefType.funcref),
      Nil
    )
    addHelperImport(
      genFunctionID.installJSStaticProperty,
      List(anyref, anyref, anyref, RefType.funcref, RefType.funcref),
      Nil
    )
    addHelperImport(
      genFunctionID.jsSuperGet,
      List(anyref, anyref, anyref),
      List(anyref)
    )
    addHelperImport(
      genFunctionID.jsSuperSet,
      List(anyref, anyref, anyref, anyref),
      Nil
    )
    addHelperImport(
      genFunctionID.jsSuperCall,
      List(anyref, anyref, anyref, anyref),
      List(anyref)
    )
  }

  /** Generates all the non-type definitions of the core Wasm lib. */
  private def genHelperDefinitions()(implicit ctx: WasmContext): Unit = {
    genStringLiteral()
    genCreateStringFromData()
    genTypeDataName()
    genCreateClassOf()
    genGetClassOf()
    genArrayTypeData()
    genIsInstance()
    genIsAssignableFromExternal()
    genIsAssignableFrom()
    genCheckCast()
    genGetComponentType()
    genNewArrayOfThisClass()
    genAnyGetClass()
    genNewArrayObject()
    genIdentityHashCode()
    genSearchReflectiveProxy()
    genArrayCloneFunctions()
  }

  private def newFunctionBuilder(functionID: FunctionID, originalName: OriginalName)(implicit
      ctx: WasmContext
  ): FunctionBuilder = {
    new FunctionBuilder(ctx.moduleBuilder, functionID, originalName, noPos)
  }

  private def newFunctionBuilder(functionID: FunctionID)(implicit
      ctx: WasmContext
  ): FunctionBuilder = {
    newFunctionBuilder(functionID, OriginalName(functionID.toString()))
  }

  private def genStringLiteral()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.stringLiteral)
    val offsetParam = fb.addParam("offset", Int32)
    val sizeParam = fb.addParam("size", Int32)
    val stringIndexParam = fb.addParam("stringIndex", Int32)
    fb.setResultType(RefType.any)

    val str = fb.addLocal("str", RefType.any)

    val instrs = fb

    instrs.block(RefType.any) { cacheHit =>
      instrs += GlobalGet(genGlobalID.stringLiteralCache)
      instrs += LocalGet(stringIndexParam)
      instrs += ArrayGet(genTypeID.anyArray)

      instrs += BrOnNonNull(cacheHit)

      // cache miss, create a new string and cache it
      instrs += GlobalGet(genGlobalID.stringLiteralCache)
      instrs += LocalGet(stringIndexParam)

      instrs += LocalGet(offsetParam)
      instrs += LocalGet(sizeParam)
      instrs += ArrayNewData(genTypeID.i16Array, genDataID.string)
      instrs += Call(genFunctionID.createStringFromData)
      instrs += LocalTee(str)
      instrs += ArraySet(genTypeID.anyArray)

      instrs += LocalGet(str)
    }

    fb.buildAndAddToModule()
  }

  /** `createStringFromData: (ref array u16) -> (ref any)` (representing a `string`). */
  private def genCreateStringFromData()(implicit ctx: WasmContext): Unit = {
    val dataType = RefType(genTypeID.i16Array)

    val fb = newFunctionBuilder(genFunctionID.createStringFromData)
    val dataParam = fb.addParam("data", dataType)
    fb.setResultType(RefType.any)

    val instrs = fb

    val lenLocal = fb.addLocal("len", Int32)
    val iLocal = fb.addLocal("i", Int32)
    val resultLocal = fb.addLocal("result", RefType.any)

    // len := data.length
    instrs += LocalGet(dataParam)
    instrs += ArrayLen
    instrs += LocalSet(lenLocal)

    // i := 0
    instrs += I32Const(0)
    instrs += LocalSet(iLocal)

    // result := ""
    instrs += GlobalGet(genGlobalID.emptyString)
    instrs += LocalSet(resultLocal)

    instrs.loop() { labelLoop =>
      // if i == len
      instrs += LocalGet(iLocal)
      instrs += LocalGet(lenLocal)
      instrs += I32Eq
      instrs.ifThen() {
        // then return result
        instrs += LocalGet(resultLocal)
        instrs += Return
      }

      // result := concat(result, charToString(data(i)))
      instrs += LocalGet(resultLocal)
      instrs += LocalGet(dataParam)
      instrs += LocalGet(iLocal)
      instrs += ArrayGetU(genTypeID.i16Array)
      instrs += Call(genFunctionID.charToString)
      instrs += Call(genFunctionID.stringConcat)
      instrs += LocalSet(resultLocal)

      // i := i - 1
      instrs += LocalGet(iLocal)
      instrs += I32Const(1)
      instrs += I32Add
      instrs += LocalSet(iLocal)

      // loop back to the beginning
      instrs += Br(labelLoop)
    } // end loop $loop
    instrs += Unreachable

    fb.buildAndAddToModule()
  }

  /** `typeDataName: (ref typeData) -> (ref any)` (representing a `string`).
    *
    * Initializes the `name` field of the given `typeData` if that was not done yet, and returns its
    * value.
    *
    * The computed value is specified by `java.lang.Class.getName()`. See also the documentation on
    * [[Names.StructFieldIdx.typeData.name]] for details.
    *
    * @see
    *   [[https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Class.html#getName()]]
    */
  private def genTypeDataName()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)
    val nameDataType = RefType(genTypeID.i16Array)

    val fb = newFunctionBuilder(genFunctionID.typeDataName)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(RefType.any)

    val instrs = fb

    val componentTypeDataLocal = fb.addLocal("componentTypeData", typeDataType)
    val componentNameDataLocal = fb.addLocal("componentNameData", nameDataType)
    val firstCharLocal = fb.addLocal("firstChar", Int32)
    val nameLocal = fb.addLocal("name", RefType.any)

    instrs.block(RefType.any) { alreadyInitializedLabel =>
      // br_on_non_null $alreadyInitialized typeData.name
      instrs += LocalGet(typeDataParam)
      instrs += StructGet(genTypeID.typeData, genFieldID.typeData.name)
      instrs += BrOnNonNull(alreadyInitializedLabel)

      // for the STRUCT_SET typeData.name near the end
      instrs += LocalGet(typeDataParam)

      // if typeData.kind == KindArray
      instrs += LocalGet(typeDataParam)
      instrs += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
      instrs += I32Const(KindArray)
      instrs += I32Eq
      instrs.ifThenElse(RefType.any) {
        // it is an array; compute its name from the component type name

        // <top of stack> := "[", for the CALL to stringConcat near the end
        instrs += I32Const('['.toInt)
        instrs += Call(genFunctionID.charToString)

        // componentTypeData := ref_as_non_null(typeData.componentType)
        instrs += LocalGet(typeDataParam)
        instrs += StructGet(
          genTypeID.typeData,
          genFieldID.typeData.componentType
        )
        instrs += RefAsNotNull
        instrs += LocalSet(componentTypeDataLocal)

        // switch (componentTypeData.kind)
        // the result of this switch is the string that must come after "["
        instrs.switch(RefType.any) { () =>
          // scrutinee
          instrs += LocalGet(componentTypeDataLocal)
          instrs += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
        }(
          List(KindBoolean) -> { () =>
            instrs += I32Const('Z'.toInt)
            instrs += Call(genFunctionID.charToString)
          },
          List(KindChar) -> { () =>
            instrs += I32Const('C'.toInt)
            instrs += Call(genFunctionID.charToString)
          },
          List(KindByte) -> { () =>
            instrs += I32Const('B'.toInt)
            instrs += Call(genFunctionID.charToString)
          },
          List(KindShort) -> { () =>
            instrs += I32Const('S'.toInt)
            instrs += Call(genFunctionID.charToString)
          },
          List(KindInt) -> { () =>
            instrs += I32Const('I'.toInt)
            instrs += Call(genFunctionID.charToString)
          },
          List(KindLong) -> { () =>
            instrs += I32Const('J'.toInt)
            instrs += Call(genFunctionID.charToString)
          },
          List(KindFloat) -> { () =>
            instrs += I32Const('F'.toInt)
            instrs += Call(genFunctionID.charToString)
          },
          List(KindDouble) -> { () =>
            instrs += I32Const('D'.toInt)
            instrs += Call(genFunctionID.charToString)
          },
          List(KindArray) -> { () =>
            // the component type is an array; get its own name
            instrs += LocalGet(componentTypeDataLocal)
            instrs += Call(genFunctionID.typeDataName)
          }
        ) { () =>
          // default: the component type is neither a primitive nor an array;
          // concatenate "L" + <its own name> + ";"
          instrs += I32Const('L'.toInt)
          instrs += Call(genFunctionID.charToString)
          instrs += LocalGet(componentTypeDataLocal)
          instrs += Call(genFunctionID.typeDataName)
          instrs += Call(genFunctionID.stringConcat)
          instrs += I32Const(';'.toInt)
          instrs += Call(genFunctionID.charToString)
          instrs += Call(genFunctionID.stringConcat)
        }

        // At this point, the stack contains "[" and the string that must be concatenated with it
        instrs += Call(genFunctionID.stringConcat)
      } {
        // it is not an array; its name is stored in nameData
        for (
          idx <- List(
            genFieldID.typeData.nameOffset,
            genFieldID.typeData.nameSize,
            genFieldID.typeData.nameStringIndex
          )
        ) {
          instrs += LocalGet(typeDataParam)
          instrs += StructGet(genTypeID.typeData, idx)
        }
        instrs += Call(genFunctionID.stringLiteral)
      }

      // typeData.name := <top of stack> ; leave it on the stack
      instrs += LocalTee(nameLocal)
      instrs += StructSet(genTypeID.typeData, genFieldID.typeData.name)
      instrs += LocalGet(nameLocal)
    }

    fb.buildAndAddToModule()
  }

  /** `createClassOf: (ref typeData) -> (ref jlClass)`.
    *
    * Creates the unique `java.lang.Class` instance associated with the given `typeData`, stores it
    * in its `classOfValue` field, and returns it.
    *
    * Must be called only if the `classOfValue` of the typeData is null. All call sites must deal
    * with the non-null case as a fast-path.
    */
  private def genCreateClassOf()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.createClassOf)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(RefType(genTypeID.ClassStruct))

    val instrs = fb

    val classInstanceLocal = fb.addLocal("classInstance", RefType(genTypeID.ClassStruct))

    // classInstance := newDefault$java.lang.Class()
    // leave it on the stack for the constructor call
    instrs += Call(genFunctionID.newDefault(ClassClass))
    instrs += LocalTee(classInstanceLocal)

    /* The JS object containing metadata to pass as argument to the `jl.Class` constructor.
     * Specified by https://lampwww.epfl.ch/~doeraene/sjsir-semantics/#sec-sjsir-createclassdataof
     * Leave it on the stack.
     */
    instrs += Call(genFunctionID.jsNewObject)
    // "__typeData": typeData (TODO hide this better? although nobody will notice anyway)
    instrs ++= ctx.getConstantStringInstr("__typeData")
    instrs += LocalGet(typeDataParam)
    instrs += Call(genFunctionID.jsObjectPush)
    // "name": typeDataName(typeData)
    instrs ++= ctx.getConstantStringInstr("name")
    instrs += LocalGet(typeDataParam)
    instrs += Call(genFunctionID.typeDataName)
    instrs += Call(genFunctionID.jsObjectPush)
    // "isPrimitive": (typeData.kind <= KindLastPrimitive)
    instrs ++= ctx.getConstantStringInstr("isPrimitive")
    instrs += LocalGet(typeDataParam)
    instrs += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
    instrs += I32Const(KindLastPrimitive)
    instrs += I32LeU
    instrs += Call(genFunctionID.box(BooleanRef))
    instrs += Call(genFunctionID.jsObjectPush)
    // "isArrayClass": (typeData.kind == KindArray)
    instrs ++= ctx.getConstantStringInstr("isArrayClass")
    instrs += LocalGet(typeDataParam)
    instrs += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
    instrs += I32Const(KindArray)
    instrs += I32Eq
    instrs += Call(genFunctionID.box(BooleanRef))
    instrs += Call(genFunctionID.jsObjectPush)
    // "isInterface": (typeData.kind == KindInterface)
    instrs ++= ctx.getConstantStringInstr("isInterface")
    instrs += LocalGet(typeDataParam)
    instrs += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
    instrs += I32Const(KindInterface)
    instrs += I32Eq
    instrs += Call(genFunctionID.box(BooleanRef))
    instrs += Call(genFunctionID.jsObjectPush)
    // "isInstance": closure(isInstance, typeData)
    instrs ++= ctx.getConstantStringInstr("isInstance")
    instrs += ctx.refFuncWithDeclaration(genFunctionID.isInstance)
    instrs += LocalGet(typeDataParam)
    instrs += Call(genFunctionID.closure)
    instrs += Call(genFunctionID.jsObjectPush)
    // "isAssignableFrom": closure(isAssignableFrom, typeData)
    instrs ++= ctx.getConstantStringInstr("isAssignableFrom")
    instrs += ctx.refFuncWithDeclaration(genFunctionID.isAssignableFromExternal)
    instrs += LocalGet(typeDataParam)
    instrs += Call(genFunctionID.closure)
    instrs += Call(genFunctionID.jsObjectPush)
    // "checkCast": closure(checkCast, typeData)
    instrs ++= ctx.getConstantStringInstr("checkCast")
    instrs += ctx.refFuncWithDeclaration(genFunctionID.checkCast)
    instrs += LocalGet(typeDataParam)
    instrs += Call(genFunctionID.closure)
    instrs += Call(genFunctionID.jsObjectPush)
    // "isAssignableFrom": closure(isAssignableFrom, typeData)
    // "getComponentType": closure(getComponentType, typeData)
    instrs ++= ctx.getConstantStringInstr("getComponentType")
    instrs += ctx.refFuncWithDeclaration(genFunctionID.getComponentType)
    instrs += LocalGet(typeDataParam)
    instrs += Call(genFunctionID.closure)
    instrs += Call(genFunctionID.jsObjectPush)
    // "newArrayOfThisClass": closure(newArrayOfThisClass, typeData)
    instrs ++= ctx.getConstantStringInstr("newArrayOfThisClass")
    instrs += ctx.refFuncWithDeclaration(genFunctionID.newArrayOfThisClass)
    instrs += LocalGet(typeDataParam)
    instrs += Call(genFunctionID.closure)
    instrs += Call(genFunctionID.jsObjectPush)

    // Call java.lang.Class::<init>(dataObject)
    instrs += Call(
      genFunctionID.forMethod(
        MemberNamespace.Constructor,
        ClassClass,
        SpecialNames.ClassCtor
      )
    )

    // typeData.classOfValue := classInstance
    instrs += LocalGet(typeDataParam)
    instrs += LocalGet(classInstanceLocal)
    instrs += StructSet(genTypeID.typeData, genFieldID.typeData.classOfValue)

    // <top-of-stack> := classInstance for the implicit return
    instrs += LocalGet(classInstanceLocal)

    fb.buildAndAddToModule()
  }

  /** `getClassOf: (ref typeData) -> (ref jlClass)`.
    *
    * Initializes the `java.lang.Class` instance associated with the given `typeData` if not already
    * done, and returns it.
    *
    * This includes the fast-path and the slow-path to `createClassOf`, for call sites that are not
    * performance-sensitive.
    */
  private def genGetClassOf()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.getClassOf)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(RefType(genTypeID.ClassStruct))

    val instrs = fb

    instrs.block(RefType(genTypeID.ClassStruct)) { alreadyInitializedLabel =>
      // fast path
      instrs += LocalGet(typeDataParam)
      instrs += StructGet(genTypeID.typeData, genFieldID.typeData.classOfValue)
      instrs += BrOnNonNull(alreadyInitializedLabel)
      // slow path
      instrs += LocalGet(typeDataParam)
      instrs += Call(genFunctionID.createClassOf)
    } // end bock alreadyInitializedLabel

    fb.buildAndAddToModule()
  }

  /** `arrayTypeData: (ref typeData), i32 -> (ref $java.lang.Object___vtable)`.
    *
    * Returns the typeData/vtable of an array with `dims` dimensions over the given typeData. `dims`
    * must be be strictly positive.
    */
  private def genArrayTypeData()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)
    val objectVTableType = RefType(genTypeID.ObjectVTable)

    /* Array classes extend Cloneable, Serializable and Object.
     * Filter out the ones that do not have run-time type info at all, as
     * we do for other classes.
     */
    val strictAncestors =
      List(CloneableClass, SerializableClass, ObjectClass)
        .filter(name => ctx.getClassInfoOption(name).exists(_.hasRuntimeTypeInfo))

    val fb = newFunctionBuilder(genFunctionID.arrayTypeData)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val dimsParam = fb.addParam("dims", Int32)
    fb.setResultType(objectVTableType)

    val instrs = fb

    val arrayTypeDataLocal = fb.addLocal("arrayTypeData", objectVTableType)

    instrs.loop() { loopLabel =>
      instrs.block(objectVTableType) { arrayOfIsNonNullLabel =>
        // br_on_non_null $arrayOfIsNonNull typeData.arrayOf
        instrs += LocalGet(typeDataParam)
        instrs += StructGet(
          genTypeID.typeData,
          genFieldID.typeData.arrayOf
        )
        instrs += BrOnNonNull(arrayOfIsNonNullLabel)

        // <top-of-stack> := typeData ; for the <old typeData>.arrayOf := ... later on
        instrs += LocalGet(typeDataParam)

        // typeData := new typeData(...)
        instrs += I32Const(0) // nameOffset
        instrs += I32Const(0) // nameSize
        instrs += I32Const(0) // nameStringIndex
        instrs += I32Const(KindArray) // kind = KindArray
        instrs += I32Const(0) // specialInstanceTypes = 0

        // strictAncestors
        for (strictAncestor <- strictAncestors)
          instrs += GlobalGet(genGlobalID.forVTable(strictAncestor))
        instrs += ArrayNewFixed(
          genTypeID.typeDataArray,
          strictAncestors.size
        )

        instrs += LocalGet(typeDataParam) // componentType
        instrs += RefNull(HeapType.None) // name
        instrs += RefNull(HeapType.None) // classOf
        instrs += RefNull(HeapType.None) // arrayOf

        // clone
        instrs.switch(RefType(genTypeID.cloneFunctionType)) { () =>
          instrs += LocalGet(typeDataParam)
          instrs += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
        }(
          List(KindBoolean) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionID.clone(BooleanRef))
          },
          List(KindChar) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionID.clone(CharRef))
          },
          List(KindByte) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionID.clone(ByteRef))
          },
          List(KindShort) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionID.clone(ShortRef))
          },
          List(KindInt) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionID.clone(IntRef))
          },
          List(KindLong) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionID.clone(LongRef))
          },
          List(KindFloat) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionID.clone(FloatRef))
          },
          List(KindDouble) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionID.clone(DoubleRef))
          }
        ) { () =>
          instrs += ctx.refFuncWithDeclaration(
            genFunctionID.clone(ClassRef(ObjectClass))
          )
        }

        // isJSClassInstance
        instrs += RefNull(HeapType.NoFunc)

        // reflectiveProxies
        instrs += ArrayNewFixed(genTypeID.reflectiveProxies, 0) // TODO

        val objectClassInfo = ctx.getClassInfo(ObjectClass)
        instrs ++= objectClassInfo.tableEntries.map { methodName =>
          ctx.refFuncWithDeclaration(objectClassInfo.resolvedMethodInfos(methodName).tableEntryName)
        }
        instrs += StructNew(genTypeID.ObjectVTable)
        instrs += LocalTee(arrayTypeDataLocal)

        // <old typeData>.arrayOf := typeData
        instrs += StructSet(genTypeID.typeData, genFieldID.typeData.arrayOf)

        // put arrayTypeData back on the stack
        instrs += LocalGet(arrayTypeDataLocal)
      } // end block $arrayOfIsNonNullLabel

      // dims := dims - 1 -- leave dims on the stack
      instrs += LocalGet(dimsParam)
      instrs += I32Const(1)
      instrs += I32Sub
      instrs += LocalTee(dimsParam)

      // if dims == 0 then
      //   return typeData.arrayOf (which is on the stack)
      instrs += I32Eqz
      instrs.ifThen(FunctionType(List(objectVTableType), List(objectVTableType))) {
        instrs += Return
      }

      // typeData := typeData.arrayOf (which is on the stack), then loop back to the beginning
      instrs += LocalSet(typeDataParam)
      instrs += Br(loopLabel)
    } // end loop $loop
    instrs += Unreachable

    fb.buildAndAddToModule()
  }

  /** `isInstance: (ref typeData), anyref -> i32` (a boolean).
    *
    * Tests whether the given value is a non-null instance of the given type.
    *
    * Specified by `"isInstance"` at
    * [[https://lampwww.epfl.ch/~doeraene/sjsir-semantics/#sec-sjsir-createclassdataof]].
    */
  private def genIsInstance()(implicit ctx: WasmContext): Unit = {
    import genFieldID.typeData._

    val typeDataType = RefType(genTypeID.typeData)
    val objectRefType = RefType(genTypeID.forClass(ObjectClass))

    val fb = newFunctionBuilder(genFunctionID.isInstance)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val valueParam = fb.addParam("value", RefType.anyref)
    fb.setResultType(Int32)

    val instrs = fb

    val valueNonNullLocal = fb.addLocal("valueNonNull", RefType.any)
    val specialInstanceTypesLocal = fb.addLocal("specialInstanceTypes", Int32)

    // switch (typeData.kind)
    instrs.switch(Int32) { () =>
      instrs += LocalGet(typeDataParam)
      instrs += StructGet(genTypeID.typeData, kind)
    }(
      // case anyPrimitiveKind => false
      (KindVoid to KindLastPrimitive).toList -> { () =>
        instrs += I32Const(0)
      },
      // case KindObject => value ne null
      List(KindObject) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += RefIsNull
        instrs += I32Eqz
      },
      // for each boxed class, the corresponding primitive type test
      List(KindBoxedUnit) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionID.isUndef)
      },
      List(KindBoxedBoolean) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionID.typeTest(BooleanRef))
      },
      List(KindBoxedCharacter) -> { () =>
        instrs += LocalGet(valueParam)
        val structTypeName = genTypeID.forClass(SpecialNames.CharBoxClass)
        instrs += RefTest(RefType(structTypeName))
      },
      List(KindBoxedByte) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionID.typeTest(ByteRef))
      },
      List(KindBoxedShort) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionID.typeTest(ShortRef))
      },
      List(KindBoxedInteger) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionID.typeTest(IntRef))
      },
      List(KindBoxedLong) -> { () =>
        instrs += LocalGet(valueParam)
        val structTypeName = genTypeID.forClass(SpecialNames.LongBoxClass)
        instrs += RefTest(RefType(structTypeName))
      },
      List(KindBoxedFloat) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionID.typeTest(FloatRef))
      },
      List(KindBoxedDouble) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionID.typeTest(DoubleRef))
      },
      List(KindBoxedString) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionID.isString)
      },
      // case KindJSType => call typeData.isJSClassInstance(value) or throw if it is null
      List(KindJSType) -> { () =>
        instrs.block(RefType.anyref) { isJSClassInstanceIsNull =>
          // Load value as the argument to the function
          instrs += LocalGet(valueParam)

          // Load the function reference; break if null
          instrs += LocalGet(typeDataParam)
          instrs += StructGet(genTypeID.typeData, isJSClassInstance)
          instrs += BrOnNull(isJSClassInstanceIsNull)

          // Call the function
          instrs += CallRef(genTypeID.isJSClassInstanceFuncType)
          instrs += Return
        }
        instrs += Drop // drop `value` which was left on the stack

        // throw new TypeError("...")
        instrs ++= ctx.getConstantStringInstr("TypeError")
        instrs += Call(genFunctionID.jsGlobalRefGet)
        instrs += Call(genFunctionID.jsNewArray)
        instrs ++= ctx.getConstantStringInstr(
          "Cannot call isInstance() on a Class representing a JS trait/object"
        )
        instrs += Call(genFunctionID.jsArrayPush)
        instrs += Call(genFunctionID.jsNew)
        instrs += ExternConvertAny
        instrs += Throw(genTagID.exception)
      }
    ) { () =>
      // case _ =>

      // valueNonNull := as_non_null value; return false if null
      instrs.block(RefType.any) { nonNullLabel =>
        instrs += LocalGet(valueParam)
        instrs += BrOnNonNull(nonNullLabel)
        instrs += I32Const(0)
        instrs += Return
      }
      instrs += LocalSet(valueNonNullLocal)

      /* If `typeData` represents an ancestor of a hijacked classes, we have to
       * answer `true` if `valueNonNull` is a primitive instance of any of the
       * hijacked classes that ancestor class/interface. For example, for
       * `Comparable`, we have to answer `true` if `valueNonNull` is a primitive
       * boolean, number or string.
       *
       * To do that, we use `jsValueType` and `typeData.specialInstanceTypes`.
       *
       * We test whether `jsValueType(valueNonNull)` is in the set represented by
       * `specialInstanceTypes`. Since the latter is a bitset where the bit
       * indices correspond to the values returned by `jsValueType`, we have to
       * test whether
       *
       * ((1 << jsValueType(valueNonNull)) & specialInstanceTypes) != 0
       *
       * Since computing `jsValueType` is somewhat expensive, we first test
       * whether `specialInstanceTypes != 0` before calling `jsValueType`.
       *
       * There is a more elaborated concrete example of this algorithm in
       * `genInstanceTest`.
       */
      instrs += LocalGet(typeDataParam)
      instrs += StructGet(genTypeID.typeData, specialInstanceTypes)
      instrs += LocalTee(specialInstanceTypesLocal)
      instrs += I32Const(0)
      instrs += I32Ne
      instrs.ifThen() {
        // Load (1 << jsValueType(valueNonNull))
        instrs += I32Const(1)
        instrs += LocalGet(valueNonNullLocal)
        instrs += Call(genFunctionID.jsValueType)
        instrs += I32Shl

        // if ((... & specialInstanceTypes) != 0)
        instrs += LocalGet(specialInstanceTypesLocal)
        instrs += I32And
        instrs += I32Const(0)
        instrs += I32Ne
        instrs.ifThen() {
          // then return true
          instrs += I32Const(1)
          instrs += Return
        }
      }

      // Get the vtable and delegate to isAssignableFrom

      // Load typeData
      instrs += LocalGet(typeDataParam)

      // Load the vtable; return false if it is not one of our object
      instrs.block(objectRefType) { ourObjectLabel =>
        // Try cast to jl.Object
        instrs += LocalGet(valueNonNullLocal)
        instrs += BrOnCast(
          ourObjectLabel,
          RefType.any,
          RefType(objectRefType.heapType)
        )

        // on cast fail, return false
        instrs += I32Const(0)
        instrs += Return
      }
      instrs += StructGet(
        genTypeID.forClass(ObjectClass),
        genFieldID.objStruct.vtable
      )

      // Call isAssignableFrom
      instrs += Call(genFunctionID.isAssignableFrom)
    }

    fb.buildAndAddToModule()
  }

  /** `isAssignableFromExternal: (ref typeData), anyref -> i32` (a boolean).
    *
    * This is the underlying func for the `isAssignableFrom()` closure inside class data objects.
    */
  private def genIsAssignableFromExternal()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.isAssignableFromExternal)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val fromParam = fb.addParam("from", RefType.anyref)
    fb.setResultType(Int32)

    val instrs = fb

    // load typeData
    instrs += LocalGet(typeDataParam)

    // load ref.cast<typeData> from["__typeData"] (as a JS selection)
    instrs += LocalGet(fromParam)
    instrs ++= ctx.getConstantStringInstr("__typeData")
    instrs += Call(genFunctionID.jsSelect)
    instrs += RefCast(RefType(typeDataType.heapType))

    // delegate to isAssignableFrom
    instrs += Call(genFunctionID.isAssignableFrom)

    fb.buildAndAddToModule()
  }

  /** `isAssignableFrom: (ref typeData), (ref typeData) -> i32` (a boolean).
    *
    * Specified by `java.lang.Class.isAssignableFrom(Class)`.
    */
  private def genIsAssignableFrom()(implicit ctx: WasmContext): Unit = {
    import genFieldID.typeData._

    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.isAssignableFrom)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val fromTypeDataParam = fb.addParam("fromTypeData", typeDataType)
    fb.setResultType(Int32)

    val instrs = fb

    val fromAncestorsLocal = fb.addLocal("fromAncestors", RefType(genTypeID.typeDataArray))
    val lenLocal = fb.addLocal("len", Int32)
    val iLocal = fb.addLocal("i", Int32)

    // if (fromTypeData eq typeData)
    instrs += LocalGet(fromTypeDataParam)
    instrs += LocalGet(typeDataParam)
    instrs += RefEq
    instrs.ifThen() {
      // then return true
      instrs += I32Const(1)
      instrs += Return
    }

    // "Tail call" loop for diving into array component types
    instrs.loop(Int32) { loopForArrayLabel =>
      // switch (typeData.kind)
      instrs.switch(Int32) { () =>
        // typeData.kind
        instrs += LocalGet(typeDataParam)
        instrs += StructGet(genTypeID.typeData, kind)
      }(
        // case anyPrimitiveKind => return false
        (KindVoid to KindLastPrimitive).toList -> { () =>
          instrs += I32Const(0)
        },
        // case KindArray => check that from is an array, recurse into component types
        List(KindArray) -> { () =>
          instrs.block() { fromComponentTypeIsNullLabel =>
            // fromTypeData := fromTypeData.componentType; jump out if null
            instrs += LocalGet(fromTypeDataParam)
            instrs += StructGet(genTypeID.typeData, componentType)
            instrs += BrOnNull(fromComponentTypeIsNullLabel)
            instrs += LocalSet(fromTypeDataParam)

            // typeData := ref.as_non_null typeData.componentType (OK because KindArray)
            instrs += LocalGet(typeDataParam)
            instrs += StructGet(genTypeID.typeData, componentType)
            instrs += RefAsNotNull
            instrs += LocalSet(typeDataParam)

            // loop back ("tail call")
            instrs += Br(loopForArrayLabel)
          }

          // return false
          instrs += I32Const(0)
        },
        // case KindObject => return (fromTypeData.kind > KindLastPrimitive)
        List(KindObject) -> { () =>
          instrs += LocalGet(fromTypeDataParam)
          instrs += StructGet(genTypeID.typeData, kind)
          instrs += I32Const(KindLastPrimitive)
          instrs += I32GtU
        }
      ) { () =>
        // All other cases: test whether `fromTypeData.strictAncestors` contains `typeData`

        instrs.block() { fromAncestorsIsNullLabel =>
          // fromAncestors := fromTypeData.strictAncestors; go to fromAncestorsIsNull if null
          instrs += LocalGet(fromTypeDataParam)
          instrs += StructGet(genTypeID.typeData, strictAncestors)
          instrs += BrOnNull(fromAncestorsIsNullLabel)
          instrs += LocalTee(fromAncestorsLocal)

          // if fromAncestors contains typeData, return true

          // len := fromAncestors.length
          instrs += ArrayLen
          instrs += LocalSet(lenLocal)

          // i := 0
          instrs += I32Const(0)
          instrs += LocalSet(iLocal)

          // while (i != len)
          instrs.whileLoop() {
            instrs += LocalGet(iLocal)
            instrs += LocalGet(lenLocal)
            instrs += I32Ne
          } {
            // if (fromAncestors[i] eq typeData)
            instrs += LocalGet(fromAncestorsLocal)
            instrs += LocalGet(iLocal)
            instrs += ArrayGet(genTypeID.typeDataArray)
            instrs += LocalGet(typeDataParam)
            instrs += RefEq
            instrs.ifThen() {
              // then return true
              instrs += I32Const(1)
              instrs += Return
            }

            // i := i + 1
            instrs += LocalGet(iLocal)
            instrs += I32Const(1)
            instrs += I32Add
            instrs += LocalSet(iLocal)
          }
        }

        // from.strictAncestors is null or does not contain typeData
        // return false
        instrs += I32Const(0)
      }
    }

    fb.buildAndAddToModule()
  }

  /** `checkCast: (ref typeData), anyref -> anyref`.
    *
    * Casts the given value to the given type; subject to undefined behaviors.
    */
  private def genCheckCast()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.checkCast)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val valueParam = fb.addParam("value", RefType.anyref)
    fb.setResultType(RefType.anyref)

    val instrs = fb

    /* Given that we only implement `CheckedBehavior.Unchecked` semantics for
     * now, this is always the identity.
     */

    instrs += LocalGet(valueParam)

    fb.buildAndAddToModule()
  }

  /** `getComponentType: (ref typeData) -> (ref null jlClass)`.
    *
    * This is the underlying func for the `getComponentType()` closure inside class data objects.
    */
  private def genGetComponentType()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.getComponentType)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(RefType.nullable(genTypeID.ClassStruct))

    val instrs = fb

    val componentTypeDataLocal = fb.addLocal("componentTypeData", typeDataType)

    instrs.block() { nullResultLabel =>
      // Try and extract non-null component type data
      instrs += LocalGet(typeDataParam)
      instrs += StructGet(genTypeID.typeData, genFieldID.typeData.componentType)
      instrs += BrOnNull(nullResultLabel)
      // Get the corresponding classOf
      instrs += Call(genFunctionID.getClassOf)
      instrs += Return
    } // end block nullResultLabel
    instrs += RefNull(HeapType(genTypeID.ClassStruct))

    fb.buildAndAddToModule()
  }

  /** `newArrayOfThisClass: (ref typeData), anyref -> (ref jlObject)`.
    *
    * This is the underlying func for the `newArrayOfThisClass()` closure inside class data objects.
    */
  private def genNewArrayOfThisClass()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)
    val i32ArrayType = RefType(genTypeID.i32Array)

    val fb = newFunctionBuilder(genFunctionID.newArrayOfThisClass)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val lengthsParam = fb.addParam("lengths", RefType.anyref)
    fb.setResultType(RefType(genTypeID.ObjectStruct))

    val instrs = fb

    val lengthsLenLocal = fb.addLocal("lengthsLenLocal", Int32)
    val lengthsValuesLocal = fb.addLocal("lengthsValues", i32ArrayType)
    val iLocal = fb.addLocal("i", Int32)

    // lengthsLen := lengths.length // as a JS field access
    instrs += LocalGet(lengthsParam)
    instrs ++= ctx.getConstantStringInstr("length")
    instrs += Call(genFunctionID.jsSelect)
    instrs += Call(genFunctionID.unbox(IntRef))
    instrs += LocalTee(lengthsLenLocal)

    // lengthsValues := array.new<i32Array> lengthsLen
    instrs += ArrayNewDefault(genTypeID.i32Array)
    instrs += LocalSet(lengthsValuesLocal)

    // i := 0
    instrs += I32Const(0)
    instrs += LocalSet(iLocal)

    // while (i != lengthsLen)
    instrs.whileLoop() {
      instrs += LocalGet(iLocal)
      instrs += LocalGet(lengthsLenLocal)
      instrs += I32Ne
    } {
      // lengthsValue[i] := lengths[i] (where the rhs is a JS field access)

      instrs += LocalGet(lengthsValuesLocal)
      instrs += LocalGet(iLocal)

      instrs += LocalGet(lengthsParam)
      instrs += LocalGet(iLocal)
      instrs += RefI31
      instrs += Call(genFunctionID.jsSelect)
      instrs += Call(genFunctionID.unbox(IntRef))

      instrs += ArraySet(genTypeID.i32Array)

      // i += 1
      instrs += LocalGet(iLocal)
      instrs += I32Const(1)
      instrs += I32Add
      instrs += LocalSet(iLocal)
    }

    // return newArrayObject(arrayTypeData(typeData, lengthsLen), lengthsValues, 0)
    instrs += LocalGet(typeDataParam)
    instrs += LocalGet(lengthsLenLocal)
    instrs += Call(genFunctionID.arrayTypeData)
    instrs += LocalGet(lengthsValuesLocal)
    instrs += I32Const(0)
    instrs += Call(genFunctionID.newArrayObject)

    fb.buildAndAddToModule()
  }

  /** `anyGetClass: (ref any) -> (ref null jlClass)`.
    *
    * This is the implementation of `value.getClass()` when `value` can be an instance of a hijacked
    * class, i.e., a primitive.
    *
    * For `number`s, the result is based on the actual value, as specified by
    * [[https://www.scala-js.org/doc/semantics.html#getclass]].
    */
  private def genAnyGetClass()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.anyGetClass)
    val valueParam = fb.addParam("value", RefType.any)
    fb.setResultType(RefType.nullable(genTypeID.ClassStruct))

    val instrs = fb

    val typeDataLocal = fb.addLocal("typeData", typeDataType)
    val doubleValueLocal = fb.addLocal("doubleValue", Float64)
    val intValueLocal = fb.addLocal("intValue", Int32)
    val ourObjectLocal = fb.addLocal("ourObject", RefType(genTypeID.ObjectStruct))

    def getHijackedClassTypeDataInstr(className: ClassName): Instr =
      GlobalGet(genGlobalID.forVTable(className))

    instrs.block(RefType.nullable(genTypeID.ClassStruct)) { nonNullClassOfLabel =>
      instrs.block(typeDataType) { gotTypeDataLabel =>
        instrs.block(RefType(genTypeID.ObjectStruct)) { ourObjectLabel =>
          // if value is our object, jump to $ourObject
          instrs += LocalGet(valueParam)
          instrs += BrOnCast(
            ourObjectLabel,
            RefType.any,
            RefType(genTypeID.ObjectStruct)
          )

          // switch(jsValueType(value)) { ... }
          instrs.switch(typeDataType) { () =>
            // scrutinee
            instrs += LocalGet(valueParam)
            instrs += Call(genFunctionID.jsValueType)
          }(
            // case JSValueTypeFalse, JSValueTypeTrue => typeDataOf[jl.Boolean]
            List(JSValueTypeFalse, JSValueTypeTrue) -> { () =>
              instrs += getHijackedClassTypeDataInstr(BoxedBooleanClass)
            },
            // case JSValueTypeString => typeDataOf[jl.String]
            List(JSValueTypeString) -> { () =>
              instrs += getHijackedClassTypeDataInstr(BoxedStringClass)
            },
            // case JSValueTypeNumber => ...
            List(JSValueTypeNumber) -> { () =>
              /* For `number`s, the result is based on the actual value, as specified by
               * [[https://www.scala-js.org/doc/semantics.html#getclass]].
               */

              // doubleValue := unboxDouble(value)
              instrs += LocalGet(valueParam)
              instrs += Call(genFunctionID.unbox(DoubleRef))
              instrs += LocalTee(doubleValueLocal)

              // intValue := doubleValue.toInt
              instrs += I32TruncSatF64S
              instrs += LocalTee(intValueLocal)

              // if same(intValue.toDouble, doubleValue) -- same bit pattern to avoid +0.0 == -0.0
              instrs += F64ConvertI32S
              instrs += I64ReinterpretF64
              instrs += LocalGet(doubleValueLocal)
              instrs += I64ReinterpretF64
              instrs += I64Eq
              instrs.ifThenElse(typeDataType) {
                // then it is a Byte, a Short, or an Integer

                // if intValue.toByte.toInt == intValue
                instrs += LocalGet(intValueLocal)
                instrs += I32Extend8S
                instrs += LocalGet(intValueLocal)
                instrs += I32Eq
                instrs.ifThenElse(typeDataType) {
                  // then it is a Byte
                  instrs += getHijackedClassTypeDataInstr(BoxedByteClass)
                } {
                  // else, if intValue.toShort.toInt == intValue
                  instrs += LocalGet(intValueLocal)
                  instrs += I32Extend16S
                  instrs += LocalGet(intValueLocal)
                  instrs += I32Eq
                  instrs.ifThenElse(typeDataType) {
                    // then it is a Short
                    instrs += getHijackedClassTypeDataInstr(BoxedShortClass)
                  } {
                    // else, it is an Integer
                    instrs += getHijackedClassTypeDataInstr(BoxedIntegerClass)
                  }
                }
              } {
                // else, it is a Float or a Double

                // if doubleValue.toFloat.toDouble == doubleValue
                instrs += LocalGet(doubleValueLocal)
                instrs += F32DemoteF64
                instrs += F64PromoteF32
                instrs += LocalGet(doubleValueLocal)
                instrs += F64Eq
                instrs.ifThenElse(typeDataType) {
                  // then it is a Float
                  instrs += getHijackedClassTypeDataInstr(BoxedFloatClass)
                } {
                  // else, if it is NaN
                  instrs += LocalGet(doubleValueLocal)
                  instrs += LocalGet(doubleValueLocal)
                  instrs += F64Ne
                  instrs.ifThenElse(typeDataType) {
                    // then it is a Float
                    instrs += getHijackedClassTypeDataInstr(BoxedFloatClass)
                  } {
                    // else, it is a Double
                    instrs += getHijackedClassTypeDataInstr(BoxedDoubleClass)
                  }
                }
              }
            },
            // case JSValueTypeUndefined => typeDataOf[jl.Void]
            List(JSValueTypeUndefined) -> { () =>
              instrs += getHijackedClassTypeDataInstr(BoxedUnitClass)
            }
          ) { () =>
            // case _ (JSValueTypeOther) => return null
            instrs += RefNull(HeapType(genTypeID.ClassStruct))
            instrs += Return
          }

          instrs += Br(gotTypeDataLabel)
        }

        /* Now we have one of our objects. Normally we only have to get the
         * vtable, but there are two exceptions. If the value is an instance of
         * `jl.CharacterBox` or `jl.LongBox`, we must use the typeData of
         * `jl.Character` or `jl.Long`, respectively.
         */
        instrs += LocalTee(ourObjectLocal)
        instrs += RefTest(RefType(genTypeID.forClass(SpecialNames.CharBoxClass)))
        instrs.ifThenElse(typeDataType) {
          instrs += getHijackedClassTypeDataInstr(BoxedCharacterClass)
        } {
          instrs += LocalGet(ourObjectLocal)
          instrs += RefTest(RefType(genTypeID.forClass(SpecialNames.LongBoxClass)))
          instrs.ifThenElse(typeDataType) {
            instrs += getHijackedClassTypeDataInstr(BoxedLongClass)
          } {
            instrs += LocalGet(ourObjectLocal)
            instrs += StructGet(
              genTypeID.forClass(ObjectClass),
              genFieldID.objStruct.vtable
            )
          }
        }
      }

      instrs += Call(genFunctionID.getClassOf)
    }

    fb.buildAndAddToModule()
  }

  /** `newArrayObject`: `(ref typeData), (ref array i32), i32 -> (ref jl.Object)`.
    *
    * The arguments are `arrayTypeData`, `lengths` and `lengthIndex`.
    *
    * This recursive function creates a multi-dimensional array. The resulting array has type data
    * `arrayTypeData` and length `lengths(lengthIndex)`. If `lengthIndex < `lengths.length - 1`, its
    * elements are recursively initialized with `newArrayObject(arrayTypeData.componentType,
    * lengths, lengthIndex - 1)`.
    */
  private def genNewArrayObject()(implicit ctx: WasmContext): Unit = {
    import genFieldID.typeData._

    val typeDataType = RefType(genTypeID.typeData)
    val i32ArrayType = RefType(genTypeID.i32Array)
    val objectVTableType = RefType(genTypeID.ObjectVTable)
    val arrayTypeDataType = objectVTableType
    val itablesType = RefType.nullable(genTypeID.itables)
    val nonNullObjectType = RefType(genTypeID.ObjectStruct)
    val anyArrayType = RefType(genTypeID.anyArray)

    val fb = newFunctionBuilder(genFunctionID.newArrayObject)
    val arrayTypeDataParam = fb.addParam("arrayTypeData", arrayTypeDataType)
    val lengthsParam = fb.addParam("lengths", i32ArrayType)
    val lengthIndexParam = fb.addParam("lengthIndex", Int32)
    fb.setResultType(nonNullObjectType)

    val instrs = fb

    val lenLocal = fb.addLocal("len", Int32)
    val underlyingLocal = fb.addLocal("underlying", anyArrayType)
    val subLengthIndexLocal = fb.addLocal("subLengthIndex", Int32)
    val arrayComponentTypeDataLocal = fb.addLocal("arrayComponentTypeData", arrayTypeDataType)
    val iLocal = fb.addLocal("i", Int32)

    /* High-level pseudo code of what this function does:
     *
     * def newArrayObject(arrayTypeData, lengths, lengthIndex) {
     *   // create an array of the right primitive type
     *   val len = lengths(lengthIndex)
     *   switch (arrayTypeData.componentType.kind) {
     *     // for primitives, return without recursion
     *     case KindBoolean => new Array[Boolean](len)
     *     ...
     *     case KindDouble => new Array[Double](len)
     *
     *     // for reference array types, maybe recursively initialize
     *     case _ =>
     *       val result = new Array[Object](len) // with arrayTypeData as vtable
     *       val subLengthIndex = lengthIndex + 1
     *       if (subLengthIndex != lengths.length) {
     *         val arrayComponentTypeData = arrayTypeData.componentType
     *         for (i <- 0 until len)
     *           result(i) = newArrayObject(arrayComponentTypeData, lengths, subLengthIndex)
     *       }
     *       result
     *   }
     * }
     */

    val primRefsWithArrayTypes = List(
      BooleanRef -> KindBoolean,
      CharRef -> KindChar,
      ByteRef -> KindByte,
      ShortRef -> KindShort,
      IntRef -> KindInt,
      LongRef -> KindLong,
      FloatRef -> KindFloat,
      DoubleRef -> KindDouble
    )

    // Load the vtable and itable or the resulting array on the stack
    instrs += LocalGet(arrayTypeDataParam) // vtable
    instrs += GlobalGet(genGlobalID.arrayClassITable) // itable

    // Load the first length
    instrs += LocalGet(lengthsParam)
    instrs += LocalGet(lengthIndexParam)
    instrs += ArrayGet(genTypeID.i32Array)

    // componentTypeData := ref_as_non_null(arrayTypeData.componentType)
    // switch (componentTypeData.kind)
    val switchClauseSig = FunctionType(
      List(arrayTypeDataType, itablesType, Int32),
      List(nonNullObjectType)
    )
    instrs.switch(switchClauseSig) { () =>
      // scrutinee
      instrs += LocalGet(arrayTypeDataParam)
      instrs += StructGet(genTypeID.typeData, genFieldID.typeData.componentType)
      instrs += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
    }(
      // For all the primitive types, by construction, this is the bottom dimension
      // case KindPrim => array.new_default underlyingPrimArray; struct.new PrimArray
      primRefsWithArrayTypes.map { case (primRef, kind) =>
        List(kind) -> { () =>
          val arrayTypeRef = ArrayTypeRef(primRef, 1)
          instrs += ArrayNewDefault(genTypeID.underlyingOf(arrayTypeRef))
          instrs += StructNew(genTypeID.forArrayClass(arrayTypeRef))
          () // required for correct type inference
        }
      }: _*
    ) { () =>
      // default -- all non-primitive array types

      // len := <top-of-stack> (which is the first length)
      instrs += LocalTee(lenLocal)

      // underlying := array.new_default anyArray
      val arrayTypeRef = ArrayTypeRef(ClassRef(ObjectClass), 1)
      instrs += ArrayNewDefault(genTypeID.underlyingOf(arrayTypeRef))
      instrs += LocalSet(underlyingLocal)

      // subLengthIndex := lengthIndex + 1
      instrs += LocalGet(lengthIndexParam)
      instrs += I32Const(1)
      instrs += I32Add
      instrs += LocalTee(subLengthIndexLocal)

      // if subLengthIndex != lengths.length
      instrs += LocalGet(lengthsParam)
      instrs += ArrayLen
      instrs += I32Ne
      instrs.ifThen() {
        // then, recursively initialize all the elements

        // arrayComponentTypeData := ref_cast<arrayTypeDataType> arrayTypeData.componentTypeData
        instrs += LocalGet(arrayTypeDataParam)
        instrs += StructGet(genTypeID.typeData, genFieldID.typeData.componentType)
        instrs += RefCast(RefType(arrayTypeDataType.heapType))
        instrs += LocalSet(arrayComponentTypeDataLocal)

        // i := 0
        instrs += I32Const(0)
        instrs += LocalSet(iLocal)

        // while (i != len)
        instrs.whileLoop() {
          instrs += LocalGet(iLocal)
          instrs += LocalGet(lenLocal)
          instrs += I32Ne
        } {
          // underlying[i] := newArrayObject(arrayComponentType, lengths, subLengthIndex)

          instrs += LocalGet(underlyingLocal)
          instrs += LocalGet(iLocal)

          instrs += LocalGet(arrayComponentTypeDataLocal)
          instrs += LocalGet(lengthsParam)
          instrs += LocalGet(subLengthIndexLocal)
          instrs += Call(genFunctionID.newArrayObject)

          instrs += ArraySet(genTypeID.anyArray)

          // i += 1
          instrs += LocalGet(iLocal)
          instrs += I32Const(1)
          instrs += I32Add
          instrs += LocalSet(iLocal)
        }
      }

      // load underlying; struct.new ObjectArray
      instrs += LocalGet(underlyingLocal)
      instrs += StructNew(genTypeID.forArrayClass(arrayTypeRef))
    }

    fb.buildAndAddToModule()
  }

  /** `identityHashCode`: `anyref -> i32`.
    *
    * This is the implementation of `IdentityHashCode`. It is also used to compute the `hashCode()`
    * of primitive values when dispatch is required (i.e., when the receiver type is not known to be
    * a specific primitive or hijacked class), so it must be consistent with the implementations of
    * `hashCode()` in hijacked classes.
    *
    * For `String` and `Double`, we actually call the hijacked class methods, as they are a bit
    * involved. For `Boolean` and `Void`, we hard-code a copy here.
    */
  private def genIdentityHashCode()(implicit ctx: WasmContext): Unit = {
    import MemberNamespace.Public
    import SpecialNames.hashCodeMethodName
    import genFieldID.typeData._

    // A global exclusively used by this function
    ctx.addGlobal(
      Global(
        genGlobalID.lastIDHashCode,
        OriginalName(genGlobalID.lastIDHashCode.toString()),
        Int32,
        Expr(List(I32Const(0))),
        isMutable = true
      )
    )

    val fb = newFunctionBuilder(genFunctionID.identityHashCode)
    val objParam = fb.addParam("obj", RefType.anyref)
    fb.setResultType(Int32)

    val instrs = fb

    val objNonNullLocal = fb.addLocal("objNonNull", RefType.any)
    val resultLocal = fb.addLocal("result", Int32)

    // If `obj` is `null`, return 0 (by spec)
    instrs.block(RefType.any) { nonNullLabel =>
      instrs += LocalGet(objParam)
      instrs += BrOnNonNull(nonNullLabel)
      instrs += I32Const(0)
      instrs += Return
    }
    instrs += LocalTee(objNonNullLocal)

    // If `obj` is one of our objects, skip all the jsValueType tests
    instrs += RefTest(RefType(genTypeID.ObjectStruct))
    instrs += I32Eqz
    instrs.ifThen() {
      instrs.switch() { () =>
        instrs += LocalGet(objNonNullLocal)
        instrs += Call(genFunctionID.jsValueType)
      }(
        List(JSValueTypeFalse) -> { () =>
          instrs += I32Const(1237) // specified by jl.Boolean.hashCode()
          instrs += Return
        },
        List(JSValueTypeTrue) -> { () =>
          instrs += I32Const(1231) // specified by jl.Boolean.hashCode()
          instrs += Return
        },
        List(JSValueTypeString) -> { () =>
          instrs += LocalGet(objNonNullLocal)
          instrs += Call(
            genFunctionID.forMethod(Public, BoxedStringClass, hashCodeMethodName)
          )
          instrs += Return
        },
        List(JSValueTypeNumber) -> { () =>
          instrs += LocalGet(objNonNullLocal)
          instrs += Call(genFunctionID.unbox(DoubleRef))
          instrs += Call(
            genFunctionID.forMethod(Public, BoxedDoubleClass, hashCodeMethodName)
          )
          instrs += Return
        },
        List(JSValueTypeUndefined) -> { () =>
          instrs += I32Const(0) // specified by jl.Void.hashCode(), Scala.js only
          instrs += Return
        },
        List(JSValueTypeBigInt) -> { () =>
          instrs += LocalGet(objNonNullLocal)
          instrs += Call(genFunctionID.bigintHashCode)
          instrs += Return
        },
        List(JSValueTypeSymbol) -> { () =>
          instrs.block() { descriptionIsNullLabel =>
            instrs += LocalGet(objNonNullLocal)
            instrs += Call(genFunctionID.symbolDescription)
            instrs += BrOnNull(descriptionIsNullLabel)
            instrs += Call(
              genFunctionID.forMethod(Public, BoxedStringClass, hashCodeMethodName)
            )
            instrs += Return
          }
          instrs += I32Const(0)
          instrs += Return
        }
      ) { () =>
        // JSValueTypeOther -- fall through to using idHashCodeMap
        ()
      }
    }

    // If we get here, use the idHashCodeMap

    // Read the existing idHashCode, if one exists
    instrs += GlobalGet(genGlobalID.idHashCodeMap)
    instrs += LocalGet(objNonNullLocal)
    instrs += Call(genFunctionID.idHashCodeGet)
    instrs += LocalTee(resultLocal)

    // If it is 0, there was no recorded idHashCode yet; allocate a new one
    instrs += I32Eqz
    instrs.ifThen() {
      // Allocate a new idHashCode
      instrs += GlobalGet(genGlobalID.lastIDHashCode)
      instrs += I32Const(1)
      instrs += I32Add
      instrs += LocalTee(resultLocal)
      instrs += GlobalSet(genGlobalID.lastIDHashCode)

      // Store it for next time
      instrs += GlobalGet(genGlobalID.idHashCodeMap)
      instrs += LocalGet(objNonNullLocal)
      instrs += LocalGet(resultLocal)
      instrs += Call(genFunctionID.idHashCodeSet)
    }

    instrs += LocalGet(resultLocal)

    fb.buildAndAddToModule()
  }

  /** Search for a reflective proxy function with the given `methodId` in the `reflectiveProxies`
    * field in `typeData` and returns the corresponding function reference.
    *
    * `searchReflectiveProxy`: [typeData, i32] -> [(ref func)]
    */
  private def genSearchReflectiveProxy()(implicit ctx: WasmContext): Unit = {
    import genFieldID.typeData._

    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.searchReflectiveProxy)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val methodIdParam = fb.addParam("methodId", Int32)
    fb.setResultType(RefType(HeapType.Func))

    val instrs = fb

    val reflectiveProxies =
      fb.addLocal("reflectiveProxies", Types.RefType(genTypeID.reflectiveProxies))
    val size = fb.addLocal("size", Types.Int32)
    val i = fb.addLocal("i", Types.Int32)

    instrs += LocalGet(typeDataParam)
    instrs += StructGet(genTypeID.typeData, genFieldID.typeData.reflectiveProxies)
    instrs += LocalTee(reflectiveProxies)
    instrs += ArrayLen
    instrs += LocalSet(size)

    instrs += I32Const(0)
    instrs += LocalSet(i)

    instrs.whileLoop() {
      instrs += LocalGet(i)
      instrs += LocalGet(size)
      instrs += I32Ne
    } {
      instrs += LocalGet(reflectiveProxies)
      instrs += LocalGet(i)
      instrs += ArrayGet(genTypeID.reflectiveProxies)

      instrs += StructGet(genTypeID.reflectiveProxy, genFieldID.reflectiveProxy.func_name)
      instrs += LocalGet(methodIdParam)
      instrs += I32Eq

      instrs.ifThen() {
        instrs += LocalGet(reflectiveProxies)
        instrs += LocalGet(i)
        instrs += ArrayGet(genTypeID.reflectiveProxies)

        // get function reference
        instrs += StructGet(genTypeID.reflectiveProxy, genFieldID.reflectiveProxy.func_ref)
        instrs += Return
      }

      // i += 1
      instrs += LocalGet(i)
      instrs += I32Const(1)
      instrs += I32Add
      instrs += LocalSet(i)
    }
    // throw new TypeError("...")
    instrs ++= ctx.getConstantStringInstr("TypeError")
    instrs += Call(genFunctionID.jsGlobalRefGet)
    instrs += Call(genFunctionID.jsNewArray)
    // Originally, exception is thrown from JS saying e.g. "obj2.z1__ is not a function"
    instrs ++= ctx.getConstantStringInstr("Method not found")
    instrs += Call(genFunctionID.jsArrayPush)
    instrs += Call(genFunctionID.jsNew)
    instrs += ExternConvertAny
    instrs += Throw(genTagID.exception)

    fb.buildAndAddToModule()
  }

  private def genArrayCloneFunctions()(implicit ctx: WasmContext): Unit = {
    val baseRefs = List(
      BooleanRef,
      CharRef,
      ByteRef,
      ShortRef,
      IntRef,
      LongRef,
      FloatRef,
      DoubleRef,
      ClassRef(ObjectClass)
    )

    for (baseRef <- baseRefs)
      genArrayCloneFunction(baseRef)
  }

  /** Generates the clone function for the array class with the given base. */
  private def genArrayCloneFunction(baseRef: NonArrayTypeRef)(implicit ctx: WasmContext): Unit = {
    val charCodeForOriginalName = baseRef match {
      case baseRef: PrimRef => baseRef.charCode
      case _: ClassRef      => 'O'
    }
    val originalName = OriginalName("cloneArray." + charCodeForOriginalName)

    val fb = newFunctionBuilder(genFunctionID.clone(baseRef), originalName)
    val fromParam = fb.addParam("from", RefType(genTypeID.ObjectStruct))
    fb.setResultType(RefType(genTypeID.ObjectStruct))
    fb.setFunctionType(genTypeID.cloneFunctionType)

    val instrs = fb

    val arrayTypeRef = ArrayTypeRef(baseRef, 1)

    val arrayStructTypeName = genTypeID.forArrayClass(arrayTypeRef)
    val arrayClassType = RefType(arrayStructTypeName)

    val underlyingArrayTypeName = genTypeID.underlyingOf(arrayTypeRef)
    val underlyingArrayType = RefType(underlyingArrayTypeName)

    val fromLocal = fb.addLocal("fromTyped", arrayClassType)
    val fromUnderlyingLocal = fb.addLocal("fromUnderlying", underlyingArrayType)
    val lengthLocal = fb.addLocal("length", Int32)
    val resultUnderlyingLocal = fb.addLocal("resultUnderlying", underlyingArrayType)

    // Cast down the from argument
    instrs += LocalGet(fromParam)
    instrs += RefCast(arrayClassType)
    instrs += LocalTee(fromLocal)

    // Load the underlying array
    instrs += StructGet(arrayStructTypeName, genFieldID.objStruct.arrayUnderlying)
    instrs += LocalTee(fromUnderlyingLocal)

    // Make a copy of the underlying array
    instrs += ArrayLen
    instrs += LocalTee(lengthLocal)
    instrs += ArrayNewDefault(underlyingArrayTypeName)
    instrs += LocalTee(resultUnderlyingLocal) // also dest for array.copy
    instrs += I32Const(0) // destOffset
    instrs += LocalGet(fromUnderlyingLocal) // src
    instrs += I32Const(0) // srcOffset
    instrs += LocalGet(lengthLocal) // length
    instrs += ArrayCopy(underlyingArrayTypeName, underlyingArrayTypeName)

    // Build the result arrayStruct
    instrs += LocalGet(fromLocal)
    instrs += StructGet(arrayStructTypeName, genFieldID.objStruct.vtable) // vtable
    instrs += GlobalGet(genGlobalID.arrayClassITable) // itable
    instrs += LocalGet(resultUnderlyingLocal)
    instrs += StructNew(arrayStructTypeName)

    fb.buildAndAddToModule()
  }

}
