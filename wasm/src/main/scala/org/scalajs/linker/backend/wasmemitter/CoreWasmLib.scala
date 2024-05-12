package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{ClassKind, Position}

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.Instructions._
import org.scalajs.linker.backend.webassembly.Names._
import org.scalajs.linker.backend.webassembly.Modules._
import org.scalajs.linker.backend.webassembly.Types._

import EmbeddedConstants._
import VarGen._
import TypeTransformer._

object CoreWasmLib {
  import WasmRefType.anyref

  private implicit val noPos: Position = Position.NoPosition

  /** Fields of the `typeData` struct definition.
    *
    * They are accessible as a public list because they must be repeated in every vtable type
    * definition.
    *
    * @see
    *   [[VarGen.genFieldName.typeData]], which contains documentation of what is in each field.
    */
  val typeDataStructFields: List[WasmStructField] = {
    import genFieldName.typeData._
    import WasmRefType.nullable
    List(
      WasmStructField(nameOffset, WasmInt32, isMutable = false),
      WasmStructField(nameSize, WasmInt32, isMutable = false),
      WasmStructField(nameStringIndex, WasmInt32, isMutable = false),
      WasmStructField(kind, WasmInt32, isMutable = false),
      WasmStructField(specialInstanceTypes, WasmInt32, isMutable = false),
      WasmStructField(strictAncestors, nullable(genTypeName.typeDataArray), isMutable = false),
      WasmStructField(componentType, nullable(genTypeName.typeData), isMutable = false),
      WasmStructField(name, WasmRefType.anyref, isMutable = true),
      WasmStructField(classOfValue, nullable(genTypeName.ClassStruct), isMutable = true),
      WasmStructField(arrayOf, nullable(genTypeName.ObjectVTable), isMutable = true),
      WasmStructField(cloneFunction, nullable(genTypeName.cloneFunctionType), isMutable = false),
      WasmStructField(
        isJSClassInstance,
        nullable(genTypeName.isJSClassInstanceFuncType),
        isMutable = false
      ),
      WasmStructField(
        reflectiveProxies,
        WasmRefType(genTypeName.reflectiveProxies),
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

    b.addRecType(genTypeName.i8Array, WasmArrayType(WasmFieldType(WasmInt8, true)))
    b.addRecType(genTypeName.i16Array, WasmArrayType(WasmFieldType(WasmInt16, true)))
    b.addRecType(genTypeName.i32Array, WasmArrayType(WasmFieldType(WasmInt32, true)))
    b.addRecType(genTypeName.i64Array, WasmArrayType(WasmFieldType(WasmInt64, true)))
    b.addRecType(genTypeName.f32Array, WasmArrayType(WasmFieldType(WasmFloat32, true)))
    b.addRecType(genTypeName.f64Array, WasmArrayType(WasmFieldType(WasmFloat64, true)))
    b.addRecType(genTypeName.anyArray, WasmArrayType(WasmFieldType(anyref, true)))
  }

  private def genCoreTypesInRecType()(implicit ctx: WasmContext): Unit = {
    ctx.mainRecType.addSubType(
      genTypeName.cloneFunctionType,
      WasmFunctionType(
        List(WasmRefType(genTypeName.ObjectStruct)),
        List(WasmRefType(genTypeName.ObjectStruct))
      )
    )

    ctx.mainRecType.addSubType(
      genTypeName.isJSClassInstanceFuncType,
      WasmFunctionType(List(WasmRefType.anyref), List(WasmInt32))
    )

    ctx.mainRecType.addSubType(
      genTypeName.typeDataArray,
      WasmArrayType(WasmFieldType(WasmRefType(genTypeName.typeData), isMutable = false))
    )
    ctx.mainRecType.addSubType(
      genTypeName.itables,
      WasmArrayType(WasmFieldType(WasmRefType.nullable(WasmHeapType.Struct), isMutable = true))
    )
    ctx.mainRecType.addSubType(
      genTypeName.reflectiveProxies,
      WasmArrayType(WasmFieldType(WasmRefType(genTypeName.reflectiveProxy), isMutable = false))
    )

    ctx.mainRecType.addSubType(
      WasmSubType(
        genTypeName.typeData,
        isFinal = false,
        None,
        WasmStructType(typeDataStructFields)
      )
    )

    ctx.mainRecType.addSubType(
      genTypeName.reflectiveProxy,
      WasmStructType(
        List(
          WasmStructField(genFieldName.reflectiveProxy.func_name, WasmInt32, isMutable = false),
          WasmStructField(
            genFieldName.reflectiveProxy.func_ref,
            WasmRefType(WasmHeapType.Func),
            isMutable = false
          )
        )
      )
    )
  }

  private def genArrayClassTypes()(implicit ctx: WasmContext): Unit = {
    // The vtable type is always the same as j.l.Object
    val vtableTypeName = genTypeName.ObjectVTable
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

    val typeRefsWithArrays: List[(WasmTypeName, WasmTypeName)] =
      List(
        (genTypeName.BooleanArray, genTypeName.i8Array),
        (genTypeName.CharArray, genTypeName.i16Array),
        (genTypeName.ByteArray, genTypeName.i8Array),
        (genTypeName.ShortArray, genTypeName.i16Array),
        (genTypeName.IntArray, genTypeName.i32Array),
        (genTypeName.LongArray, genTypeName.i64Array),
        (genTypeName.FloatArray, genTypeName.f32Array),
        (genTypeName.DoubleArray, genTypeName.f64Array),
        (genTypeName.ObjectArray, genTypeName.anyArray)
      )

    for ((structTypeName, underlyingArrayTypeName) <- typeRefsWithArrays) {
      val underlyingArrayField = WasmStructField(
        genFieldName.objStruct.arrayUnderlying,
        WasmRefType(underlyingArrayTypeName),
        isMutable = false
      )

      val superType = genTypeName.ObjectStruct
      val structType = WasmStructType(
        List(vtableField, itablesField, underlyingArrayField)
      )
      val subType = WasmSubType(structTypeName, isFinal = true, Some(superType), structType)
      ctx.mainRecType.addSubType(subType)
    }
  }

  private def genTags()(implicit ctx: WasmContext): Unit = {
    val exceptionSig = WasmFunctionSignature(List(WasmRefType.externref), Nil)
    val typeName = ctx.moduleBuilder.signatureToTypeName(exceptionSig)
    ctx.moduleBuilder.addImport(
      WasmImport(
        "__scalaJSHelpers",
        "JSTag",
        WasmImportDesc.Tag(genTagName.exceptionTagName, typeName)
      )
    )
  }

  private def genGlobalImports()(implicit ctx: WasmContext): Unit = {
    def addGlobalHelperImport(name: WasmGlobalName, typ: WasmType, isMutable: Boolean): Unit = {
      ctx.moduleBuilder.addImport(
        WasmImport(
          "__scalaJSHelpers",
          name.name,
          WasmImportDesc.Global(name, typ, isMutable)
        )
      )
    }

    addGlobalHelperImport(genGlobalName.undef, WasmRefType.any, isMutable = false)
    addGlobalHelperImport(genGlobalName.bFalse, WasmRefType.any, isMutable = false)
    addGlobalHelperImport(genGlobalName.bZero, WasmRefType.any, isMutable = false)
    addGlobalHelperImport(genGlobalName.emptyString, WasmRefType.any, isMutable = false)
    addGlobalHelperImport(genGlobalName.idHashCodeMap, WasmRefType.extern, isMutable = false)
  }

  private def genPrimitiveTypeDataGlobals()(implicit ctx: WasmContext): Unit = {
    import genFieldName.typeData._

    val primRefsWithTypeData = List(
      IRTypes.VoidRef -> KindVoid,
      IRTypes.BooleanRef -> KindBoolean,
      IRTypes.CharRef -> KindChar,
      IRTypes.ByteRef -> KindByte,
      IRTypes.ShortRef -> KindShort,
      IRTypes.IntRef -> KindInt,
      IRTypes.LongRef -> KindLong,
      IRTypes.FloatRef -> KindFloat,
      IRTypes.DoubleRef -> KindDouble
    )

    val typeDataTypeName = genTypeName.typeData

    // Other than `name` and `kind`, all the fields have the same value for all primitives
    val commonFieldValues = List(
      // specialInstanceTypes
      I32_CONST(0),
      // strictAncestors
      REF_NULL(WasmHeapType.None),
      // componentType
      REF_NULL(WasmHeapType.None),
      // name - initially `null`; filled in by the `typeDataName` helper
      REF_NULL(WasmHeapType.None),
      // the classOf instance - initially `null`; filled in by the `createClassOf` helper
      REF_NULL(WasmHeapType.None),
      // arrayOf, the typeData of an array of this type - initially `null`; filled in by the `arrayTypeData` helper
      REF_NULL(WasmHeapType.None),
      // cloneFunction
      REF_NULL(WasmHeapType.NoFunc),
      // isJSClassInstance
      REF_NULL(WasmHeapType.NoFunc),
      // reflectiveProxies
      ARRAY_NEW_FIXED(genTypeName.reflectiveProxies, 0)
    )

    for ((primRef, kind) <- primRefsWithTypeData) {
      val nameDataValue: List[WasmInstr] =
        ctx.getConstantStringDataInstr(primRef.displayName)

      val instrs: List[WasmInstr] = {
        nameDataValue ::: I32_CONST(kind) :: commonFieldValues :::
          STRUCT_NEW(genTypeName.typeData) :: Nil
      }

      ctx.addGlobal(
        WasmGlobal(
          genGlobalName.forVTable(primRef),
          WasmRefType(genTypeName.typeData),
          WasmExpr(instrs),
          isMutable = false
        )
      )
    }
  }

  private def genBoxedZeroGlobals()(implicit ctx: WasmContext): Unit = {
    val primTypesWithBoxClasses: List[(WasmGlobalName, IRNames.ClassName, WasmInstr)] = List(
      (genGlobalName.bZeroChar, SpecialNames.CharBoxClass, I32_CONST(0)),
      (genGlobalName.bZeroLong, SpecialNames.LongBoxClass, I64_CONST(0))
    )

    for ((globalName, boxClassName, zeroValueInstr) <- primTypesWithBoxClasses) {
      val boxStruct = genTypeName.forClass(boxClassName)
      val instrs: List[WasmInstr] = List(
        GLOBAL_GET(genGlobalName.forVTable(boxClassName)),
        GLOBAL_GET(genGlobalName.forITable(boxClassName)),
        zeroValueInstr,
        STRUCT_NEW(boxStruct)
      )

      ctx.addGlobal(
        WasmGlobal(
          globalName,
          WasmRefType(boxStruct),
          WasmExpr(instrs),
          isMutable = false
        )
      )
    }
  }

  private def genArrayClassGlobals()(implicit ctx: WasmContext): Unit = {
    // Common itable global for all array classes
    val itablesInit = List(
      I32_CONST(ctx.itablesLength),
      ARRAY_NEW_DEFAULT(genTypeName.itables)
    )
    ctx.addGlobal(
      WasmGlobal(
        genGlobalName.arrayClassITable,
        WasmRefType(genTypeName.itables),
        init = WasmExpr(itablesInit),
        isMutable = false
      )
    )
  }

  private def genHelperImports()(implicit ctx: WasmContext): Unit = {
    import WasmRefType.anyref
    import IRTypes._

    def addHelperImport(
        name: WasmFunctionName,
        params: List[WasmType],
        results: List[WasmType]
    ): Unit = {
      val sig = WasmFunctionSignature(params, results)
      val typeName = ctx.moduleBuilder.signatureToTypeName(sig)
      ctx.moduleBuilder.addImport(
        WasmImport("__scalaJSHelpers", name.name, WasmImportDesc.Func(name, typeName))
      )
    }

    addHelperImport(genFunctionName.is, List(anyref, anyref), List(WasmInt32))

    addHelperImport(genFunctionName.isUndef, List(anyref), List(WasmInt32))

    for (primRef <- List(BooleanRef, ByteRef, ShortRef, IntRef, FloatRef, DoubleRef)) {
      val wasmType = primRef match {
        case FloatRef  => WasmFloat32
        case DoubleRef => WasmFloat64
        case _         => WasmInt32
      }
      addHelperImport(genFunctionName.box(primRef), List(wasmType), List(anyref))
      addHelperImport(genFunctionName.unbox(primRef), List(anyref), List(wasmType))
      addHelperImport(genFunctionName.unboxOrNull(primRef), List(anyref), List(anyref))
      addHelperImport(genFunctionName.typeTest(primRef), List(anyref), List(WasmInt32))
    }

    addHelperImport(genFunctionName.fmod, List(WasmFloat64, WasmFloat64), List(WasmFloat64))

    addHelperImport(
      genFunctionName.closure,
      List(WasmRefType.func, anyref),
      List(WasmRefType.any)
    )
    addHelperImport(
      genFunctionName.closureThis,
      List(WasmRefType.func, anyref),
      List(WasmRefType.any)
    )
    addHelperImport(
      genFunctionName.closureRest,
      List(WasmRefType.func, anyref, WasmInt32),
      List(WasmRefType.any)
    )
    addHelperImport(
      genFunctionName.closureThisRest,
      List(WasmRefType.func, anyref, WasmInt32),
      List(WasmRefType.any)
    )
    addHelperImport(
      genFunctionName.closureRestNoData,
      List(WasmRefType.func, WasmInt32),
      List(WasmRefType.any)
    )

    addHelperImport(genFunctionName.stringLength, List(WasmRefType.any), List(WasmInt32))
    addHelperImport(genFunctionName.stringCharAt, List(WasmRefType.any, WasmInt32), List(WasmInt32))
    addHelperImport(genFunctionName.jsValueToString, List(WasmRefType.any), List(WasmRefType.any))
    addHelperImport(genFunctionName.jsValueToStringForConcat, List(anyref), List(WasmRefType.any))
    addHelperImport(genFunctionName.booleanToString, List(WasmInt32), List(WasmRefType.any))
    addHelperImport(genFunctionName.charToString, List(WasmInt32), List(WasmRefType.any))
    addHelperImport(genFunctionName.intToString, List(WasmInt32), List(WasmRefType.any))
    addHelperImport(genFunctionName.longToString, List(WasmInt64), List(WasmRefType.any))
    addHelperImport(genFunctionName.doubleToString, List(WasmFloat64), List(WasmRefType.any))
    addHelperImport(
      genFunctionName.stringConcat,
      List(WasmRefType.any, WasmRefType.any),
      List(WasmRefType.any)
    )
    addHelperImport(genFunctionName.isString, List(anyref), List(WasmInt32))

    addHelperImport(genFunctionName.jsValueType, List(WasmRefType.any), List(WasmInt32))
    addHelperImport(genFunctionName.bigintHashCode, List(WasmRefType.any), List(WasmInt32))
    addHelperImport(
      genFunctionName.symbolDescription,
      List(WasmRefType.any),
      List(WasmRefType.anyref)
    )
    addHelperImport(
      genFunctionName.idHashCodeGet,
      List(WasmRefType.extern, WasmRefType.any),
      List(WasmInt32)
    )
    addHelperImport(
      genFunctionName.idHashCodeSet,
      List(WasmRefType.extern, WasmRefType.any, WasmInt32),
      Nil
    )

    addHelperImport(genFunctionName.jsGlobalRefGet, List(WasmRefType.any), List(anyref))
    addHelperImport(genFunctionName.jsGlobalRefSet, List(WasmRefType.any, anyref), Nil)
    addHelperImport(genFunctionName.jsGlobalRefTypeof, List(WasmRefType.any), List(WasmRefType.any))
    addHelperImport(genFunctionName.jsNewArray, Nil, List(anyref))
    addHelperImport(genFunctionName.jsArrayPush, List(anyref, anyref), List(anyref))
    addHelperImport(
      genFunctionName.jsArraySpreadPush,
      List(anyref, anyref),
      List(anyref)
    )
    addHelperImport(genFunctionName.jsNewObject, Nil, List(anyref))
    addHelperImport(
      genFunctionName.jsObjectPush,
      List(anyref, anyref, anyref),
      List(anyref)
    )
    addHelperImport(genFunctionName.jsSelect, List(anyref, anyref), List(anyref))
    addHelperImport(genFunctionName.jsSelectSet, List(anyref, anyref, anyref), Nil)
    addHelperImport(genFunctionName.jsNew, List(anyref, anyref), List(anyref))
    addHelperImport(genFunctionName.jsFunctionApply, List(anyref, anyref), List(anyref))
    addHelperImport(
      genFunctionName.jsMethodApply,
      List(anyref, anyref, anyref),
      List(anyref)
    )
    addHelperImport(genFunctionName.jsImportCall, List(anyref), List(anyref))
    addHelperImport(genFunctionName.jsImportMeta, Nil, List(anyref))
    addHelperImport(genFunctionName.jsDelete, List(anyref, anyref), Nil)
    addHelperImport(genFunctionName.jsForInSimple, List(anyref, anyref), Nil)
    addHelperImport(genFunctionName.jsIsTruthy, List(anyref), List(WasmInt32))
    addHelperImport(genFunctionName.jsLinkingInfo, Nil, List(anyref))

    for ((op, name) <- genFunctionName.jsUnaryOps)
      addHelperImport(name, List(anyref), List(anyref))

    for ((op, name) <- genFunctionName.jsBinaryOps) {
      val resultType =
        if (op == IRTrees.JSBinaryOp.=== || op == IRTrees.JSBinaryOp.!==) WasmInt32
        else anyref
      addHelperImport(name, List(anyref, anyref), List(resultType))
    }

    addHelperImport(genFunctionName.newSymbol, Nil, List(anyref))
    addHelperImport(
      genFunctionName.createJSClass,
      List(anyref, anyref, WasmRefType.func, WasmRefType.func, WasmRefType.func, anyref),
      List(WasmRefType.any)
    )
    addHelperImport(
      genFunctionName.createJSClassRest,
      List(anyref, anyref, WasmRefType.func, WasmRefType.func, WasmRefType.func, anyref, WasmInt32),
      List(WasmRefType.any)
    )
    addHelperImport(
      genFunctionName.installJSField,
      List(anyref, anyref, anyref),
      Nil
    )
    addHelperImport(
      genFunctionName.installJSMethod,
      List(anyref, anyref, anyref, WasmRefType.func, WasmInt32),
      Nil
    )
    addHelperImport(
      genFunctionName.installJSStaticMethod,
      List(anyref, anyref, anyref, WasmRefType.func, WasmInt32),
      Nil
    )
    addHelperImport(
      genFunctionName.installJSProperty,
      List(anyref, anyref, anyref, WasmRefType.funcref, WasmRefType.funcref),
      Nil
    )
    addHelperImport(
      genFunctionName.installJSStaticProperty,
      List(anyref, anyref, anyref, WasmRefType.funcref, WasmRefType.funcref),
      Nil
    )
    addHelperImport(
      genFunctionName.jsSuperGet,
      List(anyref, anyref, anyref),
      List(anyref)
    )
    addHelperImport(
      genFunctionName.jsSuperSet,
      List(anyref, anyref, anyref, anyref),
      Nil
    )
    addHelperImport(
      genFunctionName.jsSuperCall,
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

  private def newFunctionBuilder(functionName: WasmFunctionName)(implicit
      ctx: WasmContext
  ): FunctionBuilder = {
    new FunctionBuilder(ctx.moduleBuilder, functionName, noPos)
  }

  private def genStringLiteral()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionName.stringLiteral)
    val offsetParam = fb.addParam("offset", WasmInt32)
    val sizeParam = fb.addParam("size", WasmInt32)
    val stringIndexParam = fb.addParam("stringIndex", WasmInt32)
    fb.setResultType(WasmRefType.any)

    val str = fb.addLocal("str", WasmRefType.any)

    val instrs = fb

    instrs.block(WasmRefType.any) { cacheHit =>
      instrs += GLOBAL_GET(genGlobalName.stringLiteralCache)
      instrs += LOCAL_GET(stringIndexParam)
      instrs += ARRAY_GET(genTypeName.anyArray)

      instrs += BR_ON_NON_NULL(cacheHit)

      // cache miss, create a new string and cache it
      instrs += GLOBAL_GET(genGlobalName.stringLiteralCache)
      instrs += LOCAL_GET(stringIndexParam)

      instrs += LOCAL_GET(offsetParam)
      instrs += LOCAL_GET(sizeParam)
      instrs += ARRAY_NEW_DATA(genTypeName.i16Array, genDataName.string)
      instrs += CALL(genFunctionName.createStringFromData)
      instrs += LOCAL_TEE(str)
      instrs += ARRAY_SET(genTypeName.anyArray)

      instrs += LOCAL_GET(str)
    }

    fb.buildAndAddToModule()
  }

  /** `createStringFromData: (ref array u16) -> (ref any)` (representing a `string`). */
  private def genCreateStringFromData()(implicit ctx: WasmContext): Unit = {
    val dataType = WasmRefType(genTypeName.i16Array)

    val fb = newFunctionBuilder(genFunctionName.createStringFromData)
    val dataParam = fb.addParam("data", dataType)
    fb.setResultType(WasmRefType.any)

    val instrs = fb

    val lenLocal = fb.addLocal("len", WasmInt32)
    val iLocal = fb.addLocal("i", WasmInt32)
    val resultLocal = fb.addLocal("result", WasmRefType.any)

    // len := data.length
    instrs += LOCAL_GET(dataParam)
    instrs += ARRAY_LEN
    instrs += LOCAL_SET(lenLocal)

    // i := 0
    instrs += I32_CONST(0)
    instrs += LOCAL_SET(iLocal)

    // result := ""
    instrs += GLOBAL_GET(genGlobalName.emptyString)
    instrs += LOCAL_SET(resultLocal)

    instrs.loop() { labelLoop =>
      // if i == len
      instrs += LOCAL_GET(iLocal)
      instrs += LOCAL_GET(lenLocal)
      instrs += I32_EQ
      instrs.ifThen() {
        // then return result
        instrs += LOCAL_GET(resultLocal)
        instrs += RETURN
      }

      // result := concat(result, charToString(data(i)))
      instrs += LOCAL_GET(resultLocal)
      instrs += LOCAL_GET(dataParam)
      instrs += LOCAL_GET(iLocal)
      instrs += ARRAY_GET_U(genTypeName.i16Array)
      instrs += CALL(genFunctionName.charToString)
      instrs += CALL(genFunctionName.stringConcat)
      instrs += LOCAL_SET(resultLocal)

      // i := i - 1
      instrs += LOCAL_GET(iLocal)
      instrs += I32_CONST(1)
      instrs += I32_ADD
      instrs += LOCAL_SET(iLocal)

      // loop back to the beginning
      instrs += BR(labelLoop)
    } // end loop $loop
    instrs += UNREACHABLE

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
    val typeDataType = WasmRefType(genTypeName.typeData)
    val nameDataType = WasmRefType(genTypeName.i16Array)

    val fb = newFunctionBuilder(genFunctionName.typeDataName)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(WasmRefType.any)

    val instrs = fb

    val componentTypeDataLocal = fb.addLocal("componentTypeData", typeDataType)
    val componentNameDataLocal = fb.addLocal("componentNameData", nameDataType)
    val firstCharLocal = fb.addLocal("firstChar", WasmInt32)
    val nameLocal = fb.addLocal("name", WasmRefType.any)

    instrs.block(WasmRefType.any) { alreadyInitializedLabel =>
      // br_on_non_null $alreadyInitialized typeData.name
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(genTypeName.typeData, genFieldIdx.typeData.nameIdx)
      instrs += BR_ON_NON_NULL(alreadyInitializedLabel)

      // for the STRUCT_SET typeData.name near the end
      instrs += LOCAL_GET(typeDataParam)

      // if typeData.kind == KindArray
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(genTypeName.typeData, genFieldIdx.typeData.kindIdx)
      instrs += I32_CONST(KindArray)
      instrs += I32_EQ
      instrs.ifThenElse(WasmRefType.any) {
        // it is an array; compute its name from the component type name

        // <top of stack> := "[", for the CALL to stringConcat near the end
        instrs += I32_CONST('['.toInt)
        instrs += CALL(genFunctionName.charToString)

        // componentTypeData := ref_as_non_null(typeData.componentType)
        instrs += LOCAL_GET(typeDataParam)
        instrs += STRUCT_GET(
          genTypeName.typeData,
          genFieldIdx.typeData.componentTypeIdx
        )
        instrs += REF_AS_NOT_NULL
        instrs += LOCAL_SET(componentTypeDataLocal)

        // switch (componentTypeData.kind)
        // the result of this switch is the string that must come after "["
        instrs.switch(WasmRefType.any) { () =>
          // scrutinee
          instrs += LOCAL_GET(componentTypeDataLocal)
          instrs += STRUCT_GET(genTypeName.typeData, genFieldIdx.typeData.kindIdx)
        }(
          List(KindBoolean) -> { () =>
            instrs += I32_CONST('Z'.toInt)
            instrs += CALL(genFunctionName.charToString)
          },
          List(KindChar) -> { () =>
            instrs += I32_CONST('C'.toInt)
            instrs += CALL(genFunctionName.charToString)
          },
          List(KindByte) -> { () =>
            instrs += I32_CONST('B'.toInt)
            instrs += CALL(genFunctionName.charToString)
          },
          List(KindShort) -> { () =>
            instrs += I32_CONST('S'.toInt)
            instrs += CALL(genFunctionName.charToString)
          },
          List(KindInt) -> { () =>
            instrs += I32_CONST('I'.toInt)
            instrs += CALL(genFunctionName.charToString)
          },
          List(KindLong) -> { () =>
            instrs += I32_CONST('J'.toInt)
            instrs += CALL(genFunctionName.charToString)
          },
          List(KindFloat) -> { () =>
            instrs += I32_CONST('F'.toInt)
            instrs += CALL(genFunctionName.charToString)
          },
          List(KindDouble) -> { () =>
            instrs += I32_CONST('D'.toInt)
            instrs += CALL(genFunctionName.charToString)
          },
          List(KindArray) -> { () =>
            // the component type is an array; get its own name
            instrs += LOCAL_GET(componentTypeDataLocal)
            instrs += CALL(genFunctionName.typeDataName)
          }
        ) { () =>
          // default: the component type is neither a primitive nor an array;
          // concatenate "L" + <its own name> + ";"
          instrs += I32_CONST('L'.toInt)
          instrs += CALL(genFunctionName.charToString)
          instrs += LOCAL_GET(componentTypeDataLocal)
          instrs += CALL(genFunctionName.typeDataName)
          instrs += CALL(genFunctionName.stringConcat)
          instrs += I32_CONST(';'.toInt)
          instrs += CALL(genFunctionName.charToString)
          instrs += CALL(genFunctionName.stringConcat)
        }

        // At this point, the stack contains "[" and the string that must be concatenated with it
        instrs += CALL(genFunctionName.stringConcat)
      } {
        // it is not an array; its name is stored in nameData
        for (
          idx <- List(
            genFieldIdx.typeData.nameOffsetIdx,
            genFieldIdx.typeData.nameSizeIdx,
            genFieldIdx.typeData.nameStringIndexIdx
          )
        ) {
          instrs += LOCAL_GET(typeDataParam)
          instrs += STRUCT_GET(genTypeName.typeData, idx)
        }
        instrs += CALL(genFunctionName.stringLiteral)
      }

      // typeData.name := <top of stack> ; leave it on the stack
      instrs += LOCAL_TEE(nameLocal)
      instrs += STRUCT_SET(genTypeName.typeData, genFieldIdx.typeData.nameIdx)
      instrs += LOCAL_GET(nameLocal)
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
    val typeDataType = WasmRefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.createClassOf)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(WasmRefType(genTypeName.ClassStruct))

    val instrs = fb

    val classInstanceLocal = fb.addLocal("classInstance", WasmRefType(genTypeName.ClassStruct))

    // classInstance := newDefault$java.lang.Class()
    // leave it on the stack for the constructor call
    instrs += CALL(genFunctionName.newDefault(IRNames.ClassClass))
    instrs += LOCAL_TEE(classInstanceLocal)

    /* The JS object containing metadata to pass as argument to the `jl.Class` constructor.
     * Specified by https://lampwww.epfl.ch/~doeraene/sjsir-semantics/#sec-sjsir-createclassdataof
     * Leave it on the stack.
     */
    instrs += CALL(genFunctionName.jsNewObject)
    // "__typeData": typeData (TODO hide this better? although nobody will notice anyway)
    instrs ++= ctx.getConstantStringInstr("__typeData")
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(genFunctionName.jsObjectPush)
    // "name": typeDataName(typeData)
    instrs ++= ctx.getConstantStringInstr("name")
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(genFunctionName.typeDataName)
    instrs += CALL(genFunctionName.jsObjectPush)
    // "isPrimitive": (typeData.kind <= KindLastPrimitive)
    instrs ++= ctx.getConstantStringInstr("isPrimitive")
    instrs += LOCAL_GET(typeDataParam)
    instrs += STRUCT_GET(genTypeName.typeData, genFieldIdx.typeData.kindIdx)
    instrs += I32_CONST(KindLastPrimitive)
    instrs += I32_LE_U
    instrs += CALL(genFunctionName.box(IRTypes.BooleanRef))
    instrs += CALL(genFunctionName.jsObjectPush)
    // "isArrayClass": (typeData.kind == KindArray)
    instrs ++= ctx.getConstantStringInstr("isArrayClass")
    instrs += LOCAL_GET(typeDataParam)
    instrs += STRUCT_GET(genTypeName.typeData, genFieldIdx.typeData.kindIdx)
    instrs += I32_CONST(KindArray)
    instrs += I32_EQ
    instrs += CALL(genFunctionName.box(IRTypes.BooleanRef))
    instrs += CALL(genFunctionName.jsObjectPush)
    // "isInterface": (typeData.kind == KindInterface)
    instrs ++= ctx.getConstantStringInstr("isInterface")
    instrs += LOCAL_GET(typeDataParam)
    instrs += STRUCT_GET(genTypeName.typeData, genFieldIdx.typeData.kindIdx)
    instrs += I32_CONST(KindInterface)
    instrs += I32_EQ
    instrs += CALL(genFunctionName.box(IRTypes.BooleanRef))
    instrs += CALL(genFunctionName.jsObjectPush)
    // "isInstance": closure(isInstance, typeData)
    instrs ++= ctx.getConstantStringInstr("isInstance")
    instrs += ctx.refFuncWithDeclaration(genFunctionName.isInstance)
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(genFunctionName.closure)
    instrs += CALL(genFunctionName.jsObjectPush)
    // "isAssignableFrom": closure(isAssignableFrom, typeData)
    instrs ++= ctx.getConstantStringInstr("isAssignableFrom")
    instrs += ctx.refFuncWithDeclaration(genFunctionName.isAssignableFromExternal)
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(genFunctionName.closure)
    instrs += CALL(genFunctionName.jsObjectPush)
    // "checkCast": closure(checkCast, typeData)
    instrs ++= ctx.getConstantStringInstr("checkCast")
    instrs += ctx.refFuncWithDeclaration(genFunctionName.checkCast)
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(genFunctionName.closure)
    instrs += CALL(genFunctionName.jsObjectPush)
    // "isAssignableFrom": closure(isAssignableFrom, typeData)
    // "getComponentType": closure(getComponentType, typeData)
    instrs ++= ctx.getConstantStringInstr("getComponentType")
    instrs += ctx.refFuncWithDeclaration(genFunctionName.getComponentType)
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(genFunctionName.closure)
    instrs += CALL(genFunctionName.jsObjectPush)
    // "newArrayOfThisClass": closure(newArrayOfThisClass, typeData)
    instrs ++= ctx.getConstantStringInstr("newArrayOfThisClass")
    instrs += ctx.refFuncWithDeclaration(genFunctionName.newArrayOfThisClass)
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(genFunctionName.closure)
    instrs += CALL(genFunctionName.jsObjectPush)

    // Call java.lang.Class::<init>(dataObject)
    instrs += CALL(
      genFunctionName.forMethod(
        IRTrees.MemberNamespace.Constructor,
        IRNames.ClassClass,
        SpecialNames.ClassCtor
      )
    )

    // typeData.classOf := classInstance
    instrs += LOCAL_GET(typeDataParam)
    instrs += LOCAL_GET(classInstanceLocal)
    instrs += STRUCT_SET(genTypeName.typeData, genFieldIdx.typeData.classOfIdx)

    // <top-of-stack> := classInstance for the implicit return
    instrs += LOCAL_GET(classInstanceLocal)

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
    val typeDataType = WasmRefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.getClassOf)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(WasmRefType(genTypeName.ClassStruct))

    val instrs = fb

    instrs.block(WasmRefType(genTypeName.ClassStruct)) { alreadyInitializedLabel =>
      // fast path
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(genTypeName.typeData, genFieldIdx.typeData.classOfIdx)
      instrs += BR_ON_NON_NULL(alreadyInitializedLabel)
      // slow path
      instrs += LOCAL_GET(typeDataParam)
      instrs += CALL(genFunctionName.createClassOf)
    } // end bock alreadyInitializedLabel

    fb.buildAndAddToModule()
  }

  /** `arrayTypeData: (ref typeData), i32 -> (ref $java.lang.Object___vtable)`.
    *
    * Returns the typeData/vtable of an array with `dims` dimensions over the given typeData. `dims`
    * must be be strictly positive.
    */
  private def genArrayTypeData()(implicit ctx: WasmContext): Unit = {
    val typeDataType = WasmRefType(genTypeName.typeData)
    val objectVTableType = WasmRefType(genTypeName.ObjectVTable)

    /* Array classes extend Cloneable, Serializable and Object.
     * Filter out the ones that do not have run-time type info at all, as
     * we do for other classes.
     */
    val strictAncestors =
      List(IRNames.CloneableClass, IRNames.SerializableClass, IRNames.ObjectClass)
        .filter(name => ctx.getClassInfoOption(name).exists(_.hasRuntimeTypeInfo))

    val fb = newFunctionBuilder(genFunctionName.arrayTypeData)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val dimsParam = fb.addParam("dims", WasmInt32)
    fb.setResultType(objectVTableType)

    val instrs = fb

    val arrayTypeDataLocal = fb.addLocal("arrayTypeData", objectVTableType)

    instrs.loop() { loopLabel =>
      instrs.block(objectVTableType) { arrayOfIsNonNullLabel =>
        // br_on_non_null $arrayOfIsNonNull typeData.arrayOf
        instrs += LOCAL_GET(typeDataParam)
        instrs += STRUCT_GET(
          genTypeName.typeData,
          genFieldIdx.typeData.arrayOfIdx
        )
        instrs += BR_ON_NON_NULL(arrayOfIsNonNullLabel)

        // <top-of-stack> := typeData ; for the <old typeData>.arrayOf := ... later on
        instrs += LOCAL_GET(typeDataParam)

        // typeData := new typeData(...)
        instrs += I32_CONST(0) // nameOffset
        instrs += I32_CONST(0) // nameSize
        instrs += I32_CONST(0) // nameStringIndex
        instrs += I32_CONST(KindArray) // kind = KindArray
        instrs += I32_CONST(0) // specialInstanceTypes = 0

        // strictAncestors
        for (strictAncestor <- strictAncestors)
          instrs += GLOBAL_GET(genGlobalName.forVTable(strictAncestor))
        instrs += ARRAY_NEW_FIXED(
          genTypeName.typeDataArray,
          strictAncestors.size
        )

        instrs += LOCAL_GET(typeDataParam) // componentType
        instrs += REF_NULL(WasmHeapType.None) // name
        instrs += REF_NULL(WasmHeapType.None) // classOf
        instrs += REF_NULL(WasmHeapType.None) // arrayOf

        // clone
        instrs.switch(WasmRefType(genTypeName.cloneFunctionType)) { () =>
          instrs += LOCAL_GET(typeDataParam)
          instrs += STRUCT_GET(genTypeName.typeData, genFieldIdx.typeData.kindIdx)
        }(
          List(KindBoolean) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(IRTypes.BooleanRef))
          },
          List(KindChar) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(IRTypes.CharRef))
          },
          List(KindByte) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(IRTypes.ByteRef))
          },
          List(KindShort) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(IRTypes.ShortRef))
          },
          List(KindInt) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(IRTypes.IntRef))
          },
          List(KindLong) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(IRTypes.LongRef))
          },
          List(KindFloat) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(IRTypes.FloatRef))
          },
          List(KindDouble) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(IRTypes.DoubleRef))
          }
        ) { () =>
          instrs += ctx.refFuncWithDeclaration(
            genFunctionName.clone(IRTypes.ClassRef(IRNames.ObjectClass))
          )
        }

        // isJSClassInstance
        instrs += REF_NULL(WasmHeapType.NoFunc)

        // reflectiveProxies
        instrs += ARRAY_NEW_FIXED(genTypeName.reflectiveProxies, 0) // TODO

        val objectClassInfo = ctx.getClassInfo(IRNames.ObjectClass)
        instrs ++= objectClassInfo.tableEntries.map { methodName =>
          ctx.refFuncWithDeclaration(objectClassInfo.resolvedMethodInfos(methodName).tableEntryName)
        }
        instrs += STRUCT_NEW(genTypeName.ObjectVTable)
        instrs += LOCAL_TEE(arrayTypeDataLocal)

        // <old typeData>.arrayOf := typeData
        instrs += STRUCT_SET(
          genTypeName.typeData,
          genFieldIdx.typeData.arrayOfIdx
        )

        // put arrayTypeData back on the stack
        instrs += LOCAL_GET(arrayTypeDataLocal)
      } // end block $arrayOfIsNonNullLabel

      // dims := dims - 1 -- leave dims on the stack
      instrs += LOCAL_GET(dimsParam)
      instrs += I32_CONST(1)
      instrs += I32_SUB
      instrs += LOCAL_TEE(dimsParam)

      // if dims == 0 then
      //   return typeData.arrayOf (which is on the stack)
      instrs += I32_EQZ
      instrs.ifThen(WasmFunctionSignature(List(objectVTableType), List(objectVTableType))) {
        instrs += RETURN
      }

      // typeData := typeData.arrayOf (which is on the stack), then loop back to the beginning
      instrs += LOCAL_SET(typeDataParam)
      instrs += BR(loopLabel)
    } // end loop $loop
    instrs += UNREACHABLE

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
    import genFieldIdx.typeData._

    val typeDataType = WasmRefType(genTypeName.typeData)
    val objectRefType = WasmRefType(genTypeName.forClass(IRNames.ObjectClass))

    val fb = newFunctionBuilder(genFunctionName.isInstance)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val valueParam = fb.addParam("value", WasmRefType.anyref)
    fb.setResultType(WasmInt32)

    val instrs = fb

    val valueNonNullLocal = fb.addLocal("valueNonNull", WasmRefType.any)
    val specialInstanceTypesLocal = fb.addLocal("specialInstanceTypes", WasmInt32)

    // switch (typeData.kind)
    instrs.switch(WasmInt32) { () =>
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(genTypeName.typeData, kindIdx)
    }(
      // case anyPrimitiveKind => false
      (KindVoid to KindLastPrimitive).toList -> { () =>
        instrs += I32_CONST(0)
      },
      // case KindObject => value ne null
      List(KindObject) -> { () =>
        instrs += LOCAL_GET(valueParam)
        instrs += REF_IS_NULL
        instrs += I32_EQZ
      },
      // for each boxed class, the corresponding primitive type test
      List(KindBoxedUnit) -> { () =>
        instrs += LOCAL_GET(valueParam)
        instrs += CALL(genFunctionName.isUndef)
      },
      List(KindBoxedBoolean) -> { () =>
        instrs += LOCAL_GET(valueParam)
        instrs += CALL(genFunctionName.typeTest(IRTypes.BooleanRef))
      },
      List(KindBoxedCharacter) -> { () =>
        instrs += LOCAL_GET(valueParam)
        val structTypeName = genTypeName.forClass(SpecialNames.CharBoxClass)
        instrs += REF_TEST(WasmRefType(structTypeName))
      },
      List(KindBoxedByte) -> { () =>
        instrs += LOCAL_GET(valueParam)
        instrs += CALL(genFunctionName.typeTest(IRTypes.ByteRef))
      },
      List(KindBoxedShort) -> { () =>
        instrs += LOCAL_GET(valueParam)
        instrs += CALL(genFunctionName.typeTest(IRTypes.ShortRef))
      },
      List(KindBoxedInteger) -> { () =>
        instrs += LOCAL_GET(valueParam)
        instrs += CALL(genFunctionName.typeTest(IRTypes.IntRef))
      },
      List(KindBoxedLong) -> { () =>
        instrs += LOCAL_GET(valueParam)
        val structTypeName = genTypeName.forClass(SpecialNames.LongBoxClass)
        instrs += REF_TEST(WasmRefType(structTypeName))
      },
      List(KindBoxedFloat) -> { () =>
        instrs += LOCAL_GET(valueParam)
        instrs += CALL(genFunctionName.typeTest(IRTypes.FloatRef))
      },
      List(KindBoxedDouble) -> { () =>
        instrs += LOCAL_GET(valueParam)
        instrs += CALL(genFunctionName.typeTest(IRTypes.DoubleRef))
      },
      List(KindBoxedString) -> { () =>
        instrs += LOCAL_GET(valueParam)
        instrs += CALL(genFunctionName.isString)
      },
      // case KindJSType => call typeData.isJSClassInstance(value) or throw if it is null
      List(KindJSType) -> { () =>
        instrs.block(WasmRefType.anyref) { isJSClassInstanceIsNull =>
          // Load value as the argument to the function
          instrs += LOCAL_GET(valueParam)

          // Load the function reference; break if null
          instrs += LOCAL_GET(typeDataParam)
          instrs += STRUCT_GET(genTypeName.typeData, isJSClassInstanceIdx)
          instrs += BR_ON_NULL(isJSClassInstanceIsNull)

          // Call the function
          instrs += CALL_REF(genTypeName.isJSClassInstanceFuncType)
          instrs += RETURN
        }
        instrs += DROP // drop `value` which was left on the stack

        // throw new TypeError("...")
        instrs ++= ctx.getConstantStringInstr("TypeError")
        instrs += CALL(genFunctionName.jsGlobalRefGet)
        instrs += CALL(genFunctionName.jsNewArray)
        instrs ++= ctx.getConstantStringInstr(
          "Cannot call isInstance() on a Class representing a JS trait/object"
        )
        instrs += CALL(genFunctionName.jsArrayPush)
        instrs += CALL(genFunctionName.jsNew)
        instrs += EXTERN_CONVERT_ANY
        instrs += THROW(genTagName.exceptionTagName)
      }
    ) { () =>
      // case _ =>

      // valueNonNull := as_non_null value; return false if null
      instrs.block(WasmRefType.any) { nonNullLabel =>
        instrs += LOCAL_GET(valueParam)
        instrs += BR_ON_NON_NULL(nonNullLabel)
        instrs += I32_CONST(0)
        instrs += RETURN
      }
      instrs += LOCAL_SET(valueNonNullLocal)

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
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(genTypeName.typeData, specialInstanceTypesIdx)
      instrs += LOCAL_TEE(specialInstanceTypesLocal)
      instrs += I32_CONST(0)
      instrs += I32_NE
      instrs.ifThen() {
        // Load (1 << jsValueType(valueNonNull))
        instrs += I32_CONST(1)
        instrs += LOCAL_GET(valueNonNullLocal)
        instrs += CALL(genFunctionName.jsValueType)
        instrs += I32_SHL

        // if ((... & specialInstanceTypes) != 0)
        instrs += LOCAL_GET(specialInstanceTypesLocal)
        instrs += I32_AND
        instrs += I32_CONST(0)
        instrs += I32_NE
        instrs.ifThen() {
          // then return true
          instrs += I32_CONST(1)
          instrs += RETURN
        }
      }

      // Get the vtable and delegate to isAssignableFrom

      // Load typeData
      instrs += LOCAL_GET(typeDataParam)

      // Load the vtable; return false if it is not one of our object
      instrs.block(objectRefType) { ourObjectLabel =>
        // Try cast to jl.Object
        instrs += LOCAL_GET(valueNonNullLocal)
        instrs += BR_ON_CAST(
          ourObjectLabel,
          WasmRefType.any,
          WasmRefType(objectRefType.heapType)
        )

        // on cast fail, return false
        instrs += I32_CONST(0)
        instrs += RETURN
      }
      instrs += STRUCT_GET(
        genTypeName.forClass(IRNames.ObjectClass),
        genFieldIdx.objStruct.vtable
      )

      // Call isAssignableFrom
      instrs += CALL(genFunctionName.isAssignableFrom)
    }

    fb.buildAndAddToModule()
  }

  /** `isAssignableFromExternal: (ref typeData), anyref -> i32` (a boolean).
    *
    * This is the underlying func for the `isAssignableFrom()` closure inside class data objects.
    */
  private def genIsAssignableFromExternal()(implicit ctx: WasmContext): Unit = {
    val typeDataType = WasmRefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.isAssignableFromExternal)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val fromParam = fb.addParam("from", WasmRefType.anyref)
    fb.setResultType(WasmInt32)

    val instrs = fb

    // load typeData
    instrs += LOCAL_GET(typeDataParam)

    // load ref.cast<typeData> from["__typeData"] (as a JS selection)
    instrs += LOCAL_GET(fromParam)
    instrs ++= ctx.getConstantStringInstr("__typeData")
    instrs += CALL(genFunctionName.jsSelect)
    instrs += REF_CAST(WasmRefType(typeDataType.heapType))

    // delegate to isAssignableFrom
    instrs += CALL(genFunctionName.isAssignableFrom)

    fb.buildAndAddToModule()
  }

  /** `isAssignableFrom: (ref typeData), (ref typeData) -> i32` (a boolean).
    *
    * Specified by `java.lang.Class.isAssignableFrom(Class)`.
    */
  private def genIsAssignableFrom()(implicit ctx: WasmContext): Unit = {
    import genFieldIdx.typeData._

    val typeDataType = WasmRefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.isAssignableFrom)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val fromTypeDataParam = fb.addParam("fromTypeData", typeDataType)
    fb.setResultType(WasmInt32)

    val instrs = fb

    val fromAncestorsLocal = fb.addLocal("fromAncestors", WasmRefType(genTypeName.typeDataArray))
    val lenLocal = fb.addLocal("len", WasmInt32)
    val iLocal = fb.addLocal("i", WasmInt32)

    // if (fromTypeData eq typeData)
    instrs += LOCAL_GET(fromTypeDataParam)
    instrs += LOCAL_GET(typeDataParam)
    instrs += REF_EQ
    instrs.ifThen() {
      // then return true
      instrs += I32_CONST(1)
      instrs += RETURN
    }

    // "Tail call" loop for diving into array component types
    instrs.loop(WasmInt32) { loopForArrayLabel =>
      // switch (typeData.kind)
      instrs.switch(WasmInt32) { () =>
        // typeData.kind
        instrs += LOCAL_GET(typeDataParam)
        instrs += STRUCT_GET(genTypeName.typeData, kindIdx)
      }(
        // case anyPrimitiveKind => return false
        (KindVoid to KindLastPrimitive).toList -> { () =>
          instrs += I32_CONST(0)
        },
        // case KindArray => check that from is an array, recurse into component types
        List(KindArray) -> { () =>
          instrs.block() { fromComponentTypeIsNullLabel =>
            // fromTypeData := fromTypeData.componentType; jump out if null
            instrs += LOCAL_GET(fromTypeDataParam)
            instrs += STRUCT_GET(genTypeName.typeData, componentTypeIdx)
            instrs += BR_ON_NULL(fromComponentTypeIsNullLabel)
            instrs += LOCAL_SET(fromTypeDataParam)

            // typeData := ref.as_non_null typeData.componentType (OK because KindArray)
            instrs += LOCAL_GET(typeDataParam)
            instrs += STRUCT_GET(genTypeName.typeData, componentTypeIdx)
            instrs += REF_AS_NOT_NULL
            instrs += LOCAL_SET(typeDataParam)

            // loop back ("tail call")
            instrs += BR(loopForArrayLabel)
          }

          // return false
          instrs += I32_CONST(0)
        },
        // case KindObject => return (fromTypeData.kind > KindLastPrimitive)
        List(KindObject) -> { () =>
          instrs += LOCAL_GET(fromTypeDataParam)
          instrs += STRUCT_GET(genTypeName.typeData, kindIdx)
          instrs += I32_CONST(KindLastPrimitive)
          instrs += I32_GT_U
        }
      ) { () =>
        // All other cases: test whether `fromTypeData.strictAncestors` contains `typeData`

        instrs.block() { fromAncestorsIsNullLabel =>
          // fromAncestors := fromTypeData.strictAncestors; go to fromAncestorsIsNull if null
          instrs += LOCAL_GET(fromTypeDataParam)
          instrs += STRUCT_GET(genTypeName.typeData, strictAncestorsIdx)
          instrs += BR_ON_NULL(fromAncestorsIsNullLabel)
          instrs += LOCAL_TEE(fromAncestorsLocal)

          // if fromAncestors contains typeData, return true

          // len := fromAncestors.length
          instrs += ARRAY_LEN
          instrs += LOCAL_SET(lenLocal)

          // i := 0
          instrs += I32_CONST(0)
          instrs += LOCAL_SET(iLocal)

          // while (i != len)
          instrs.whileLoop() {
            instrs += LOCAL_GET(iLocal)
            instrs += LOCAL_GET(lenLocal)
            instrs += I32_NE
          } {
            // if (fromAncestors[i] eq typeData)
            instrs += LOCAL_GET(fromAncestorsLocal)
            instrs += LOCAL_GET(iLocal)
            instrs += ARRAY_GET(genTypeName.typeDataArray)
            instrs += LOCAL_GET(typeDataParam)
            instrs += REF_EQ
            instrs.ifThen() {
              // then return true
              instrs += I32_CONST(1)
              instrs += RETURN
            }

            // i := i + 1
            instrs += LOCAL_GET(iLocal)
            instrs += I32_CONST(1)
            instrs += I32_ADD
            instrs += LOCAL_SET(iLocal)
          }
        }

        // from.strictAncestors is null or does not contain typeData
        // return false
        instrs += I32_CONST(0)
      }
    }

    fb.buildAndAddToModule()
  }

  /** `checkCast: (ref typeData), anyref -> anyref`.
    *
    * Casts the given value to the given type; subject to undefined behaviors.
    */
  private def genCheckCast()(implicit ctx: WasmContext): Unit = {
    val typeDataType = WasmRefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.checkCast)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val valueParam = fb.addParam("value", WasmRefType.anyref)
    fb.setResultType(WasmRefType.anyref)

    val instrs = fb

    /* Given that we only implement `CheckedBehavior.Unchecked` semantics for
     * now, this is always the identity.
     */

    instrs += LOCAL_GET(valueParam)

    fb.buildAndAddToModule()
  }

  /** `getComponentType: (ref typeData) -> (ref null jlClass)`.
    *
    * This is the underlying func for the `getComponentType()` closure inside class data objects.
    */
  private def genGetComponentType()(implicit ctx: WasmContext): Unit = {
    val typeDataType = WasmRefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.getComponentType)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(WasmRefType.nullable(genTypeName.ClassStruct))

    val instrs = fb

    val componentTypeDataLocal = fb.addLocal("componentTypeData", typeDataType)

    instrs.block() { nullResultLabel =>
      // Try and extract non-null component type data
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(
        genTypeName.typeData,
        genFieldIdx.typeData.componentTypeIdx
      )
      instrs += BR_ON_NULL(nullResultLabel)
      // Get the corresponding classOf
      instrs += CALL(genFunctionName.getClassOf)
      instrs += RETURN
    } // end block nullResultLabel
    instrs += REF_NULL(WasmHeapType(genTypeName.ClassStruct))

    fb.buildAndAddToModule()
  }

  /** `newArrayOfThisClass: (ref typeData), anyref -> (ref jlObject)`.
    *
    * This is the underlying func for the `newArrayOfThisClass()` closure inside class data objects.
    */
  private def genNewArrayOfThisClass()(implicit ctx: WasmContext): Unit = {
    val typeDataType = WasmRefType(genTypeName.typeData)
    val i32ArrayType = WasmRefType(genTypeName.i32Array)

    val fb = newFunctionBuilder(genFunctionName.newArrayOfThisClass)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val lengthsParam = fb.addParam("lengths", WasmRefType.anyref)
    fb.setResultType(WasmRefType(genTypeName.ObjectStruct))

    val instrs = fb

    val lengthsLenLocal = fb.addLocal("lengthsLenLocal", WasmInt32)
    val lengthsValuesLocal = fb.addLocal("lengthsValues", i32ArrayType)
    val iLocal = fb.addLocal("i", WasmInt32)

    // lengthsLen := lengths.length // as a JS field access
    instrs += LOCAL_GET(lengthsParam)
    instrs ++= ctx.getConstantStringInstr("length")
    instrs += CALL(genFunctionName.jsSelect)
    instrs += CALL(genFunctionName.unbox(IRTypes.IntRef))
    instrs += LOCAL_TEE(lengthsLenLocal)

    // lengthsValues := array.new<i32Array> lengthsLen
    instrs += ARRAY_NEW_DEFAULT(genTypeName.i32Array)
    instrs += LOCAL_SET(lengthsValuesLocal)

    // i := 0
    instrs += I32_CONST(0)
    instrs += LOCAL_SET(iLocal)

    // while (i != lengthsLen)
    instrs.whileLoop() {
      instrs += LOCAL_GET(iLocal)
      instrs += LOCAL_GET(lengthsLenLocal)
      instrs += I32_NE
    } {
      // lengthsValue[i] := lengths[i] (where the rhs is a JS field access)

      instrs += LOCAL_GET(lengthsValuesLocal)
      instrs += LOCAL_GET(iLocal)

      instrs += LOCAL_GET(lengthsParam)
      instrs += LOCAL_GET(iLocal)
      instrs += REF_I31
      instrs += CALL(genFunctionName.jsSelect)
      instrs += CALL(genFunctionName.unbox(IRTypes.IntRef))

      instrs += ARRAY_SET(genTypeName.i32Array)

      // i += 1
      instrs += LOCAL_GET(iLocal)
      instrs += I32_CONST(1)
      instrs += I32_ADD
      instrs += LOCAL_SET(iLocal)
    }

    // return newArrayObject(arrayTypeData(typeData, lengthsLen), lengthsValues, 0)
    instrs += LOCAL_GET(typeDataParam)
    instrs += LOCAL_GET(lengthsLenLocal)
    instrs += CALL(genFunctionName.arrayTypeData)
    instrs += LOCAL_GET(lengthsValuesLocal)
    instrs += I32_CONST(0)
    instrs += CALL(genFunctionName.newArrayObject)

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
    val typeDataType = WasmRefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.anyGetClass)
    val valueParam = fb.addParam("value", WasmRefType.any)
    fb.setResultType(WasmRefType.nullable(genTypeName.ClassStruct))

    val instrs = fb

    val typeDataLocal = fb.addLocal("typeData", typeDataType)
    val doubleValueLocal = fb.addLocal("doubleValue", WasmFloat64)
    val intValueLocal = fb.addLocal("intValue", WasmInt32)
    val ourObjectLocal = fb.addLocal("ourObject", WasmRefType(genTypeName.ObjectStruct))

    def getHijackedClassTypeDataInstr(className: IRNames.ClassName): WasmInstr =
      GLOBAL_GET(genGlobalName.forVTable(className))

    instrs.block(WasmRefType.nullable(genTypeName.ClassStruct)) { nonNullClassOfLabel =>
      instrs.block(typeDataType) { gotTypeDataLabel =>
        instrs.block(WasmRefType(genTypeName.ObjectStruct)) { ourObjectLabel =>
          // if value is our object, jump to $ourObject
          instrs += LOCAL_GET(valueParam)
          instrs += BR_ON_CAST(
            ourObjectLabel,
            WasmRefType.any,
            WasmRefType(genTypeName.ObjectStruct)
          )

          // switch(jsValueType(value)) { ... }
          instrs.switch(typeDataType) { () =>
            // scrutinee
            instrs += LOCAL_GET(valueParam)
            instrs += CALL(genFunctionName.jsValueType)
          }(
            // case JSValueTypeFalse, JSValueTypeTrue => typeDataOf[jl.Boolean]
            List(JSValueTypeFalse, JSValueTypeTrue) -> { () =>
              instrs += getHijackedClassTypeDataInstr(IRNames.BoxedBooleanClass)
            },
            // case JSValueTypeString => typeDataOf[jl.String]
            List(JSValueTypeString) -> { () =>
              instrs += getHijackedClassTypeDataInstr(IRNames.BoxedStringClass)
            },
            // case JSValueTypeNumber => ...
            List(JSValueTypeNumber) -> { () =>
              /* For `number`s, the result is based on the actual value, as specified by
               * [[https://www.scala-js.org/doc/semantics.html#getclass]].
               */

              // doubleValue := unboxDouble(value)
              instrs += LOCAL_GET(valueParam)
              instrs += CALL(genFunctionName.unbox(IRTypes.DoubleRef))
              instrs += LOCAL_TEE(doubleValueLocal)

              // intValue := doubleValue.toInt
              instrs += I32_TRUNC_SAT_F64_S
              instrs += LOCAL_TEE(intValueLocal)

              // if same(intValue.toDouble, doubleValue) -- same bit pattern to avoid +0.0 == -0.0
              instrs += F64_CONVERT_I32_S
              instrs += I64_REINTERPRET_F64
              instrs += LOCAL_GET(doubleValueLocal)
              instrs += I64_REINTERPRET_F64
              instrs += I64_EQ
              instrs.ifThenElse(typeDataType) {
                // then it is a Byte, a Short, or an Integer

                // if intValue.toByte.toInt == intValue
                instrs += LOCAL_GET(intValueLocal)
                instrs += I32_EXTEND8_S
                instrs += LOCAL_GET(intValueLocal)
                instrs += I32_EQ
                instrs.ifThenElse(typeDataType) {
                  // then it is a Byte
                  instrs += getHijackedClassTypeDataInstr(IRNames.BoxedByteClass)
                } {
                  // else, if intValue.toShort.toInt == intValue
                  instrs += LOCAL_GET(intValueLocal)
                  instrs += I32_EXTEND16_S
                  instrs += LOCAL_GET(intValueLocal)
                  instrs += I32_EQ
                  instrs.ifThenElse(typeDataType) {
                    // then it is a Short
                    instrs += getHijackedClassTypeDataInstr(IRNames.BoxedShortClass)
                  } {
                    // else, it is an Integer
                    instrs += getHijackedClassTypeDataInstr(IRNames.BoxedIntegerClass)
                  }
                }
              } {
                // else, it is a Float or a Double

                // if doubleValue.toFloat.toDouble == doubleValue
                instrs += LOCAL_GET(doubleValueLocal)
                instrs += F32_DEMOTE_F64
                instrs += F64_PROMOTE_F32
                instrs += LOCAL_GET(doubleValueLocal)
                instrs += F64_EQ
                instrs.ifThenElse(typeDataType) {
                  // then it is a Float
                  instrs += getHijackedClassTypeDataInstr(IRNames.BoxedFloatClass)
                } {
                  // else, if it is NaN
                  instrs += LOCAL_GET(doubleValueLocal)
                  instrs += LOCAL_GET(doubleValueLocal)
                  instrs += F64_NE
                  instrs.ifThenElse(typeDataType) {
                    // then it is a Float
                    instrs += getHijackedClassTypeDataInstr(IRNames.BoxedFloatClass)
                  } {
                    // else, it is a Double
                    instrs += getHijackedClassTypeDataInstr(IRNames.BoxedDoubleClass)
                  }
                }
              }
            },
            // case JSValueTypeUndefined => typeDataOf[jl.Void]
            List(JSValueTypeUndefined) -> { () =>
              instrs += getHijackedClassTypeDataInstr(IRNames.BoxedUnitClass)
            }
          ) { () =>
            // case _ (JSValueTypeOther) => return null
            instrs += REF_NULL(WasmHeapType(genTypeName.ClassStruct))
            instrs += RETURN
          }

          instrs += BR(gotTypeDataLabel)
        }

        /* Now we have one of our objects. Normally we only have to get the
         * vtable, but there are two exceptions. If the value is an instance of
         * `jl.CharacterBox` or `jl.LongBox`, we must use the typeData of
         * `jl.Character` or `jl.Long`, respectively.
         */
        instrs += LOCAL_TEE(ourObjectLocal)
        instrs += REF_TEST(WasmRefType(genTypeName.forClass(SpecialNames.CharBoxClass)))
        instrs.ifThenElse(typeDataType) {
          instrs += getHijackedClassTypeDataInstr(IRNames.BoxedCharacterClass)
        } {
          instrs += LOCAL_GET(ourObjectLocal)
          instrs += REF_TEST(WasmRefType(genTypeName.forClass(SpecialNames.LongBoxClass)))
          instrs.ifThenElse(typeDataType) {
            instrs += getHijackedClassTypeDataInstr(IRNames.BoxedLongClass)
          } {
            instrs += LOCAL_GET(ourObjectLocal)
            instrs += STRUCT_GET(
              genTypeName.forClass(IRNames.ObjectClass),
              genFieldIdx.objStruct.vtable
            )
          }
        }
      }

      instrs += CALL(genFunctionName.getClassOf)
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
    import genFieldIdx.typeData._

    val typeDataType = WasmRefType(genTypeName.typeData)
    val i32ArrayType = WasmRefType(genTypeName.i32Array)
    val objectVTableType = WasmRefType(genTypeName.ObjectVTable)
    val arrayTypeDataType = objectVTableType
    val itablesType = WasmRefType.nullable(genTypeName.itables)
    val nonNullObjectType = WasmRefType(genTypeName.ObjectStruct)
    val anyArrayType = WasmRefType(genTypeName.anyArray)

    val fb = newFunctionBuilder(genFunctionName.newArrayObject)
    val arrayTypeDataParam = fb.addParam("arrayTypeData", arrayTypeDataType)
    val lengthsParam = fb.addParam("lengths", i32ArrayType)
    val lengthIndexParam = fb.addParam("lengthIndex", WasmInt32)
    fb.setResultType(nonNullObjectType)

    val instrs = fb

    val lenLocal = fb.addLocal("len", WasmInt32)
    val underlyingLocal = fb.addLocal("underlying", anyArrayType)
    val subLengthIndexLocal = fb.addLocal("subLengthIndex", WasmInt32)
    val arrayComponentTypeDataLocal = fb.addLocal("arrayComponentTypeData", arrayTypeDataType)
    val iLocal = fb.addLocal("i", WasmInt32)

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
      IRTypes.BooleanRef -> KindBoolean,
      IRTypes.CharRef -> KindChar,
      IRTypes.ByteRef -> KindByte,
      IRTypes.ShortRef -> KindShort,
      IRTypes.IntRef -> KindInt,
      IRTypes.LongRef -> KindLong,
      IRTypes.FloatRef -> KindFloat,
      IRTypes.DoubleRef -> KindDouble
    )

    // Load the vtable and itable or the resulting array on the stack
    instrs += LOCAL_GET(arrayTypeDataParam) // vtable
    instrs += GLOBAL_GET(genGlobalName.arrayClassITable) // itable

    // Load the first length
    instrs += LOCAL_GET(lengthsParam)
    instrs += LOCAL_GET(lengthIndexParam)
    instrs += ARRAY_GET(genTypeName.i32Array)

    // componentTypeData := ref_as_non_null(arrayTypeData.componentType)
    // switch (componentTypeData.kind)
    val switchClauseSig = WasmFunctionSignature(
      List(arrayTypeDataType, itablesType, WasmInt32),
      List(nonNullObjectType)
    )
    instrs.switch(switchClauseSig) { () =>
      // scrutinee
      instrs += LOCAL_GET(arrayTypeDataParam)
      instrs += STRUCT_GET(
        genTypeName.typeData,
        genFieldIdx.typeData.componentTypeIdx
      )
      instrs += STRUCT_GET(
        genTypeName.typeData,
        genFieldIdx.typeData.kindIdx
      )
    }(
      // For all the primitive types, by construction, this is the bottom dimension
      // case KindPrim => array.new_default underlyingPrimArray; struct.new PrimArray
      primRefsWithArrayTypes.map { case (primRef, kind) =>
        List(kind) -> { () =>
          val arrayTypeRef = IRTypes.ArrayTypeRef(primRef, 1)
          instrs += ARRAY_NEW_DEFAULT(genTypeName.underlyingOf(arrayTypeRef))
          instrs += STRUCT_NEW(genTypeName.forArrayClass(arrayTypeRef))
          () // required for correct type inference
        }
      }: _*
    ) { () =>
      // default -- all non-primitive array types

      // len := <top-of-stack> (which is the first length)
      instrs += LOCAL_TEE(lenLocal)

      // underlying := array.new_default anyArray
      val arrayTypeRef = IRTypes.ArrayTypeRef(IRTypes.ClassRef(IRNames.ObjectClass), 1)
      instrs += ARRAY_NEW_DEFAULT(genTypeName.underlyingOf(arrayTypeRef))
      instrs += LOCAL_SET(underlyingLocal)

      // subLengthIndex := lengthIndex + 1
      instrs += LOCAL_GET(lengthIndexParam)
      instrs += I32_CONST(1)
      instrs += I32_ADD
      instrs += LOCAL_TEE(subLengthIndexLocal)

      // if subLengthIndex != lengths.length
      instrs += LOCAL_GET(lengthsParam)
      instrs += ARRAY_LEN
      instrs += I32_NE
      instrs.ifThen() {
        // then, recursively initialize all the elements

        // arrayComponentTypeData := ref_cast<arrayTypeDataType> arrayTypeData.componentTypeData
        instrs += LOCAL_GET(arrayTypeDataParam)
        instrs += STRUCT_GET(
          genTypeName.typeData,
          genFieldIdx.typeData.componentTypeIdx
        )
        instrs += REF_CAST(WasmRefType(arrayTypeDataType.heapType))
        instrs += LOCAL_SET(arrayComponentTypeDataLocal)

        // i := 0
        instrs += I32_CONST(0)
        instrs += LOCAL_SET(iLocal)

        // while (i != len)
        instrs.whileLoop() {
          instrs += LOCAL_GET(iLocal)
          instrs += LOCAL_GET(lenLocal)
          instrs += I32_NE
        } {
          // underlying[i] := newArrayObject(arrayComponentType, lengths, subLengthIndex)

          instrs += LOCAL_GET(underlyingLocal)
          instrs += LOCAL_GET(iLocal)

          instrs += LOCAL_GET(arrayComponentTypeDataLocal)
          instrs += LOCAL_GET(lengthsParam)
          instrs += LOCAL_GET(subLengthIndexLocal)
          instrs += CALL(genFunctionName.newArrayObject)

          instrs += ARRAY_SET(genTypeName.anyArray)

          // i += 1
          instrs += LOCAL_GET(iLocal)
          instrs += I32_CONST(1)
          instrs += I32_ADD
          instrs += LOCAL_SET(iLocal)
        }
      }

      // load underlying; struct.new ObjectArray
      instrs += LOCAL_GET(underlyingLocal)
      instrs += STRUCT_NEW(genTypeName.forArrayClass(arrayTypeRef))
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
    import IRTrees.MemberNamespace.Public
    import SpecialNames.hashCodeMethodName
    import genFieldIdx.typeData._

    // A global exclusively used by this function
    ctx.addGlobal(
      WasmGlobal(
        genGlobalName.lastIDHashCode,
        WasmInt32,
        WasmExpr(List(I32_CONST(0))),
        isMutable = true
      )
    )

    val fb = newFunctionBuilder(genFunctionName.identityHashCode)
    val objParam = fb.addParam("obj", WasmRefType.anyref)
    fb.setResultType(WasmInt32)

    val instrs = fb

    val objNonNullLocal = fb.addLocal("objNonNull", WasmRefType.any)
    val resultLocal = fb.addLocal("result", WasmInt32)

    // If `obj` is `null`, return 0 (by spec)
    instrs.block(WasmRefType.any) { nonNullLabel =>
      instrs += LOCAL_GET(objParam)
      instrs += BR_ON_NON_NULL(nonNullLabel)
      instrs += I32_CONST(0)
      instrs += RETURN
    }
    instrs += LOCAL_TEE(objNonNullLocal)

    // If `obj` is one of our objects, skip all the jsValueType tests
    instrs += REF_TEST(WasmRefType(genTypeName.ObjectStruct))
    instrs += I32_EQZ
    instrs.ifThen() {
      instrs.switch() { () =>
        instrs += LOCAL_GET(objNonNullLocal)
        instrs += CALL(genFunctionName.jsValueType)
      }(
        List(JSValueTypeFalse) -> { () =>
          instrs += I32_CONST(1237) // specified by jl.Boolean.hashCode()
          instrs += RETURN
        },
        List(JSValueTypeTrue) -> { () =>
          instrs += I32_CONST(1231) // specified by jl.Boolean.hashCode()
          instrs += RETURN
        },
        List(JSValueTypeString) -> { () =>
          instrs += LOCAL_GET(objNonNullLocal)
          instrs += CALL(
            genFunctionName.forMethod(Public, IRNames.BoxedStringClass, hashCodeMethodName)
          )
          instrs += RETURN
        },
        List(JSValueTypeNumber) -> { () =>
          instrs += LOCAL_GET(objNonNullLocal)
          instrs += CALL(genFunctionName.unbox(IRTypes.DoubleRef))
          instrs += CALL(
            genFunctionName.forMethod(Public, IRNames.BoxedDoubleClass, hashCodeMethodName)
          )
          instrs += RETURN
        },
        List(JSValueTypeUndefined) -> { () =>
          instrs += I32_CONST(0) // specified by jl.Void.hashCode(), Scala.js only
          instrs += RETURN
        },
        List(JSValueTypeBigInt) -> { () =>
          instrs += LOCAL_GET(objNonNullLocal)
          instrs += CALL(genFunctionName.bigintHashCode)
          instrs += RETURN
        },
        List(JSValueTypeSymbol) -> { () =>
          instrs.block() { descriptionIsNullLabel =>
            instrs += LOCAL_GET(objNonNullLocal)
            instrs += CALL(genFunctionName.symbolDescription)
            instrs += BR_ON_NULL(descriptionIsNullLabel)
            instrs += CALL(
              genFunctionName.forMethod(Public, IRNames.BoxedStringClass, hashCodeMethodName)
            )
            instrs += RETURN
          }
          instrs += I32_CONST(0)
          instrs += RETURN
        }
      ) { () =>
        // JSValueTypeOther -- fall through to using idHashCodeMap
        ()
      }
    }

    // If we get here, use the idHashCodeMap

    // Read the existing idHashCode, if one exists
    instrs += GLOBAL_GET(genGlobalName.idHashCodeMap)
    instrs += LOCAL_GET(objNonNullLocal)
    instrs += CALL(genFunctionName.idHashCodeGet)
    instrs += LOCAL_TEE(resultLocal)

    // If it is 0, there was no recorded idHashCode yet; allocate a new one
    instrs += I32_EQZ
    instrs.ifThen() {
      // Allocate a new idHashCode
      instrs += GLOBAL_GET(genGlobalName.lastIDHashCode)
      instrs += I32_CONST(1)
      instrs += I32_ADD
      instrs += LOCAL_TEE(resultLocal)
      instrs += GLOBAL_SET(genGlobalName.lastIDHashCode)

      // Store it for next time
      instrs += GLOBAL_GET(genGlobalName.idHashCodeMap)
      instrs += LOCAL_GET(objNonNullLocal)
      instrs += LOCAL_GET(resultLocal)
      instrs += CALL(genFunctionName.idHashCodeSet)
    }

    instrs += LOCAL_GET(resultLocal)

    fb.buildAndAddToModule()
  }

  /** Search for a reflective proxy function with the given `methodId` in the `reflectiveProxies`
    * field in `typeData` and returns the corresponding function reference.
    *
    * `searchReflectiveProxy`: [typeData, i32] -> [(ref func)]
    */
  private def genSearchReflectiveProxy()(implicit ctx: WasmContext): Unit = {
    import genFieldIdx.typeData._

    val typeDataType = WasmRefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.searchReflectiveProxy)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val methodIdParam = fb.addParam("methodId", WasmInt32)
    fb.setResultType(WasmRefType(WasmHeapType.Func))

    val instrs = fb

    val reflectiveProxies =
      fb.addLocal("reflectiveProxies", Types.WasmRefType(genTypeName.reflectiveProxies))
    val size = fb.addLocal("size", Types.WasmInt32)
    val i = fb.addLocal("i", Types.WasmInt32)

    instrs += LOCAL_GET(typeDataParam)
    instrs += STRUCT_GET(
      genTypeName.typeData,
      genFieldIdx.typeData.reflectiveProxiesIdx
    )
    instrs += LOCAL_TEE(reflectiveProxies)
    instrs += ARRAY_LEN
    instrs += LOCAL_SET(size)

    instrs += I32_CONST(0)
    instrs += LOCAL_SET(i)

    instrs.whileLoop() {
      instrs += LOCAL_GET(i)
      instrs += LOCAL_GET(size)
      instrs += I32_NE
    } {
      instrs += LOCAL_GET(reflectiveProxies)
      instrs += LOCAL_GET(i)
      instrs += ARRAY_GET(genTypeName.reflectiveProxies)

      instrs += STRUCT_GET(
        genTypeName.reflectiveProxy,
        genFieldIdx.reflectiveProxy.nameIdx
      )
      instrs += LOCAL_GET(methodIdParam)
      instrs += I32_EQ

      instrs.ifThen() {
        instrs += LOCAL_GET(reflectiveProxies)
        instrs += LOCAL_GET(i)
        instrs += ARRAY_GET(genTypeName.reflectiveProxies)

        // get function reference
        instrs += STRUCT_GET(
          genTypeName.reflectiveProxy,
          genFieldIdx.reflectiveProxy.funcIdx
        )
        instrs += RETURN
      }

      // i += 1
      instrs += LOCAL_GET(i)
      instrs += I32_CONST(1)
      instrs += I32_ADD
      instrs += LOCAL_SET(i)
    }
    // throw new TypeError("...")
    instrs ++= ctx.getConstantStringInstr("TypeError")
    instrs += CALL(genFunctionName.jsGlobalRefGet)
    instrs += CALL(genFunctionName.jsNewArray)
    // Originally, exception is thrown from JS saying e.g. "obj2.z1__ is not a function"
    instrs ++= ctx.getConstantStringInstr("Method not found")
    instrs += CALL(genFunctionName.jsArrayPush)
    instrs += CALL(genFunctionName.jsNew)
    instrs += EXTERN_CONVERT_ANY
    instrs += THROW(genTagName.exceptionTagName)

    fb.buildAndAddToModule()
  }

  private def genArrayCloneFunctions()(implicit ctx: WasmContext): Unit = {
    val baseRefs = List(
      IRTypes.BooleanRef,
      IRTypes.CharRef,
      IRTypes.ByteRef,
      IRTypes.ShortRef,
      IRTypes.IntRef,
      IRTypes.LongRef,
      IRTypes.FloatRef,
      IRTypes.DoubleRef,
      IRTypes.ClassRef(IRNames.ObjectClass)
    )

    for (baseRef <- baseRefs)
      genArrayCloneFunction(baseRef)
  }

  /** Generates the clone function for the array class with the given base. */
  private def genArrayCloneFunction(
      baseRef: IRTypes.NonArrayTypeRef
  )(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionName.clone(baseRef))
    val fromParam = fb.addParam("from", WasmRefType(genTypeName.ObjectStruct))
    fb.setResultType(WasmRefType(genTypeName.ObjectStruct))
    fb.setFunctionType(genTypeName.cloneFunctionType)

    val instrs = fb

    val arrayTypeRef = IRTypes.ArrayTypeRef(baseRef, 1)

    val arrayStructTypeName = genTypeName.forArrayClass(arrayTypeRef)
    val arrayClassType = WasmRefType(arrayStructTypeName)

    val underlyingArrayTypeName = genTypeName.underlyingOf(arrayTypeRef)
    val underlyingArrayType = WasmRefType(underlyingArrayTypeName)

    val fromLocal = fb.addLocal("fromTyped", arrayClassType)
    val fromUnderlyingLocal = fb.addLocal("fromUnderlying", underlyingArrayType)
    val lengthLocal = fb.addLocal("length", WasmInt32)
    val resultUnderlyingLocal = fb.addLocal("resultUnderlying", underlyingArrayType)

    // Cast down the from argument
    instrs += LOCAL_GET(fromParam)
    instrs += REF_CAST(arrayClassType)
    instrs += LOCAL_TEE(fromLocal)

    // Load the underlying array
    instrs += STRUCT_GET(arrayStructTypeName, genFieldIdx.objStruct.uniqueRegularField)
    instrs += LOCAL_TEE(fromUnderlyingLocal)

    // Make a copy of the underlying array
    instrs += ARRAY_LEN
    instrs += LOCAL_TEE(lengthLocal)
    instrs += ARRAY_NEW_DEFAULT(underlyingArrayTypeName)
    instrs += LOCAL_TEE(resultUnderlyingLocal) // also dest for array.copy
    instrs += I32_CONST(0) // destOffset
    instrs += LOCAL_GET(fromUnderlyingLocal) // src
    instrs += I32_CONST(0) // srcOffset
    instrs += LOCAL_GET(lengthLocal) // length
    instrs += ARRAY_COPY(underlyingArrayTypeName, underlyingArrayTypeName)

    // Build the result arrayStruct
    instrs += LOCAL_GET(fromLocal)
    instrs += STRUCT_GET(arrayStructTypeName, genFieldIdx.objStruct.vtable) // vtable
    instrs += GLOBAL_GET(genGlobalName.arrayClassITable) // itable
    instrs += LOCAL_GET(resultUnderlyingLocal)
    instrs += STRUCT_NEW(arrayStructTypeName)

    fb.buildAndAddToModule()
  }

}
