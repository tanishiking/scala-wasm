package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.{JSUnaryOp, JSBinaryOp, MemberNamespace}
import org.scalajs.ir.Types.{Type => _, ArrayType => _, _}
import org.scalajs.ir.Position

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.Instructions._
import org.scalajs.linker.backend.webassembly.Names._
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
    import genFieldName.typeData._
    import RefType.nullable
    List(
      StructField(nameOffset, Int32, isMutable = false),
      StructField(nameSize, Int32, isMutable = false),
      StructField(nameStringIndex, Int32, isMutable = false),
      StructField(kind, Int32, isMutable = false),
      StructField(specialInstanceTypes, Int32, isMutable = false),
      StructField(strictAncestors, nullable(genTypeName.typeDataArray), isMutable = false),
      StructField(componentType, nullable(genTypeName.typeData), isMutable = false),
      StructField(name, RefType.anyref, isMutable = true),
      StructField(classOfValue, nullable(genTypeName.ClassStruct), isMutable = true),
      StructField(arrayOf, nullable(genTypeName.ObjectVTable), isMutable = true),
      StructField(cloneFunction, nullable(genTypeName.cloneFunctionType), isMutable = false),
      StructField(
        isJSClassInstance,
        nullable(genTypeName.isJSClassInstanceFuncType),
        isMutable = false
      ),
      StructField(
        reflectiveProxies,
        RefType(genTypeName.reflectiveProxies),
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

    b.addRecType(genTypeName.i8Array, ArrayType(FieldType(Int8, true)))
    b.addRecType(genTypeName.i16Array, ArrayType(FieldType(Int16, true)))
    b.addRecType(genTypeName.i32Array, ArrayType(FieldType(Int32, true)))
    b.addRecType(genTypeName.i64Array, ArrayType(FieldType(Int64, true)))
    b.addRecType(genTypeName.f32Array, ArrayType(FieldType(Float32, true)))
    b.addRecType(genTypeName.f64Array, ArrayType(FieldType(Float64, true)))
    b.addRecType(genTypeName.anyArray, ArrayType(FieldType(anyref, true)))
  }

  private def genCoreTypesInRecType()(implicit ctx: WasmContext): Unit = {
    ctx.mainRecType.addSubType(
      genTypeName.cloneFunctionType,
      FunctionType(
        List(RefType(genTypeName.ObjectStruct)),
        List(RefType(genTypeName.ObjectStruct))
      )
    )

    ctx.mainRecType.addSubType(
      genTypeName.isJSClassInstanceFuncType,
      FunctionType(List(RefType.anyref), List(Int32))
    )

    ctx.mainRecType.addSubType(
      genTypeName.typeDataArray,
      ArrayType(FieldType(RefType(genTypeName.typeData), isMutable = false))
    )
    ctx.mainRecType.addSubType(
      genTypeName.itables,
      ArrayType(FieldType(RefType.nullable(HeapType.Struct), isMutable = true))
    )
    ctx.mainRecType.addSubType(
      genTypeName.reflectiveProxies,
      ArrayType(FieldType(RefType(genTypeName.reflectiveProxy), isMutable = false))
    )

    ctx.mainRecType.addSubType(
      SubType(
        genTypeName.typeData,
        isFinal = false,
        None,
        StructType(typeDataStructFields)
      )
    )

    ctx.mainRecType.addSubType(
      genTypeName.reflectiveProxy,
      StructType(
        List(
          StructField(genFieldName.reflectiveProxy.func_name, Int32, isMutable = false),
          StructField(
            genFieldName.reflectiveProxy.func_ref,
            RefType(HeapType.Func),
            isMutable = false
          )
        )
      )
    )
  }

  private def genArrayClassTypes()(implicit ctx: WasmContext): Unit = {
    // The vtable type is always the same as j.l.Object
    val vtableTypeName = genTypeName.ObjectVTable
    val vtableField = StructField(
      genFieldName.objStruct.vtable,
      RefType(vtableTypeName),
      isMutable = false
    )
    val itablesField = StructField(
      genFieldName.objStruct.itables,
      RefType.nullable(genTypeName.itables),
      isMutable = false
    )

    val typeRefsWithArrays: List[(TypeName, TypeName)] =
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
      val underlyingArrayField = StructField(
        genFieldName.objStruct.arrayUnderlying,
        RefType(underlyingArrayTypeName),
        isMutable = false
      )

      val superType = genTypeName.ObjectStruct
      val structType = StructType(
        List(vtableField, itablesField, underlyingArrayField)
      )
      val subType = SubType(structTypeName, isFinal = true, Some(superType), structType)
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
        ImportDesc.Tag(genTagName.exceptionTagName, typeName)
      )
    )
  }

  private def genGlobalImports()(implicit ctx: WasmContext): Unit = {
    def addGlobalHelperImport(name: GlobalName, typ: Type, isMutable: Boolean): Unit = {
      ctx.moduleBuilder.addImport(
        Import(
          "__scalaJSHelpers",
          name.name,
          ImportDesc.Global(name, typ, isMutable)
        )
      )
    }

    addGlobalHelperImport(genGlobalName.undef, RefType.any, isMutable = false)
    addGlobalHelperImport(genGlobalName.bFalse, RefType.any, isMutable = false)
    addGlobalHelperImport(genGlobalName.bZero, RefType.any, isMutable = false)
    addGlobalHelperImport(genGlobalName.emptyString, RefType.any, isMutable = false)
    addGlobalHelperImport(genGlobalName.idHashCodeMap, RefType.extern, isMutable = false)
  }

  private def genPrimitiveTypeDataGlobals()(implicit ctx: WasmContext): Unit = {
    import genFieldName.typeData._

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

    val typeDataTypeName = genTypeName.typeData

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
      ArrayNewFixed(genTypeName.reflectiveProxies, 0)
    )

    for ((primRef, kind) <- primRefsWithTypeData) {
      val nameDataValue: List[Instr] =
        ctx.getConstantStringDataInstr(primRef.displayName)

      val instrs: List[Instr] = {
        nameDataValue ::: I32Const(kind) :: commonFieldValues :::
          StructNew(genTypeName.typeData) :: Nil
      }

      ctx.addGlobal(
        Global(
          genGlobalName.forVTable(primRef),
          RefType(genTypeName.typeData),
          Expr(instrs),
          isMutable = false
        )
      )
    }
  }

  private def genBoxedZeroGlobals()(implicit ctx: WasmContext): Unit = {
    val primTypesWithBoxClasses: List[(GlobalName, ClassName, Instr)] = List(
      (genGlobalName.bZeroChar, SpecialNames.CharBoxClass, I32Const(0)),
      (genGlobalName.bZeroLong, SpecialNames.LongBoxClass, I64Const(0))
    )

    for ((globalName, boxClassName, zeroValueInstr) <- primTypesWithBoxClasses) {
      val boxStruct = genTypeName.forClass(boxClassName)
      val instrs: List[Instr] = List(
        GlobalGet(genGlobalName.forVTable(boxClassName)),
        GlobalGet(genGlobalName.forITable(boxClassName)),
        zeroValueInstr,
        StructNew(boxStruct)
      )

      ctx.addGlobal(
        Global(
          globalName,
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
      ArrayNewDefault(genTypeName.itables)
    )
    ctx.addGlobal(
      Global(
        genGlobalName.arrayClassITable,
        RefType(genTypeName.itables),
        init = Expr(itablesInit),
        isMutable = false
      )
    )
  }

  private def genHelperImports()(implicit ctx: WasmContext): Unit = {
    import RefType.anyref

    def addHelperImport(
        name: FunctionName,
        params: List[Type],
        results: List[Type]
    ): Unit = {
      val sig = FunctionType(params, results)
      val typeName = ctx.moduleBuilder.functionTypeToTypeName(sig)
      ctx.moduleBuilder.addImport(
        Import("__scalaJSHelpers", name.name, ImportDesc.Func(name, typeName))
      )
    }

    addHelperImport(genFunctionName.is, List(anyref, anyref), List(Int32))

    addHelperImport(genFunctionName.isUndef, List(anyref), List(Int32))

    for (primRef <- List(BooleanRef, ByteRef, ShortRef, IntRef, FloatRef, DoubleRef)) {
      val wasmType = primRef match {
        case FloatRef  => Float32
        case DoubleRef => Float64
        case _         => Int32
      }
      addHelperImport(genFunctionName.box(primRef), List(wasmType), List(anyref))
      addHelperImport(genFunctionName.unbox(primRef), List(anyref), List(wasmType))
      addHelperImport(genFunctionName.unboxOrNull(primRef), List(anyref), List(anyref))
      addHelperImport(genFunctionName.typeTest(primRef), List(anyref), List(Int32))
    }

    addHelperImport(genFunctionName.fmod, List(Float64, Float64), List(Float64))

    addHelperImport(
      genFunctionName.closure,
      List(RefType.func, anyref),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionName.closureThis,
      List(RefType.func, anyref),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionName.closureRest,
      List(RefType.func, anyref, Int32),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionName.closureThisRest,
      List(RefType.func, anyref, Int32),
      List(RefType.any)
    )

    addHelperImport(genFunctionName.makeExportedDef, List(RefType.func), List(RefType.any))
    addHelperImport(
      genFunctionName.makeExportedDefRest,
      List(RefType.func, Int32),
      List(RefType.any)
    )

    addHelperImport(genFunctionName.stringLength, List(RefType.any), List(Int32))
    addHelperImport(genFunctionName.stringCharAt, List(RefType.any, Int32), List(Int32))
    addHelperImport(genFunctionName.jsValueToString, List(RefType.any), List(RefType.any))
    addHelperImport(genFunctionName.jsValueToStringForConcat, List(anyref), List(RefType.any))
    addHelperImport(genFunctionName.booleanToString, List(Int32), List(RefType.any))
    addHelperImport(genFunctionName.charToString, List(Int32), List(RefType.any))
    addHelperImport(genFunctionName.intToString, List(Int32), List(RefType.any))
    addHelperImport(genFunctionName.longToString, List(Int64), List(RefType.any))
    addHelperImport(genFunctionName.doubleToString, List(Float64), List(RefType.any))
    addHelperImport(
      genFunctionName.stringConcat,
      List(RefType.any, RefType.any),
      List(RefType.any)
    )
    addHelperImport(genFunctionName.isString, List(anyref), List(Int32))

    addHelperImport(genFunctionName.jsValueType, List(RefType.any), List(Int32))
    addHelperImport(genFunctionName.bigintHashCode, List(RefType.any), List(Int32))
    addHelperImport(
      genFunctionName.symbolDescription,
      List(RefType.any),
      List(RefType.anyref)
    )
    addHelperImport(
      genFunctionName.idHashCodeGet,
      List(RefType.extern, RefType.any),
      List(Int32)
    )
    addHelperImport(
      genFunctionName.idHashCodeSet,
      List(RefType.extern, RefType.any, Int32),
      Nil
    )

    addHelperImport(genFunctionName.jsGlobalRefGet, List(RefType.any), List(anyref))
    addHelperImport(genFunctionName.jsGlobalRefSet, List(RefType.any, anyref), Nil)
    addHelperImport(genFunctionName.jsGlobalRefTypeof, List(RefType.any), List(RefType.any))
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
    addHelperImport(genFunctionName.jsIsTruthy, List(anyref), List(Int32))
    addHelperImport(genFunctionName.jsLinkingInfo, Nil, List(anyref))

    for ((op, name) <- genFunctionName.jsUnaryOps)
      addHelperImport(name, List(anyref), List(anyref))

    for ((op, name) <- genFunctionName.jsBinaryOps) {
      val resultType =
        if (op == JSBinaryOp.=== || op == JSBinaryOp.!==) Int32
        else anyref
      addHelperImport(name, List(anyref, anyref), List(resultType))
    }

    addHelperImport(genFunctionName.newSymbol, Nil, List(anyref))
    addHelperImport(
      genFunctionName.createJSClass,
      List(anyref, anyref, RefType.func, RefType.func, RefType.func, anyref),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionName.createJSClassRest,
      List(anyref, anyref, RefType.func, RefType.func, RefType.func, anyref, Int32),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionName.installJSField,
      List(anyref, anyref, anyref),
      Nil
    )
    addHelperImport(
      genFunctionName.installJSMethod,
      List(anyref, anyref, anyref, RefType.func, Int32),
      Nil
    )
    addHelperImport(
      genFunctionName.installJSStaticMethod,
      List(anyref, anyref, anyref, RefType.func, Int32),
      Nil
    )
    addHelperImport(
      genFunctionName.installJSProperty,
      List(anyref, anyref, anyref, RefType.funcref, RefType.funcref),
      Nil
    )
    addHelperImport(
      genFunctionName.installJSStaticProperty,
      List(anyref, anyref, anyref, RefType.funcref, RefType.funcref),
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

  private def newFunctionBuilder(functionName: FunctionName)(implicit
      ctx: WasmContext
  ): FunctionBuilder = {
    new FunctionBuilder(ctx.moduleBuilder, functionName, noPos)
  }

  private def genStringLiteral()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionName.stringLiteral)
    val offsetParam = fb.addParam("offset", Int32)
    val sizeParam = fb.addParam("size", Int32)
    val stringIndexParam = fb.addParam("stringIndex", Int32)
    fb.setResultType(RefType.any)

    val str = fb.addLocal("str", RefType.any)

    val instrs = fb

    instrs.block(RefType.any) { cacheHit =>
      instrs += GlobalGet(genGlobalName.stringLiteralCache)
      instrs += LocalGet(stringIndexParam)
      instrs += ArrayGet(genTypeName.anyArray)

      instrs += BrOnNonNull(cacheHit)

      // cache miss, create a new string and cache it
      instrs += GlobalGet(genGlobalName.stringLiteralCache)
      instrs += LocalGet(stringIndexParam)

      instrs += LocalGet(offsetParam)
      instrs += LocalGet(sizeParam)
      instrs += ArrayNewData(genTypeName.i16Array, genDataName.string)
      instrs += Call(genFunctionName.createStringFromData)
      instrs += LocalTee(str)
      instrs += ArraySet(genTypeName.anyArray)

      instrs += LocalGet(str)
    }

    fb.buildAndAddToModule()
  }

  /** `createStringFromData: (ref array u16) -> (ref any)` (representing a `string`). */
  private def genCreateStringFromData()(implicit ctx: WasmContext): Unit = {
    val dataType = RefType(genTypeName.i16Array)

    val fb = newFunctionBuilder(genFunctionName.createStringFromData)
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
    instrs += GlobalGet(genGlobalName.emptyString)
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
      instrs += ArrayGetU(genTypeName.i16Array)
      instrs += Call(genFunctionName.charToString)
      instrs += Call(genFunctionName.stringConcat)
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
    val typeDataType = RefType(genTypeName.typeData)
    val nameDataType = RefType(genTypeName.i16Array)

    val fb = newFunctionBuilder(genFunctionName.typeDataName)
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
      instrs += StructGet(genTypeName.typeData, genFieldIdx.typeData.nameIdx)
      instrs += BrOnNonNull(alreadyInitializedLabel)

      // for the STRUCT_SET typeData.name near the end
      instrs += LocalGet(typeDataParam)

      // if typeData.kind == KindArray
      instrs += LocalGet(typeDataParam)
      instrs += StructGet(genTypeName.typeData, genFieldIdx.typeData.kindIdx)
      instrs += I32Const(KindArray)
      instrs += I32Eq
      instrs.ifThenElse(RefType.any) {
        // it is an array; compute its name from the component type name

        // <top of stack> := "[", for the CALL to stringConcat near the end
        instrs += I32Const('['.toInt)
        instrs += Call(genFunctionName.charToString)

        // componentTypeData := ref_as_non_null(typeData.componentType)
        instrs += LocalGet(typeDataParam)
        instrs += StructGet(
          genTypeName.typeData,
          genFieldIdx.typeData.componentTypeIdx
        )
        instrs += RefAsNotNull
        instrs += LocalSet(componentTypeDataLocal)

        // switch (componentTypeData.kind)
        // the result of this switch is the string that must come after "["
        instrs.switch(RefType.any) { () =>
          // scrutinee
          instrs += LocalGet(componentTypeDataLocal)
          instrs += StructGet(genTypeName.typeData, genFieldIdx.typeData.kindIdx)
        }(
          List(KindBoolean) -> { () =>
            instrs += I32Const('Z'.toInt)
            instrs += Call(genFunctionName.charToString)
          },
          List(KindChar) -> { () =>
            instrs += I32Const('C'.toInt)
            instrs += Call(genFunctionName.charToString)
          },
          List(KindByte) -> { () =>
            instrs += I32Const('B'.toInt)
            instrs += Call(genFunctionName.charToString)
          },
          List(KindShort) -> { () =>
            instrs += I32Const('S'.toInt)
            instrs += Call(genFunctionName.charToString)
          },
          List(KindInt) -> { () =>
            instrs += I32Const('I'.toInt)
            instrs += Call(genFunctionName.charToString)
          },
          List(KindLong) -> { () =>
            instrs += I32Const('J'.toInt)
            instrs += Call(genFunctionName.charToString)
          },
          List(KindFloat) -> { () =>
            instrs += I32Const('F'.toInt)
            instrs += Call(genFunctionName.charToString)
          },
          List(KindDouble) -> { () =>
            instrs += I32Const('D'.toInt)
            instrs += Call(genFunctionName.charToString)
          },
          List(KindArray) -> { () =>
            // the component type is an array; get its own name
            instrs += LocalGet(componentTypeDataLocal)
            instrs += Call(genFunctionName.typeDataName)
          }
        ) { () =>
          // default: the component type is neither a primitive nor an array;
          // concatenate "L" + <its own name> + ";"
          instrs += I32Const('L'.toInt)
          instrs += Call(genFunctionName.charToString)
          instrs += LocalGet(componentTypeDataLocal)
          instrs += Call(genFunctionName.typeDataName)
          instrs += Call(genFunctionName.stringConcat)
          instrs += I32Const(';'.toInt)
          instrs += Call(genFunctionName.charToString)
          instrs += Call(genFunctionName.stringConcat)
        }

        // At this point, the stack contains "[" and the string that must be concatenated with it
        instrs += Call(genFunctionName.stringConcat)
      } {
        // it is not an array; its name is stored in nameData
        for (
          idx <- List(
            genFieldIdx.typeData.nameOffsetIdx,
            genFieldIdx.typeData.nameSizeIdx,
            genFieldIdx.typeData.nameStringIndexIdx
          )
        ) {
          instrs += LocalGet(typeDataParam)
          instrs += StructGet(genTypeName.typeData, idx)
        }
        instrs += Call(genFunctionName.stringLiteral)
      }

      // typeData.name := <top of stack> ; leave it on the stack
      instrs += LocalTee(nameLocal)
      instrs += StructSet(genTypeName.typeData, genFieldIdx.typeData.nameIdx)
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
    val typeDataType = RefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.createClassOf)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(RefType(genTypeName.ClassStruct))

    val instrs = fb

    val classInstanceLocal = fb.addLocal("classInstance", RefType(genTypeName.ClassStruct))

    // classInstance := newDefault$java.lang.Class()
    // leave it on the stack for the constructor call
    instrs += Call(genFunctionName.newDefault(ClassClass))
    instrs += LocalTee(classInstanceLocal)

    /* The JS object containing metadata to pass as argument to the `jl.Class` constructor.
     * Specified by https://lampwww.epfl.ch/~doeraene/sjsir-semantics/#sec-sjsir-createclassdataof
     * Leave it on the stack.
     */
    instrs += Call(genFunctionName.jsNewObject)
    // "__typeData": typeData (TODO hide this better? although nobody will notice anyway)
    instrs ++= ctx.getConstantStringInstr("__typeData")
    instrs += LocalGet(typeDataParam)
    instrs += Call(genFunctionName.jsObjectPush)
    // "name": typeDataName(typeData)
    instrs ++= ctx.getConstantStringInstr("name")
    instrs += LocalGet(typeDataParam)
    instrs += Call(genFunctionName.typeDataName)
    instrs += Call(genFunctionName.jsObjectPush)
    // "isPrimitive": (typeData.kind <= KindLastPrimitive)
    instrs ++= ctx.getConstantStringInstr("isPrimitive")
    instrs += LocalGet(typeDataParam)
    instrs += StructGet(genTypeName.typeData, genFieldIdx.typeData.kindIdx)
    instrs += I32Const(KindLastPrimitive)
    instrs += I32LeU
    instrs += Call(genFunctionName.box(BooleanRef))
    instrs += Call(genFunctionName.jsObjectPush)
    // "isArrayClass": (typeData.kind == KindArray)
    instrs ++= ctx.getConstantStringInstr("isArrayClass")
    instrs += LocalGet(typeDataParam)
    instrs += StructGet(genTypeName.typeData, genFieldIdx.typeData.kindIdx)
    instrs += I32Const(KindArray)
    instrs += I32Eq
    instrs += Call(genFunctionName.box(BooleanRef))
    instrs += Call(genFunctionName.jsObjectPush)
    // "isInterface": (typeData.kind == KindInterface)
    instrs ++= ctx.getConstantStringInstr("isInterface")
    instrs += LocalGet(typeDataParam)
    instrs += StructGet(genTypeName.typeData, genFieldIdx.typeData.kindIdx)
    instrs += I32Const(KindInterface)
    instrs += I32Eq
    instrs += Call(genFunctionName.box(BooleanRef))
    instrs += Call(genFunctionName.jsObjectPush)
    // "isInstance": closure(isInstance, typeData)
    instrs ++= ctx.getConstantStringInstr("isInstance")
    instrs += ctx.refFuncWithDeclaration(genFunctionName.isInstance)
    instrs += LocalGet(typeDataParam)
    instrs += Call(genFunctionName.closure)
    instrs += Call(genFunctionName.jsObjectPush)
    // "isAssignableFrom": closure(isAssignableFrom, typeData)
    instrs ++= ctx.getConstantStringInstr("isAssignableFrom")
    instrs += ctx.refFuncWithDeclaration(genFunctionName.isAssignableFromExternal)
    instrs += LocalGet(typeDataParam)
    instrs += Call(genFunctionName.closure)
    instrs += Call(genFunctionName.jsObjectPush)
    // "checkCast": closure(checkCast, typeData)
    instrs ++= ctx.getConstantStringInstr("checkCast")
    instrs += ctx.refFuncWithDeclaration(genFunctionName.checkCast)
    instrs += LocalGet(typeDataParam)
    instrs += Call(genFunctionName.closure)
    instrs += Call(genFunctionName.jsObjectPush)
    // "isAssignableFrom": closure(isAssignableFrom, typeData)
    // "getComponentType": closure(getComponentType, typeData)
    instrs ++= ctx.getConstantStringInstr("getComponentType")
    instrs += ctx.refFuncWithDeclaration(genFunctionName.getComponentType)
    instrs += LocalGet(typeDataParam)
    instrs += Call(genFunctionName.closure)
    instrs += Call(genFunctionName.jsObjectPush)
    // "newArrayOfThisClass": closure(newArrayOfThisClass, typeData)
    instrs ++= ctx.getConstantStringInstr("newArrayOfThisClass")
    instrs += ctx.refFuncWithDeclaration(genFunctionName.newArrayOfThisClass)
    instrs += LocalGet(typeDataParam)
    instrs += Call(genFunctionName.closure)
    instrs += Call(genFunctionName.jsObjectPush)

    // Call java.lang.Class::<init>(dataObject)
    instrs += Call(
      genFunctionName.forMethod(
        MemberNamespace.Constructor,
        ClassClass,
        SpecialNames.ClassCtor
      )
    )

    // typeData.classOf := classInstance
    instrs += LocalGet(typeDataParam)
    instrs += LocalGet(classInstanceLocal)
    instrs += StructSet(genTypeName.typeData, genFieldIdx.typeData.classOfIdx)

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
    val typeDataType = RefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.getClassOf)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(RefType(genTypeName.ClassStruct))

    val instrs = fb

    instrs.block(RefType(genTypeName.ClassStruct)) { alreadyInitializedLabel =>
      // fast path
      instrs += LocalGet(typeDataParam)
      instrs += StructGet(genTypeName.typeData, genFieldIdx.typeData.classOfIdx)
      instrs += BrOnNonNull(alreadyInitializedLabel)
      // slow path
      instrs += LocalGet(typeDataParam)
      instrs += Call(genFunctionName.createClassOf)
    } // end bock alreadyInitializedLabel

    fb.buildAndAddToModule()
  }

  /** `arrayTypeData: (ref typeData), i32 -> (ref $java.lang.Object___vtable)`.
    *
    * Returns the typeData/vtable of an array with `dims` dimensions over the given typeData. `dims`
    * must be be strictly positive.
    */
  private def genArrayTypeData()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeName.typeData)
    val objectVTableType = RefType(genTypeName.ObjectVTable)

    /* Array classes extend Cloneable, Serializable and Object.
     * Filter out the ones that do not have run-time type info at all, as
     * we do for other classes.
     */
    val strictAncestors =
      List(CloneableClass, SerializableClass, ObjectClass)
        .filter(name => ctx.getClassInfoOption(name).exists(_.hasRuntimeTypeInfo))

    val fb = newFunctionBuilder(genFunctionName.arrayTypeData)
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
          genTypeName.typeData,
          genFieldIdx.typeData.arrayOfIdx
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
          instrs += GlobalGet(genGlobalName.forVTable(strictAncestor))
        instrs += ArrayNewFixed(
          genTypeName.typeDataArray,
          strictAncestors.size
        )

        instrs += LocalGet(typeDataParam) // componentType
        instrs += RefNull(HeapType.None) // name
        instrs += RefNull(HeapType.None) // classOf
        instrs += RefNull(HeapType.None) // arrayOf

        // clone
        instrs.switch(RefType(genTypeName.cloneFunctionType)) { () =>
          instrs += LocalGet(typeDataParam)
          instrs += StructGet(genTypeName.typeData, genFieldIdx.typeData.kindIdx)
        }(
          List(KindBoolean) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(BooleanRef))
          },
          List(KindChar) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(CharRef))
          },
          List(KindByte) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(ByteRef))
          },
          List(KindShort) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(ShortRef))
          },
          List(KindInt) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(IntRef))
          },
          List(KindLong) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(LongRef))
          },
          List(KindFloat) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(FloatRef))
          },
          List(KindDouble) -> { () =>
            instrs += ctx.refFuncWithDeclaration(genFunctionName.clone(DoubleRef))
          }
        ) { () =>
          instrs += ctx.refFuncWithDeclaration(
            genFunctionName.clone(ClassRef(ObjectClass))
          )
        }

        // isJSClassInstance
        instrs += RefNull(HeapType.NoFunc)

        // reflectiveProxies
        instrs += ArrayNewFixed(genTypeName.reflectiveProxies, 0) // TODO

        val objectClassInfo = ctx.getClassInfo(ObjectClass)
        instrs ++= objectClassInfo.tableEntries.map { methodName =>
          ctx.refFuncWithDeclaration(objectClassInfo.resolvedMethodInfos(methodName).tableEntryName)
        }
        instrs += StructNew(genTypeName.ObjectVTable)
        instrs += LocalTee(arrayTypeDataLocal)

        // <old typeData>.arrayOf := typeData
        instrs += StructSet(
          genTypeName.typeData,
          genFieldIdx.typeData.arrayOfIdx
        )

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
    import genFieldIdx.typeData._

    val typeDataType = RefType(genTypeName.typeData)
    val objectRefType = RefType(genTypeName.forClass(ObjectClass))

    val fb = newFunctionBuilder(genFunctionName.isInstance)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val valueParam = fb.addParam("value", RefType.anyref)
    fb.setResultType(Int32)

    val instrs = fb

    val valueNonNullLocal = fb.addLocal("valueNonNull", RefType.any)
    val specialInstanceTypesLocal = fb.addLocal("specialInstanceTypes", Int32)

    // switch (typeData.kind)
    instrs.switch(Int32) { () =>
      instrs += LocalGet(typeDataParam)
      instrs += StructGet(genTypeName.typeData, kindIdx)
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
        instrs += Call(genFunctionName.isUndef)
      },
      List(KindBoxedBoolean) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionName.typeTest(BooleanRef))
      },
      List(KindBoxedCharacter) -> { () =>
        instrs += LocalGet(valueParam)
        val structTypeName = genTypeName.forClass(SpecialNames.CharBoxClass)
        instrs += RefTest(RefType(structTypeName))
      },
      List(KindBoxedByte) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionName.typeTest(ByteRef))
      },
      List(KindBoxedShort) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionName.typeTest(ShortRef))
      },
      List(KindBoxedInteger) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionName.typeTest(IntRef))
      },
      List(KindBoxedLong) -> { () =>
        instrs += LocalGet(valueParam)
        val structTypeName = genTypeName.forClass(SpecialNames.LongBoxClass)
        instrs += RefTest(RefType(structTypeName))
      },
      List(KindBoxedFloat) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionName.typeTest(FloatRef))
      },
      List(KindBoxedDouble) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionName.typeTest(DoubleRef))
      },
      List(KindBoxedString) -> { () =>
        instrs += LocalGet(valueParam)
        instrs += Call(genFunctionName.isString)
      },
      // case KindJSType => call typeData.isJSClassInstance(value) or throw if it is null
      List(KindJSType) -> { () =>
        instrs.block(RefType.anyref) { isJSClassInstanceIsNull =>
          // Load value as the argument to the function
          instrs += LocalGet(valueParam)

          // Load the function reference; break if null
          instrs += LocalGet(typeDataParam)
          instrs += StructGet(genTypeName.typeData, isJSClassInstanceIdx)
          instrs += BrOnNull(isJSClassInstanceIsNull)

          // Call the function
          instrs += CallRef(genTypeName.isJSClassInstanceFuncType)
          instrs += Return
        }
        instrs += Drop // drop `value` which was left on the stack

        // throw new TypeError("...")
        instrs ++= ctx.getConstantStringInstr("TypeError")
        instrs += Call(genFunctionName.jsGlobalRefGet)
        instrs += Call(genFunctionName.jsNewArray)
        instrs ++= ctx.getConstantStringInstr(
          "Cannot call isInstance() on a Class representing a JS trait/object"
        )
        instrs += Call(genFunctionName.jsArrayPush)
        instrs += Call(genFunctionName.jsNew)
        instrs += ExternConvertAny
        instrs += Throw(genTagName.exceptionTagName)
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
      instrs += StructGet(genTypeName.typeData, specialInstanceTypesIdx)
      instrs += LocalTee(specialInstanceTypesLocal)
      instrs += I32Const(0)
      instrs += I32Ne
      instrs.ifThen() {
        // Load (1 << jsValueType(valueNonNull))
        instrs += I32Const(1)
        instrs += LocalGet(valueNonNullLocal)
        instrs += Call(genFunctionName.jsValueType)
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
        genTypeName.forClass(ObjectClass),
        genFieldIdx.objStruct.vtable
      )

      // Call isAssignableFrom
      instrs += Call(genFunctionName.isAssignableFrom)
    }

    fb.buildAndAddToModule()
  }

  /** `isAssignableFromExternal: (ref typeData), anyref -> i32` (a boolean).
    *
    * This is the underlying func for the `isAssignableFrom()` closure inside class data objects.
    */
  private def genIsAssignableFromExternal()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.isAssignableFromExternal)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val fromParam = fb.addParam("from", RefType.anyref)
    fb.setResultType(Int32)

    val instrs = fb

    // load typeData
    instrs += LocalGet(typeDataParam)

    // load ref.cast<typeData> from["__typeData"] (as a JS selection)
    instrs += LocalGet(fromParam)
    instrs ++= ctx.getConstantStringInstr("__typeData")
    instrs += Call(genFunctionName.jsSelect)
    instrs += RefCast(RefType(typeDataType.heapType))

    // delegate to isAssignableFrom
    instrs += Call(genFunctionName.isAssignableFrom)

    fb.buildAndAddToModule()
  }

  /** `isAssignableFrom: (ref typeData), (ref typeData) -> i32` (a boolean).
    *
    * Specified by `java.lang.Class.isAssignableFrom(Class)`.
    */
  private def genIsAssignableFrom()(implicit ctx: WasmContext): Unit = {
    import genFieldIdx.typeData._

    val typeDataType = RefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.isAssignableFrom)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val fromTypeDataParam = fb.addParam("fromTypeData", typeDataType)
    fb.setResultType(Int32)

    val instrs = fb

    val fromAncestorsLocal = fb.addLocal("fromAncestors", RefType(genTypeName.typeDataArray))
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
        instrs += StructGet(genTypeName.typeData, kindIdx)
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
            instrs += StructGet(genTypeName.typeData, componentTypeIdx)
            instrs += BrOnNull(fromComponentTypeIsNullLabel)
            instrs += LocalSet(fromTypeDataParam)

            // typeData := ref.as_non_null typeData.componentType (OK because KindArray)
            instrs += LocalGet(typeDataParam)
            instrs += StructGet(genTypeName.typeData, componentTypeIdx)
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
          instrs += StructGet(genTypeName.typeData, kindIdx)
          instrs += I32Const(KindLastPrimitive)
          instrs += I32GtU
        }
      ) { () =>
        // All other cases: test whether `fromTypeData.strictAncestors` contains `typeData`

        instrs.block() { fromAncestorsIsNullLabel =>
          // fromAncestors := fromTypeData.strictAncestors; go to fromAncestorsIsNull if null
          instrs += LocalGet(fromTypeDataParam)
          instrs += StructGet(genTypeName.typeData, strictAncestorsIdx)
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
            instrs += ArrayGet(genTypeName.typeDataArray)
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
    val typeDataType = RefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.checkCast)
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
    val typeDataType = RefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.getComponentType)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(RefType.nullable(genTypeName.ClassStruct))

    val instrs = fb

    val componentTypeDataLocal = fb.addLocal("componentTypeData", typeDataType)

    instrs.block() { nullResultLabel =>
      // Try and extract non-null component type data
      instrs += LocalGet(typeDataParam)
      instrs += StructGet(
        genTypeName.typeData,
        genFieldIdx.typeData.componentTypeIdx
      )
      instrs += BrOnNull(nullResultLabel)
      // Get the corresponding classOf
      instrs += Call(genFunctionName.getClassOf)
      instrs += Return
    } // end block nullResultLabel
    instrs += RefNull(HeapType(genTypeName.ClassStruct))

    fb.buildAndAddToModule()
  }

  /** `newArrayOfThisClass: (ref typeData), anyref -> (ref jlObject)`.
    *
    * This is the underlying func for the `newArrayOfThisClass()` closure inside class data objects.
    */
  private def genNewArrayOfThisClass()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeName.typeData)
    val i32ArrayType = RefType(genTypeName.i32Array)

    val fb = newFunctionBuilder(genFunctionName.newArrayOfThisClass)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val lengthsParam = fb.addParam("lengths", RefType.anyref)
    fb.setResultType(RefType(genTypeName.ObjectStruct))

    val instrs = fb

    val lengthsLenLocal = fb.addLocal("lengthsLenLocal", Int32)
    val lengthsValuesLocal = fb.addLocal("lengthsValues", i32ArrayType)
    val iLocal = fb.addLocal("i", Int32)

    // lengthsLen := lengths.length // as a JS field access
    instrs += LocalGet(lengthsParam)
    instrs ++= ctx.getConstantStringInstr("length")
    instrs += Call(genFunctionName.jsSelect)
    instrs += Call(genFunctionName.unbox(IntRef))
    instrs += LocalTee(lengthsLenLocal)

    // lengthsValues := array.new<i32Array> lengthsLen
    instrs += ArrayNewDefault(genTypeName.i32Array)
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
      instrs += Call(genFunctionName.jsSelect)
      instrs += Call(genFunctionName.unbox(IntRef))

      instrs += ArraySet(genTypeName.i32Array)

      // i += 1
      instrs += LocalGet(iLocal)
      instrs += I32Const(1)
      instrs += I32Add
      instrs += LocalSet(iLocal)
    }

    // return newArrayObject(arrayTypeData(typeData, lengthsLen), lengthsValues, 0)
    instrs += LocalGet(typeDataParam)
    instrs += LocalGet(lengthsLenLocal)
    instrs += Call(genFunctionName.arrayTypeData)
    instrs += LocalGet(lengthsValuesLocal)
    instrs += I32Const(0)
    instrs += Call(genFunctionName.newArrayObject)

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
    val typeDataType = RefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.anyGetClass)
    val valueParam = fb.addParam("value", RefType.any)
    fb.setResultType(RefType.nullable(genTypeName.ClassStruct))

    val instrs = fb

    val typeDataLocal = fb.addLocal("typeData", typeDataType)
    val doubleValueLocal = fb.addLocal("doubleValue", Float64)
    val intValueLocal = fb.addLocal("intValue", Int32)
    val ourObjectLocal = fb.addLocal("ourObject", RefType(genTypeName.ObjectStruct))

    def getHijackedClassTypeDataInstr(className: ClassName): Instr =
      GlobalGet(genGlobalName.forVTable(className))

    instrs.block(RefType.nullable(genTypeName.ClassStruct)) { nonNullClassOfLabel =>
      instrs.block(typeDataType) { gotTypeDataLabel =>
        instrs.block(RefType(genTypeName.ObjectStruct)) { ourObjectLabel =>
          // if value is our object, jump to $ourObject
          instrs += LocalGet(valueParam)
          instrs += BrOnCast(
            ourObjectLabel,
            RefType.any,
            RefType(genTypeName.ObjectStruct)
          )

          // switch(jsValueType(value)) { ... }
          instrs.switch(typeDataType) { () =>
            // scrutinee
            instrs += LocalGet(valueParam)
            instrs += Call(genFunctionName.jsValueType)
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
              instrs += Call(genFunctionName.unbox(DoubleRef))
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
            instrs += RefNull(HeapType(genTypeName.ClassStruct))
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
        instrs += RefTest(RefType(genTypeName.forClass(SpecialNames.CharBoxClass)))
        instrs.ifThenElse(typeDataType) {
          instrs += getHijackedClassTypeDataInstr(BoxedCharacterClass)
        } {
          instrs += LocalGet(ourObjectLocal)
          instrs += RefTest(RefType(genTypeName.forClass(SpecialNames.LongBoxClass)))
          instrs.ifThenElse(typeDataType) {
            instrs += getHijackedClassTypeDataInstr(BoxedLongClass)
          } {
            instrs += LocalGet(ourObjectLocal)
            instrs += StructGet(
              genTypeName.forClass(ObjectClass),
              genFieldIdx.objStruct.vtable
            )
          }
        }
      }

      instrs += Call(genFunctionName.getClassOf)
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

    val typeDataType = RefType(genTypeName.typeData)
    val i32ArrayType = RefType(genTypeName.i32Array)
    val objectVTableType = RefType(genTypeName.ObjectVTable)
    val arrayTypeDataType = objectVTableType
    val itablesType = RefType.nullable(genTypeName.itables)
    val nonNullObjectType = RefType(genTypeName.ObjectStruct)
    val anyArrayType = RefType(genTypeName.anyArray)

    val fb = newFunctionBuilder(genFunctionName.newArrayObject)
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
    instrs += GlobalGet(genGlobalName.arrayClassITable) // itable

    // Load the first length
    instrs += LocalGet(lengthsParam)
    instrs += LocalGet(lengthIndexParam)
    instrs += ArrayGet(genTypeName.i32Array)

    // componentTypeData := ref_as_non_null(arrayTypeData.componentType)
    // switch (componentTypeData.kind)
    val switchClauseSig = FunctionType(
      List(arrayTypeDataType, itablesType, Int32),
      List(nonNullObjectType)
    )
    instrs.switch(switchClauseSig) { () =>
      // scrutinee
      instrs += LocalGet(arrayTypeDataParam)
      instrs += StructGet(
        genTypeName.typeData,
        genFieldIdx.typeData.componentTypeIdx
      )
      instrs += StructGet(
        genTypeName.typeData,
        genFieldIdx.typeData.kindIdx
      )
    }(
      // For all the primitive types, by construction, this is the bottom dimension
      // case KindPrim => array.new_default underlyingPrimArray; struct.new PrimArray
      primRefsWithArrayTypes.map { case (primRef, kind) =>
        List(kind) -> { () =>
          val arrayTypeRef = ArrayTypeRef(primRef, 1)
          instrs += ArrayNewDefault(genTypeName.underlyingOf(arrayTypeRef))
          instrs += StructNew(genTypeName.forArrayClass(arrayTypeRef))
          () // required for correct type inference
        }
      }: _*
    ) { () =>
      // default -- all non-primitive array types

      // len := <top-of-stack> (which is the first length)
      instrs += LocalTee(lenLocal)

      // underlying := array.new_default anyArray
      val arrayTypeRef = ArrayTypeRef(ClassRef(ObjectClass), 1)
      instrs += ArrayNewDefault(genTypeName.underlyingOf(arrayTypeRef))
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
        instrs += StructGet(
          genTypeName.typeData,
          genFieldIdx.typeData.componentTypeIdx
        )
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
          instrs += Call(genFunctionName.newArrayObject)

          instrs += ArraySet(genTypeName.anyArray)

          // i += 1
          instrs += LocalGet(iLocal)
          instrs += I32Const(1)
          instrs += I32Add
          instrs += LocalSet(iLocal)
        }
      }

      // load underlying; struct.new ObjectArray
      instrs += LocalGet(underlyingLocal)
      instrs += StructNew(genTypeName.forArrayClass(arrayTypeRef))
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
    import genFieldIdx.typeData._

    // A global exclusively used by this function
    ctx.addGlobal(
      Global(
        genGlobalName.lastIDHashCode,
        Int32,
        Expr(List(I32Const(0))),
        isMutable = true
      )
    )

    val fb = newFunctionBuilder(genFunctionName.identityHashCode)
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
    instrs += RefTest(RefType(genTypeName.ObjectStruct))
    instrs += I32Eqz
    instrs.ifThen() {
      instrs.switch() { () =>
        instrs += LocalGet(objNonNullLocal)
        instrs += Call(genFunctionName.jsValueType)
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
            genFunctionName.forMethod(Public, BoxedStringClass, hashCodeMethodName)
          )
          instrs += Return
        },
        List(JSValueTypeNumber) -> { () =>
          instrs += LocalGet(objNonNullLocal)
          instrs += Call(genFunctionName.unbox(DoubleRef))
          instrs += Call(
            genFunctionName.forMethod(Public, BoxedDoubleClass, hashCodeMethodName)
          )
          instrs += Return
        },
        List(JSValueTypeUndefined) -> { () =>
          instrs += I32Const(0) // specified by jl.Void.hashCode(), Scala.js only
          instrs += Return
        },
        List(JSValueTypeBigInt) -> { () =>
          instrs += LocalGet(objNonNullLocal)
          instrs += Call(genFunctionName.bigintHashCode)
          instrs += Return
        },
        List(JSValueTypeSymbol) -> { () =>
          instrs.block() { descriptionIsNullLabel =>
            instrs += LocalGet(objNonNullLocal)
            instrs += Call(genFunctionName.symbolDescription)
            instrs += BrOnNull(descriptionIsNullLabel)
            instrs += Call(
              genFunctionName.forMethod(Public, BoxedStringClass, hashCodeMethodName)
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
    instrs += GlobalGet(genGlobalName.idHashCodeMap)
    instrs += LocalGet(objNonNullLocal)
    instrs += Call(genFunctionName.idHashCodeGet)
    instrs += LocalTee(resultLocal)

    // If it is 0, there was no recorded idHashCode yet; allocate a new one
    instrs += I32Eqz
    instrs.ifThen() {
      // Allocate a new idHashCode
      instrs += GlobalGet(genGlobalName.lastIDHashCode)
      instrs += I32Const(1)
      instrs += I32Add
      instrs += LocalTee(resultLocal)
      instrs += GlobalSet(genGlobalName.lastIDHashCode)

      // Store it for next time
      instrs += GlobalGet(genGlobalName.idHashCodeMap)
      instrs += LocalGet(objNonNullLocal)
      instrs += LocalGet(resultLocal)
      instrs += Call(genFunctionName.idHashCodeSet)
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
    import genFieldIdx.typeData._

    val typeDataType = RefType(genTypeName.typeData)

    val fb = newFunctionBuilder(genFunctionName.searchReflectiveProxy)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val methodIdParam = fb.addParam("methodId", Int32)
    fb.setResultType(RefType(HeapType.Func))

    val instrs = fb

    val reflectiveProxies =
      fb.addLocal("reflectiveProxies", Types.RefType(genTypeName.reflectiveProxies))
    val size = fb.addLocal("size", Types.Int32)
    val i = fb.addLocal("i", Types.Int32)

    instrs += LocalGet(typeDataParam)
    instrs += StructGet(
      genTypeName.typeData,
      genFieldIdx.typeData.reflectiveProxiesIdx
    )
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
      instrs += ArrayGet(genTypeName.reflectiveProxies)

      instrs += StructGet(
        genTypeName.reflectiveProxy,
        genFieldIdx.reflectiveProxy.nameIdx
      )
      instrs += LocalGet(methodIdParam)
      instrs += I32Eq

      instrs.ifThen() {
        instrs += LocalGet(reflectiveProxies)
        instrs += LocalGet(i)
        instrs += ArrayGet(genTypeName.reflectiveProxies)

        // get function reference
        instrs += StructGet(
          genTypeName.reflectiveProxy,
          genFieldIdx.reflectiveProxy.funcIdx
        )
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
    instrs += Call(genFunctionName.jsGlobalRefGet)
    instrs += Call(genFunctionName.jsNewArray)
    // Originally, exception is thrown from JS saying e.g. "obj2.z1__ is not a function"
    instrs ++= ctx.getConstantStringInstr("Method not found")
    instrs += Call(genFunctionName.jsArrayPush)
    instrs += Call(genFunctionName.jsNew)
    instrs += ExternConvertAny
    instrs += Throw(genTagName.exceptionTagName)

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
    val fb = newFunctionBuilder(genFunctionName.clone(baseRef))
    val fromParam = fb.addParam("from", RefType(genTypeName.ObjectStruct))
    fb.setResultType(RefType(genTypeName.ObjectStruct))
    fb.setFunctionType(genTypeName.cloneFunctionType)

    val instrs = fb

    val arrayTypeRef = ArrayTypeRef(baseRef, 1)

    val arrayStructTypeName = genTypeName.forArrayClass(arrayTypeRef)
    val arrayClassType = RefType(arrayStructTypeName)

    val underlyingArrayTypeName = genTypeName.underlyingOf(arrayTypeRef)
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
    instrs += StructGet(arrayStructTypeName, genFieldIdx.objStruct.uniqueRegularField)
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
    instrs += StructGet(arrayStructTypeName, genFieldIdx.objStruct.vtable) // vtable
    instrs += GlobalGet(genGlobalName.arrayClassITable) // itable
    instrs += LocalGet(resultUnderlyingLocal)
    instrs += StructNew(arrayStructTypeName)

    fb.buildAndAddToModule()
  }

}
