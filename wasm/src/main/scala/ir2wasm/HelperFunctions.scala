package wasm.ir2wasm

import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Names => IRNames}
import org.scalajs.linker.standard.LinkedClass
import org.scalajs.ir.ClassKind

import wasm.wasm4s._
import wasm.wasm4s.WasmContext._
import wasm.wasm4s.Names._
import wasm.wasm4s.Types._
import wasm.wasm4s.WasmInstr._

import EmbeddedConstants._
import TypeTransformer._
import wasm4s.Defaults

object HelperFunctions {

  def genGlobalHelpers()(implicit ctx: WasmContext): Unit = {
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
  }

  private def genStringLiteral()(implicit ctx: WasmContext): Unit = {
    import WasmTypeName.WasmArrayTypeName
    import WasmImmediate._
    val fctx = WasmFunctionContext(
      WasmFunctionName.stringLiteral,
      List("offset" -> WasmInt32, "size" -> WasmInt32, "stringIndex" -> WasmInt32),
      List(WasmRefType.any)
    )
    val List(offsetParam, sizeParam, stringIndexParam) = fctx.paramIndices
    val str = fctx.addLocal("str", WasmRefType.any)

    import fctx.instrs

    fctx.block(WasmRefType.any) { cacheHit =>
      instrs += GLOBAL_GET(GlobalIdx(WasmGlobalName.WasmGlobalStringLiteralCache))
      instrs += LOCAL_GET(stringIndexParam)
      instrs += ARRAY_GET(TypeIdx(WasmArrayTypeName.anyArray))

      instrs += BR_ON_NON_NULL(cacheHit)

      // cache miss, create a new string and cache it
      instrs += GLOBAL_GET(GlobalIdx(WasmGlobalName.WasmGlobalStringLiteralCache))
      instrs += LOCAL_GET(stringIndexParam)

      instrs += LOCAL_GET(offsetParam)
      instrs += LOCAL_GET(sizeParam)
      instrs += ARRAY_NEW_DATA(TypeIdx(WasmArrayTypeName.i16Array), DataIdx(WasmDataName.string))
      instrs += CALL(FuncIdx(WasmFunctionName.createStringFromData))
      instrs += LOCAL_TEE(str)
      instrs += ARRAY_SET(TypeIdx(WasmArrayTypeName.anyArray))

      instrs += LOCAL_GET(str)
    }

    fctx.buildAndAddToContext()
  }

  /** `createStringFromData: (ref array u16) -> (ref any)` (representing a `string`). */
  private def genCreateStringFromData()(implicit ctx: WasmContext): Unit = {
    import WasmImmediate._
    import WasmTypeName.WasmArrayTypeName

    val dataType = WasmRefType(WasmHeapType.Type(WasmArrayTypeName.i16Array))

    val fctx = WasmFunctionContext(
      WasmFunctionName.createStringFromData,
      List("data" -> dataType),
      List(WasmRefType.any)
    )

    val List(dataParam) = fctx.paramIndices

    import fctx.instrs

    val lenLocal = fctx.addLocal("len", WasmInt32)
    val iLocal = fctx.addLocal("i", WasmInt32)
    val resultLocal = fctx.addLocal("result", WasmRefType.any)

    // len := data.length
    instrs += LOCAL_GET(dataParam)
    instrs += ARRAY_LEN
    instrs += LOCAL_SET(lenLocal)

    // i := 0
    instrs += I32_CONST(I32(0))
    instrs += LOCAL_SET(iLocal)

    // result := ""
    instrs += CALL(FuncIdx(WasmFunctionName.emptyString))
    instrs += LOCAL_SET(resultLocal)

    fctx.loop() { labelLoop =>
      // if i == len
      instrs += LOCAL_GET(iLocal)
      instrs += LOCAL_GET(lenLocal)
      instrs += I32_EQ
      fctx.ifThen() {
        // then return result
        instrs += LOCAL_GET(resultLocal)
        instrs += RETURN
      }

      // result := concat(result, charToString(data(i)))
      instrs += LOCAL_GET(resultLocal)
      instrs += LOCAL_GET(dataParam)
      instrs += LOCAL_GET(iLocal)
      instrs += ARRAY_GET_U(TypeIdx(WasmArrayTypeName.i16Array))
      instrs += CALL(FuncIdx(WasmFunctionName.charToString))
      instrs += CALL(FuncIdx(WasmFunctionName.stringConcat))
      instrs += LOCAL_SET(resultLocal)

      // i := i - 1
      instrs += LOCAL_GET(iLocal)
      instrs += I32_CONST(I32(1))
      instrs += I32_ADD
      instrs += LOCAL_SET(iLocal)

      // loop back to the beginning
      instrs += BR(labelLoop)
    } // end loop $loop
    instrs += UNREACHABLE

    fctx.buildAndAddToContext()
  }

  /** `typeDataName: (ref typeData) -> (ref any)` (representing a `string`).
    *
    * Initializes the `name` field of the given `typeData` if that was not done yet, and returns its
    * value.
    *
    * The computed value is specified by `java.lang.Class.getName()`. See also the documentation on
    * [[Names.WasmFieldName.typeData.name]] for details.
    *
    * @see
    *   [[https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Class.html#getName()]]
    */
  private def genTypeDataName()(implicit ctx: WasmContext): Unit = {
    import WasmImmediate._
    import WasmTypeName._

    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))
    val nameDataType = WasmRefType(WasmHeapType.Type(WasmArrayTypeName.i16Array))

    val u16ArrayIdx = TypeIdx(WasmArrayTypeName.i16Array)

    val fctx = WasmFunctionContext(
      WasmFunctionName.typeDataName,
      List("typeData" -> typeDataType),
      List(WasmRefType.any)
    )

    val List(typeDataParam) = fctx.paramIndices

    import fctx.instrs

    val componentTypeDataLocal = fctx.addLocal("componentTypeData", typeDataType)
    val componentNameDataLocal = fctx.addLocal("componentNameData", nameDataType)
    val firstCharLocal = fctx.addLocal("firstChar", WasmInt32)
    val nameLocal = fctx.addLocal("name", WasmRefType.any)

    fctx.block(WasmRefType.any) { alreadyInitializedLabel =>
      // br_on_non_null $alreadyInitialized typeData.name
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.nameIdx)
      instrs += BR_ON_NON_NULL(alreadyInitializedLabel)

      // for the STRUCT_SET typeData.name near the end
      instrs += LOCAL_GET(typeDataParam)

      // if typeData.kind == KindArray
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.kindIdx)
      instrs += I32_CONST(I32(WasmFieldName.typeData.KindArray))
      instrs += I32_EQ
      fctx.ifThenElse(WasmRefType.any) {
        // it is an array; compute its name from the component type name

        // <top of stack> := "[", for the CALL to stringConcat near the end
        instrs += I32_CONST(I32('['.toInt))
        instrs += CALL(FuncIdx(WasmFunctionName.charToString))

        // componentTypeData := ref_as_non_null(typeData.componentType)
        instrs += LOCAL_GET(typeDataParam)
        instrs += STRUCT_GET(
          TypeIdx(WasmStructTypeName.typeData),
          WasmFieldName.typeData.componentTypeIdx
        )
        instrs += REF_AS_NOT_NULL
        instrs += LOCAL_SET(componentTypeDataLocal)

        // switch (componentTypeData.kind)
        // the result of this switch is the string that must come after "["
        fctx.switch(WasmRefType.any) { () =>
          // scrutinee
          instrs += LOCAL_GET(componentTypeDataLocal)
          instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.kindIdx)
        }(
          List(WasmFieldName.typeData.KindBoolean) -> { () =>
            instrs += I32_CONST(I32('Z'.toInt))
            instrs += CALL(FuncIdx(WasmFunctionName.charToString))
          },
          List(WasmFieldName.typeData.KindChar) -> { () =>
            instrs += I32_CONST(I32('C'.toInt))
            instrs += CALL(FuncIdx(WasmFunctionName.charToString))
          },
          List(WasmFieldName.typeData.KindByte) -> { () =>
            instrs += I32_CONST(I32('B'.toInt))
            instrs += CALL(FuncIdx(WasmFunctionName.charToString))
          },
          List(WasmFieldName.typeData.KindShort) -> { () =>
            instrs += I32_CONST(I32('S'.toInt))
            instrs += CALL(FuncIdx(WasmFunctionName.charToString))
          },
          List(WasmFieldName.typeData.KindInt) -> { () =>
            instrs += I32_CONST(I32('I'.toInt))
            instrs += CALL(FuncIdx(WasmFunctionName.charToString))
          },
          List(WasmFieldName.typeData.KindLong) -> { () =>
            instrs += I32_CONST(I32('J'.toInt))
            instrs += CALL(FuncIdx(WasmFunctionName.charToString))
          },
          List(WasmFieldName.typeData.KindFloat) -> { () =>
            instrs += I32_CONST(I32('F'.toInt))
            instrs += CALL(FuncIdx(WasmFunctionName.charToString))
          },
          List(WasmFieldName.typeData.KindDouble) -> { () =>
            instrs += I32_CONST(I32('D'.toInt))
            instrs += CALL(FuncIdx(WasmFunctionName.charToString))
          },
          List(WasmFieldName.typeData.KindArray) -> { () =>
            // the component type is an array; get its own name
            instrs += LOCAL_GET(componentTypeDataLocal)
            instrs += CALL(FuncIdx(WasmFunctionName.typeDataName))
          }
        ) { () =>
          // default: the component type is neither a primitive nor an array;
          // concatenate "L" + <its own name> + ";"
          instrs += I32_CONST(I32('L'.toInt))
          instrs += CALL(FuncIdx(WasmFunctionName.charToString))
          instrs += LOCAL_GET(componentTypeDataLocal)
          instrs += CALL(FuncIdx(WasmFunctionName.typeDataName))
          instrs += CALL(FuncIdx(WasmFunctionName.stringConcat))
          instrs += I32_CONST(I32(';'.toInt))
          instrs += CALL(FuncIdx(WasmFunctionName.charToString))
          instrs += CALL(FuncIdx(WasmFunctionName.stringConcat))
        }

        // At this point, the stack contains "[" and the string that must be concatenated with it
        instrs += CALL(FuncIdx(WasmFunctionName.stringConcat))
      } {
        // it is not an array; its name is stored in nameData
        instrs += LOCAL_GET(typeDataParam)
        instrs += STRUCT_GET(
          TypeIdx(WasmStructTypeName.typeData),
          WasmFieldName.typeData.nameDataIdx
        )
        instrs += REF_AS_NOT_NULL
        instrs += CALL(FuncIdx(WasmFunctionName.createStringFromData))
      }

      // typeData.name := <top of stack> ; leave it on the stack
      instrs += LOCAL_TEE(nameLocal)
      instrs += STRUCT_SET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.nameIdx)
      instrs += LOCAL_GET(nameLocal)
    }

    fctx.buildAndAddToContext()
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
    import WasmImmediate._
    import WasmTypeName.WasmStructTypeName

    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))

    val fctx = WasmFunctionContext(
      WasmFunctionName.createClassOf,
      List("typeData" -> typeDataType),
      List(WasmRefType(WasmHeapType.ClassType))
    )

    val List(typeDataParam) = fctx.paramIndices

    import fctx.instrs

    val classInstanceLocal = fctx.addLocal("classInstance", WasmRefType(WasmHeapType.ClassType))

    // classInstance := newDefault$java.lang.Class()
    // leave it on the stack for the constructor call
    instrs += CALL(FuncIdx(WasmFunctionName.newDefault(IRNames.ClassClass)))
    instrs += LOCAL_TEE(classInstanceLocal)

    /* The JS object containing metadata to pass as argument to the `jl.Class` constructor.
     * Specified by https://lampwww.epfl.ch/~doeraene/sjsir-semantics/#sec-sjsir-createclassdataof
     * Leave it on the stack.
     */
    instrs += CALL(FuncIdx(WasmFunctionName.jsNewObject))
    // "__typeData": typeData (TODO hide this better? although nobody will notice anyway)
    instrs ++= ctx.getConstantStringInstr("__typeData")
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    // "name": typeDataName(typeData)
    instrs ++= ctx.getConstantStringInstr("name")
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(FuncIdx(WasmFunctionName.typeDataName))
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    // "isPrimitive": (typeData.kind <= KindLastPrimitive)
    instrs ++= ctx.getConstantStringInstr("isPrimitive")
    instrs += LOCAL_GET(typeDataParam)
    instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.kindIdx)
    instrs += I32_CONST(I32(WasmFieldName.typeData.KindLastPrimitive))
    instrs += I32_LE_U
    instrs += CALL(FuncIdx(WasmFunctionName.box(IRTypes.BooleanRef)))
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    // "isArrayClass": (typeData.kind == KindArray)
    instrs ++= ctx.getConstantStringInstr("isArrayClass")
    instrs += LOCAL_GET(typeDataParam)
    instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.kindIdx)
    instrs += I32_CONST(I32(WasmFieldName.typeData.KindArray))
    instrs += I32_EQ
    instrs += CALL(FuncIdx(WasmFunctionName.box(IRTypes.BooleanRef)))
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    // "isInterface": (typeData.kind == KindInterface)
    instrs ++= ctx.getConstantStringInstr("isInterface")
    instrs += LOCAL_GET(typeDataParam)
    instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.kindIdx)
    instrs += I32_CONST(I32(WasmFieldName.typeData.KindInterface))
    instrs += I32_EQ
    instrs += CALL(FuncIdx(WasmFunctionName.box(IRTypes.BooleanRef)))
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    // "isInstance": closure(isInstance, typeData)
    instrs ++= ctx.getConstantStringInstr("isInstance")
    instrs += ctx.refFuncWithDeclaration(WasmFunctionName.isInstance)
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(FuncIdx(WasmFunctionName.closure))
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    // "isAssignableFrom": closure(isAssignableFrom, typeData)
    instrs ++= ctx.getConstantStringInstr("isAssignableFrom")
    instrs += ctx.refFuncWithDeclaration(WasmFunctionName.isAssignableFromExternal)
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(FuncIdx(WasmFunctionName.closure))
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    // "checkCast": closure(checkCast, typeData)
    instrs ++= ctx.getConstantStringInstr("checkCast")
    instrs += ctx.refFuncWithDeclaration(WasmFunctionName.checkCast)
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(FuncIdx(WasmFunctionName.closure))
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    // "isAssignableFrom": closure(isAssignableFrom, typeData)
    // "getComponentType": closure(getComponentType, typeData)
    instrs ++= ctx.getConstantStringInstr("getComponentType")
    instrs += ctx.refFuncWithDeclaration(WasmFunctionName.getComponentType)
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(FuncIdx(WasmFunctionName.closure))
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    // "newArrayOfThisClass": closure(newArrayOfThisClass, typeData)
    instrs ++= ctx.getConstantStringInstr("newArrayOfThisClass")
    instrs += ctx.refFuncWithDeclaration(WasmFunctionName.newArrayOfThisClass)
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(FuncIdx(WasmFunctionName.closure))
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))

    // Call java.lang.Class::<init>(dataObject)
    instrs += CALL(
      FuncIdx(
        WasmFunctionName(
          IRTrees.MemberNamespace.Constructor,
          IRNames.ClassClass,
          SpecialNames.ClassCtor
        )
      )
    )

    // typeData.classOf := classInstance
    instrs += LOCAL_GET(typeDataParam)
    instrs += LOCAL_GET(classInstanceLocal)
    instrs += STRUCT_SET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.classOfIdx)

    // <top-of-stack> := classInstance for the implicit return
    instrs += LOCAL_GET(classInstanceLocal)

    fctx.buildAndAddToContext()
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
    import WasmImmediate._
    import WasmTypeName.WasmStructTypeName

    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))

    val fctx = WasmFunctionContext(
      WasmFunctionName.getClassOf,
      List("typeData" -> typeDataType),
      List(WasmRefType(WasmHeapType.ClassType))
    )

    val List(typeDataParam) = fctx.paramIndices

    import fctx.instrs

    fctx.block(WasmRefType(WasmHeapType.ClassType)) { alreadyInitializedLabel =>
      // fast path
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.classOfIdx)
      instrs += BR_ON_NON_NULL(alreadyInitializedLabel)
      // slow path
      instrs += LOCAL_GET(typeDataParam)
      instrs += CALL(FuncIdx(WasmFunctionName.createClassOf))
    } // end bock alreadyInitializedLabel

    fctx.buildAndAddToContext()
  }

  /** `arrayTypeData: (ref typeData), i32 -> (ref $java.lang.Object___vtable)`.
    *
    * Returns the typeData/vtable of an array with `dims` dimensions over the given typeData. `dims`
    * must be be strictly positive.
    */
  private def genArrayTypeData()(implicit ctx: WasmContext): Unit = {
    import WasmImmediate._
    import WasmTypeName._

    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))
    val objectVTableType = WasmRefType(
      WasmHeapType.Type(WasmTypeName.WasmVTableTypeName.ObjectVTable)
    )

    /* Array classes extend Cloneable, Serializable and Object.
     * Filter out the ones that do not have run-time type info at all, as
     * we do for other classes.
     */
    val strictAncestors =
      List(IRNames.CloneableClass, IRNames.SerializableClass, IRNames.ObjectClass)
        .filter(name => ctx.getClassInfoOption(name).exists(_.hasRuntimeTypeInfo))

    val fctx = WasmFunctionContext(
      WasmFunctionName.arrayTypeData,
      List("typeData" -> typeDataType, "dims" -> WasmInt32),
      List(objectVTableType)
    )

    val List(typeDataParam, dimsParam) = fctx.paramIndices

    import fctx.instrs

    val arrayTypeDataLocal = fctx.addLocal("arrayTypeData", objectVTableType)

    fctx.loop() { loopLabel =>
      fctx.block(objectVTableType) { arrayOfIsNonNullLabel =>
        // br_on_non_null $arrayOfIsNonNull typeData.arrayOf
        instrs += LOCAL_GET(typeDataParam)
        instrs += STRUCT_GET(
          TypeIdx(WasmStructTypeName.typeData),
          WasmFieldName.typeData.arrayOfIdx
        )
        instrs += BR_ON_NON_NULL(arrayOfIsNonNullLabel)

        // <top-of-stack> := typeData ; for the <old typeData>.arrayOf := ... later on
        instrs += LOCAL_GET(typeDataParam)

        // typeData := new typeData(...)
        instrs += REF_NULL(HeapType(WasmHeapType.Simple.None)) // nameData
        instrs += I32_CONST(I32(WasmFieldName.typeData.KindArray)) // kind = KindArray
        instrs += I32_CONST(I32(0)) // specialInstanceTypes = 0

        // strictAncestors
        for (strictAncestor <- strictAncestors)
          instrs += GLOBAL_GET(GlobalIdx(WasmGlobalName.WasmGlobalVTableName(strictAncestor)))
        instrs += ARRAY_NEW_FIXED(
          TypeIdx(WasmArrayTypeName.typeDataArray),
          I32(strictAncestors.size)
        )

        instrs += LOCAL_GET(typeDataParam) // componentType
        instrs += REF_NULL(HeapType(WasmHeapType.Simple.None)) // name
        instrs += REF_NULL(HeapType(WasmHeapType.Simple.None)) // classOf
        instrs += REF_NULL(HeapType(WasmHeapType.Simple.None)) // arrayOf
        instrs += REF_NULL(
          HeapType(WasmHeapType.Type(ctx.cloneFunctionTypeName))
        ) // clone
        instrs ++= ctx
          .calculateGlobalVTable(IRNames.ObjectClass)
          .map(method => WasmInstr.REF_FUNC(method.name))
        instrs += STRUCT_NEW(TypeIdx(WasmTypeName.WasmVTableTypeName.ObjectVTable))
        instrs += LOCAL_TEE(arrayTypeDataLocal)

        // <old typeData>.arrayOf := typeData
        instrs += STRUCT_SET(
          TypeIdx(WasmStructTypeName.typeData),
          WasmFieldName.typeData.arrayOfIdx
        )

        // put arrayTypeData back on the stack
        instrs += LOCAL_GET(arrayTypeDataLocal)
      } // end block $arrayOfIsNonNullLabel

      // dims := dims - 1 -- leave dims on the stack
      instrs += LOCAL_GET(dimsParam)
      instrs += I32_CONST(I32(1))
      instrs += I32_SUB
      instrs += LOCAL_TEE(dimsParam)

      // if dims == 0 then
      //   return typeData.arrayOf (which is on the stack)
      instrs += I32_EQZ
      fctx.ifThen(WasmFunctionSignature(List(objectVTableType), List(objectVTableType))) {
        instrs += RETURN
      }

      // typeData := typeData.arrayOf (which is on the stack), then loop back to the beginning
      instrs += LOCAL_SET(typeDataParam)
      instrs += BR(loopLabel)
    } // end loop $loop
    instrs += UNREACHABLE

    fctx.buildAndAddToContext()
  }

  /** `isInstance: (ref typeData), anyref -> i32` (a boolean).
    *
    * Tests whether the given value is a non-null instance of the given type.
    *
    * Specified by `"isInstance"` at
    * [[https://lampwww.epfl.ch/~doeraene/sjsir-semantics/#sec-sjsir-createclassdataof]].
    */
  private def genIsInstance()(implicit ctx: WasmContext): Unit = {
    import WasmImmediate._
    import WasmTypeName.WasmStructTypeName
    import WasmFieldName.typeData._

    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))
    val objectRefType = WasmRefType(
      WasmHeapType.Type(WasmTypeName.WasmStructTypeName(IRNames.ObjectClass))
    )

    val fctx = WasmFunctionContext(
      WasmFunctionName.isInstance,
      List("typeData" -> typeDataType, "value" -> WasmAnyRef),
      List(WasmInt32)
    )

    val List(typeDataParam, valueParam) = fctx.paramIndices

    import fctx.instrs

    val valueNonNullLocal = fctx.addLocal("valueNonNull", WasmRefType.any)
    val specialInstanceTypesLocal = fctx.addLocal("specialInstanceTypes", WasmInt32)

    // valueNonNull := as_non_null value; return false if null
    fctx.block(WasmRefType.any) { nonNullLabel =>
      instrs += LOCAL_GET(valueParam)
      instrs += BR_ON_NON_NULL(nonNullLabel)
      instrs += I32_CONST(I32(0))
      instrs += RETURN
    }
    instrs += LOCAL_SET(valueNonNullLocal)

    // switch (typeData.kind)
    fctx.switch(WasmInt32) { () =>
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), kindIdx)
    }(
      // case anyPrimitiveKind => false
      (KindVoid to KindLastPrimitive).toList -> { () =>
        instrs += I32_CONST(I32(0))
      },
      // case KindObject => true
      List(KindObject) -> { () =>
        instrs += I32_CONST(I32(1))
      },
      // for each boxed class, the corresponding primitive type test
      List(KindBoxedUnit) -> { () =>
        instrs += LOCAL_GET(valueNonNullLocal)
        instrs += CALL(FuncIdx(WasmFunctionName.isUndef))
      },
      List(KindBoxedBoolean) -> { () =>
        instrs += LOCAL_GET(valueNonNullLocal)
        instrs += CALL(FuncIdx(WasmFunctionName.typeTest(IRTypes.BooleanRef)))
      },
      List(KindBoxedCharacter) -> { () =>
        instrs += LOCAL_GET(valueNonNullLocal)
        val structTypeName = WasmStructTypeName(SpecialNames.CharBoxClass)
        instrs += REF_TEST(HeapType(Types.WasmHeapType.Type(structTypeName)))
      },
      List(KindBoxedByte) -> { () =>
        instrs += LOCAL_GET(valueNonNullLocal)
        instrs += CALL(FuncIdx(WasmFunctionName.typeTest(IRTypes.ByteRef)))
      },
      List(KindBoxedShort) -> { () =>
        instrs += LOCAL_GET(valueNonNullLocal)
        instrs += CALL(FuncIdx(WasmFunctionName.typeTest(IRTypes.ShortRef)))
      },
      List(KindBoxedInteger) -> { () =>
        instrs += LOCAL_GET(valueNonNullLocal)
        instrs += CALL(FuncIdx(WasmFunctionName.typeTest(IRTypes.IntRef)))
      },
      List(KindBoxedLong) -> { () =>
        instrs += LOCAL_GET(valueNonNullLocal)
        val structTypeName = WasmStructTypeName(SpecialNames.LongBoxClass)
        instrs += REF_TEST(HeapType(Types.WasmHeapType.Type(structTypeName)))
      },
      List(KindBoxedFloat) -> { () =>
        instrs += LOCAL_GET(valueNonNullLocal)
        instrs += CALL(FuncIdx(WasmFunctionName.typeTest(IRTypes.FloatRef)))
      },
      List(KindBoxedDouble) -> { () =>
        instrs += LOCAL_GET(valueNonNullLocal)
        instrs += CALL(FuncIdx(WasmFunctionName.typeTest(IRTypes.DoubleRef)))
      },
      List(KindBoxedString) -> { () =>
        instrs += LOCAL_GET(valueNonNullLocal)
        instrs += CALL(FuncIdx(WasmFunctionName.isString))
      },
      // case KindJSType => trap (TODO: don't trap for JS *class*es)
      List(KindJSType) -> { () =>
        instrs += UNREACHABLE
      }
    ) { () =>
      // case _ =>

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
       */
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), specialInstanceTypesIdx)
      instrs += LOCAL_TEE(specialInstanceTypesLocal)
      instrs += I32_CONST(I32(0))
      instrs += I32_NE
      fctx.ifThen() {
        // Load (1 << jsValueType(valueNonNull))
        instrs += I32_CONST(I32(1))
        instrs += LOCAL_GET(valueNonNullLocal)
        instrs += CALL(FuncIdx(WasmFunctionName.jsValueType))
        instrs += I32_SHL

        // if ((... & specialInstanceTypes) != 0)
        instrs += LOCAL_GET(specialInstanceTypesLocal)
        instrs += I32_AND
        instrs += I32_CONST(I32(0))
        instrs += I32_NE
        fctx.ifThen() {
          // then return true
          instrs += I32_CONST(I32(1))
          instrs += RETURN
        }
      }

      // Get the vtable and delegate to isAssignableFrom

      // Load typeData
      instrs += LOCAL_GET(typeDataParam)

      // Load the vtable; return false if it is not one of our object
      fctx.block(objectRefType) { ourObjectLabel =>
        // Try cast to jl.Object
        instrs += LOCAL_GET(valueNonNullLocal)
        instrs += BR_ON_CAST(
          CastFlags(false, false),
          ourObjectLabel,
          HeapType(WasmHeapType.Simple.Any),
          HeapType(objectRefType.heapType)
        )

        // on cast fail, return false
        instrs += I32_CONST(I32(0))
        instrs += RETURN
      }
      instrs += STRUCT_GET(TypeIdx(WasmStructTypeName(IRNames.ObjectClass)), StructFieldIdx.vtable)

      // Call isAssignableFrom
      instrs += CALL(FuncIdx(WasmFunctionName.isAssignableFrom))
    }

    fctx.buildAndAddToContext()
  }

  /** `isAssignableFromExternal: (ref typeData), anyref -> i32` (a boolean).
    *
    * This is the underlying func for the `isAssignableFrom()` closure inside class data objects.
    */
  private def genIsAssignableFromExternal()(implicit ctx: WasmContext): Unit = {
    import WasmImmediate._

    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))

    val fctx = WasmFunctionContext(
      WasmFunctionName.isAssignableFromExternal,
      List("typeData" -> typeDataType, "from" -> WasmAnyRef),
      List(WasmInt32)
    )

    val List(typeDataParam, fromParam) = fctx.paramIndices

    import fctx.instrs

    // load typeData
    instrs += LOCAL_GET(typeDataParam)

    // load ref.cast<typeData> from["__typeData"] (as a JS selection)
    instrs += LOCAL_GET(fromParam)
    instrs ++= ctx.getConstantStringInstr("__typeData")
    instrs += CALL(FuncIdx(WasmFunctionName.jsSelect))
    instrs += REF_CAST(HeapType(typeDataType.heapType))

    // delegate to isAssignableFrom
    instrs += CALL(FuncIdx(WasmFunctionName.isAssignableFrom))

    fctx.buildAndAddToContext()
  }

  /** `isAssignableFrom: (ref typeData), (ref typeData) -> i32` (a boolean).
    *
    * Specified by `java.lang.Class.isAssignableFrom(Class)`.
    */
  private def genIsAssignableFrom()(implicit ctx: WasmContext): Unit = {
    import WasmImmediate._
    import WasmTypeName._
    import WasmFieldName.typeData._

    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))

    val fctx = WasmFunctionContext(
      WasmFunctionName.isAssignableFrom,
      List("typeData" -> typeDataType, "fromTypeData" -> typeDataType),
      List(WasmInt32)
    )

    val List(typeDataParam, fromTypeDataParam) = fctx.paramIndices

    import fctx.instrs

    val fromAncestorsLocal = fctx.addLocal(
      "fromAncestorsLocal",
      WasmRefType(WasmHeapType.Type(WasmArrayTypeName.typeDataArray))
    )
    val lenLocal = fctx.addLocal("len", WasmInt32)
    val iLocal = fctx.addLocal("i", WasmInt32)

    // if (fromTypeData eq typeData)
    instrs += LOCAL_GET(fromTypeDataParam)
    instrs += LOCAL_GET(typeDataParam)
    instrs += REF_EQ
    fctx.ifThen() {
      // then return true
      instrs += I32_CONST(I32(1))
      instrs += RETURN
    }

    // "Tail call" loop for diving into array component types
    fctx.loop(WasmInt32) { loopForArrayLabel =>
      // switch (typeData.kind)
      fctx.switch(WasmInt32) { () =>
        // typeData.kind
        instrs += LOCAL_GET(typeDataParam)
        instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), kindIdx)
      }(
        // case anyPrimitiveKind => return false
        (KindVoid to KindLastPrimitive).toList -> { () =>
          instrs += I32_CONST(I32(0))
        },
        // case KindArray => check that from is an array, recurse into component types
        List(KindArray) -> { () =>
          fctx.block() { fromComponentTypeIsNullLabel =>
            // fromTypeData := fromTypeData.componentType; jump out if null
            instrs += LOCAL_GET(fromTypeDataParam)
            instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), componentTypeIdx)
            instrs += BR_ON_NULL(fromComponentTypeIsNullLabel)
            instrs += LOCAL_SET(fromTypeDataParam)

            // typeData := ref.as_non_null typeData.componentType (OK because KindArray)
            instrs += LOCAL_GET(typeDataParam)
            instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), componentTypeIdx)
            instrs += REF_AS_NOT_NULL
            instrs += LOCAL_SET(typeDataParam)

            // loop back ("tail call")
            instrs += BR(loopForArrayLabel)
          }

          // return false
          instrs += I32_CONST(I32(0))
        },
        // case KindObject => return (fromTypeData.kind > KindLastPrimitive)
        List(KindObject) -> { () =>
          instrs += LOCAL_GET(fromTypeDataParam)
          instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), kindIdx)
          instrs += I32_CONST(I32(KindLastPrimitive))
          instrs += I32_GT_U
        }
      ) { () =>
        // All other cases: test whether `fromTypeData.strictAncestors` contains `typeData`

        fctx.block() { fromAncestorsIsNullLabel =>
          // fromAncestors := fromTypeData.strictAncestors; go to fromAncestorsIsNull if null
          instrs += LOCAL_GET(fromTypeDataParam)
          instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), strictAncestorsIdx)
          instrs += BR_ON_NULL(fromAncestorsIsNullLabel)
          instrs += LOCAL_TEE(fromAncestorsLocal)

          // if fromAncestors contains typeData, return true

          // len := fromAncestors.length
          instrs += ARRAY_LEN
          instrs += LOCAL_SET(lenLocal)

          // i := 0
          instrs += I32_CONST(I32(0))
          instrs += LOCAL_SET(iLocal)

          // while (i != len)
          fctx.whileLoop() {
            instrs += LOCAL_GET(iLocal)
            instrs += LOCAL_GET(lenLocal)
            instrs += I32_NE
          } {
            // if (fromAncestors[i] eq typeData)
            instrs += LOCAL_GET(fromAncestorsLocal)
            instrs += LOCAL_GET(iLocal)
            instrs += ARRAY_GET(TypeIdx(WasmArrayTypeName.typeDataArray))
            instrs += LOCAL_GET(typeDataParam)
            instrs += REF_EQ
            fctx.ifThen() {
              // then return true
              instrs += I32_CONST(I32(1))
              instrs += RETURN
            }

            // i := i + 1
            instrs += LOCAL_GET(iLocal)
            instrs += I32_CONST(I32(1))
            instrs += I32_ADD
            instrs += LOCAL_SET(iLocal)
          }
        }

        // from.strictAncestors is null or does not contain typeData
        // return false
        instrs += I32_CONST(I32(0))
      }
    }

    fctx.buildAndAddToContext()
  }

  /** `checkCast: (ref typeData), anyref -> anyref`.
    *
    * Casts the given value to the given type; subject to undefined behaviors.
    */
  private def genCheckCast()(implicit ctx: WasmContext): Unit = {
    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))

    val fctx = WasmFunctionContext(
      WasmFunctionName.checkCast,
      List("typeData" -> typeDataType, "value" -> WasmAnyRef),
      List(WasmAnyRef)
    )

    val List(typeDataParam, valueParam) = fctx.paramIndices

    import fctx.instrs

    /* Given that we only implement `CheckedBehavior.Unchecked` semantics for
     * now, this is always the identity.
     */

    instrs += LOCAL_GET(valueParam)

    fctx.buildAndAddToContext()
  }

  /** `getComponentType: (ref typeData) -> (ref null jlClass)`.
    *
    * This is the underlying func for the `getComponentType()` closure inside class data objects.
    */
  private def genGetComponentType()(implicit ctx: WasmContext): Unit = {
    import WasmImmediate._
    import WasmTypeName.WasmStructTypeName

    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))

    val fctx = WasmFunctionContext(
      WasmFunctionName.getComponentType,
      List("typeData" -> typeDataType),
      List(WasmRefNullType(WasmHeapType.ClassType))
    )

    val List(typeDataParam) = fctx.paramIndices

    import fctx.instrs

    val componentTypeDataLocal = fctx.addLocal("componentTypeData", typeDataType)

    fctx.block() { nullResultLabel =>
      // Try and extract non-null component type data
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(
        TypeIdx(WasmStructTypeName.typeData),
        WasmFieldName.typeData.componentTypeIdx
      )
      instrs += BR_ON_NULL(nullResultLabel)
      // Get the corresponding classOf
      instrs += CALL(FuncIdx(WasmFunctionName.getClassOf))
      instrs += RETURN
    } // end block nullResultLabel
    instrs += REF_NULL(HeapType(WasmHeapType.ClassType))

    fctx.buildAndAddToContext()
  }

  /** `newArrayOfThisClass: (ref typeData), anyref -> (ref jlObject)`.
    *
    * This is the underlying func for the `newArrayOfThisClass()` closure inside class data objects.
    */
  private def genNewArrayOfThisClass()(implicit ctx: WasmContext): Unit = {
    import WasmImmediate._
    import WasmTypeName.WasmStructTypeName

    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))
    val i32ArrayType = WasmRefType(WasmHeapType.Type(WasmTypeName.WasmArrayTypeName.i32Array))

    val fctx = WasmFunctionContext(
      WasmFunctionName.newArrayOfThisClass,
      List("typeData" -> typeDataType, "lengths" -> WasmAnyRef),
      List(WasmRefType(WasmHeapType.ObjectType))
    )

    val List(typeDataParam, lengthsParam) = fctx.paramIndices

    import fctx.instrs

    val lengthsLenLocal = fctx.addLocal("lengthsLenLocal", WasmInt32)
    val lengthsValuesLocal = fctx.addLocal("lengthsValues", i32ArrayType)
    val iLocal = fctx.addLocal("i", WasmInt32)

    // lengthsLen := lengths.length // as a JS field access
    instrs += LOCAL_GET(lengthsParam)
    instrs ++= ctx.getConstantStringInstr("length")
    instrs += CALL(FuncIdx(WasmFunctionName.jsSelect))
    instrs += CALL(FuncIdx(WasmFunctionName.unbox(IRTypes.IntRef)))
    instrs += LOCAL_TEE(lengthsLenLocal)

    // lengthsValues := array.new<i32Array> lengthsLen
    instrs += ARRAY_NEW_DEFAULT(TypeIdx(WasmTypeName.WasmArrayTypeName.i32Array))
    instrs += LOCAL_SET(lengthsValuesLocal)

    // i := 0
    instrs += I32_CONST(I32(0))
    instrs += LOCAL_SET(iLocal)

    // while (i != lengthsLen)
    fctx.whileLoop() {
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
      instrs += CALL(FuncIdx(WasmFunctionName.jsSelect))
      instrs += CALL(FuncIdx(WasmFunctionName.unbox(IRTypes.IntRef)))

      instrs += ARRAY_SET(TypeIdx(WasmTypeName.WasmArrayTypeName.i32Array))

      // i += 1
      instrs += LOCAL_GET(iLocal)
      instrs += I32_CONST(I32(1))
      instrs += I32_ADD
      instrs += LOCAL_SET(iLocal)
    }

    // return newArrayObject(arrayTypeData(typeData, lengthsLen), lengthsValues, 0)
    instrs += LOCAL_GET(typeDataParam)
    instrs += LOCAL_GET(lengthsLenLocal)
    instrs += CALL(FuncIdx(WasmFunctionName.arrayTypeData))
    instrs += LOCAL_GET(lengthsValuesLocal)
    instrs += I32_CONST(I32(0))
    instrs += CALL(FuncIdx(WasmFunctionName.newArrayObject))

    fctx.buildAndAddToContext()
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
    import WasmImmediate._
    import WasmTypeName.WasmStructTypeName

    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))

    val fctx = WasmFunctionContext(
      WasmFunctionName.anyGetClass,
      List("value" -> WasmRefType.any),
      List(WasmRefNullType(WasmHeapType.ClassType))
    )

    val List(valueParam) = fctx.paramIndices

    import fctx.instrs

    val objectTypeIdx = TypeIdx(WasmStructTypeName(IRNames.ObjectClass))
    val typeDataLocal = fctx.addLocal("typeData", typeDataType)
    val doubleValueLocal = fctx.addLocal("doubleValue", WasmFloat64)
    val intValueLocal = fctx.addLocal("intValue", WasmInt32)

    def getHijackedClassTypeDataInstr(className: IRNames.ClassName): WasmInstr =
      GLOBAL_GET(GlobalIdx(WasmGlobalName.WasmGlobalVTableName(IRTypes.ClassRef(className))))

    fctx.block(WasmRefNullType(WasmHeapType.ClassType)) { nonNullClassOfLabel =>
      fctx.block(typeDataType) { gotTypeDataLabel =>
        fctx.block(WasmRefType(WasmHeapType.ObjectType)) { ourObjectLabel =>
          // if value is our object, jump to $ourObject
          instrs += LOCAL_GET(valueParam)
          instrs += BR_ON_CAST(
            CastFlags(false, false),
            ourObjectLabel,
            WasmImmediate.HeapType(WasmHeapType.Simple.Any),
            WasmImmediate.HeapType(WasmHeapType.ObjectType)
          )

          // switch(jsValueType(value)) { ... }
          fctx.switch(typeDataType) { () =>
            // scrutinee
            instrs += LOCAL_GET(valueParam)
            instrs += CALL(FuncIdx(WasmFunctionName.jsValueType))
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
              instrs += CALL(FuncIdx(WasmFunctionName.unbox(IRTypes.DoubleRef)))
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
              fctx.ifThenElse(typeDataType) {
                // then it is a Byte, a Short, or an Integer

                // if intValue.toByte.toInt == intValue
                instrs += LOCAL_GET(intValueLocal)
                instrs += I32_EXTEND8_S
                instrs += LOCAL_GET(intValueLocal)
                instrs += I32_EQ
                fctx.ifThenElse(typeDataType) {
                  // then it is a Byte
                  instrs += getHijackedClassTypeDataInstr(IRNames.BoxedByteClass)
                } {
                  // else, if intValue.toShort.toInt == intValue
                  instrs += LOCAL_GET(intValueLocal)
                  instrs += I32_EXTEND16_S
                  instrs += LOCAL_GET(intValueLocal)
                  instrs += I32_EQ
                  fctx.ifThenElse(typeDataType) {
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
                fctx.ifThenElse(typeDataType) {
                  // then it is a Float
                  instrs += getHijackedClassTypeDataInstr(IRNames.BoxedFloatClass)
                } {
                  // else, if it is NaN
                  instrs += LOCAL_GET(doubleValueLocal)
                  instrs += LOCAL_GET(doubleValueLocal)
                  instrs += F64_NE
                  fctx.ifThenElse(typeDataType) {
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
            instrs += REF_NULL(HeapType(WasmHeapType.ClassType))
            instrs += RETURN
          }

          instrs += BR(gotTypeDataLabel)
        }

        instrs += STRUCT_GET(objectTypeIdx, StructFieldIdx(0))
      }

      instrs += CALL(FuncIdx(WasmFunctionName.getClassOf))
    }

    fctx.buildAndAddToContext()
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
  def genNewArrayObject()(implicit ctx: WasmContext): Unit = {
    import WasmImmediate._
    import WasmTypeName._
    import WasmFieldName.typeData._

    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))
    val i32ArrayType = WasmRefType(WasmHeapType.Type(WasmTypeName.WasmArrayTypeName.i32Array))
    val objectVTableType = WasmRefType(WasmHeapType.Type(WasmVTableTypeName.ObjectVTable))
    val arrayTypeDataType = objectVTableType
    val itablesType = WasmRefNullType(WasmHeapType.Type(WasmArrayType.itables.name))
    val nonNullObjectType = WasmRefType(WasmHeapType.ObjectType)
    val anyArrayType = WasmRefType(WasmHeapType.Type(WasmArrayTypeName.anyArray))

    val fctx = WasmFunctionContext(
      WasmFunctionName.newArrayObject,
      List(
        "arrayTypeData" -> arrayTypeDataType,
        "lengths" -> i32ArrayType,
        "lengthIndex" -> WasmInt32
      ),
      List(nonNullObjectType)
    )

    val List(arrayTypeDataParam, lengthsParam, lengthIndexParam) = fctx.paramIndices

    import fctx.instrs

    val lenLocal = fctx.addLocal("len", WasmInt32)
    val underlyingLocal = fctx.addLocal("underlying", anyArrayType)
    val subLengthIndexLocal = fctx.addLocal("subLengthIndex", WasmInt32)
    val arrayComponentTypeDataLocal = fctx.addLocal("arrayComponentTypeData", arrayTypeDataType)
    val iLocal = fctx.addLocal("i", WasmInt32)

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
    // TODO: this should not be null because of Serializable and Cloneable
    instrs += REF_NULL(HeapType(Types.WasmHeapType.Type(WasmArrayType.itables.name))) // itable

    // Load the first length
    instrs += LOCAL_GET(lengthsParam)
    instrs += LOCAL_GET(lengthIndexParam)
    instrs += ARRAY_GET(TypeIdx(Names.WasmTypeName.WasmArrayTypeName.i32Array))

    // componentTypeData := ref_as_non_null(arrayTypeData.componentType)
    // switch (componentTypeData.kind)
    val switchClauseSig = WasmFunctionSignature(
      List(arrayTypeDataType, itablesType, WasmInt32),
      List(nonNullObjectType)
    )
    fctx.switch(switchClauseSig) { () =>
      // scrutinee
      instrs += LOCAL_GET(arrayTypeDataParam)
      instrs += STRUCT_GET(
        TypeIdx(WasmStructTypeName.typeData),
        WasmFieldName.typeData.componentTypeIdx
      )
      instrs += STRUCT_GET(
        TypeIdx(WasmStructTypeName.typeData),
        WasmFieldName.typeData.kindIdx
      )
    }(
      // For all the primitive types, by construction, this is the bottom dimension
      // case KindPrim => array.new_default underlyingPrimArray; struct.new PrimArray
      primRefsWithArrayTypes.map { case (primRef, kind) =>
        List(kind) -> { () =>
          val arrayTypeRef = IRTypes.ArrayTypeRef(primRef, 1)
          instrs += ARRAY_NEW_DEFAULT(TypeIdx(WasmArrayTypeName.underlyingOf(arrayTypeRef)))
          instrs += STRUCT_NEW(TypeIdx(WasmStructTypeName(arrayTypeRef)))
          () // required for correct type inference
        }
      }: _*
    ) { () =>
      // default -- all non-primitive array types

      // len := <top-of-stack> (which is the first length)
      instrs += LOCAL_TEE(lenLocal)

      // underlying := array.new_default anyArray
      val arrayTypeRef = IRTypes.ArrayTypeRef(IRTypes.ClassRef(IRNames.ObjectClass), 1)
      instrs += ARRAY_NEW_DEFAULT(TypeIdx(WasmArrayTypeName.underlyingOf(arrayTypeRef)))
      instrs += LOCAL_SET(underlyingLocal)

      // subLengthIndex := lengthIndex + 1
      instrs += LOCAL_GET(lengthIndexParam)
      instrs += I32_CONST(I32(1))
      instrs += I32_ADD
      instrs += LOCAL_TEE(subLengthIndexLocal)

      // if subLengthIndex != lengths.length
      instrs += LOCAL_GET(lengthsParam)
      instrs += ARRAY_LEN
      instrs += I32_NE
      fctx.ifThen() {
        // then, recursively initialize all the elements

        // arrayComponentTypeData := ref_cast<arrayTypeDataType> arrayTypeData.componentTypeData
        instrs += LOCAL_GET(arrayTypeDataParam)
        instrs += STRUCT_GET(
          TypeIdx(WasmStructTypeName.typeData),
          WasmFieldName.typeData.componentTypeIdx
        )
        instrs += REF_CAST(HeapType(arrayTypeDataType.heapType))
        instrs += LOCAL_SET(arrayComponentTypeDataLocal)

        // i := 0
        instrs += I32_CONST(I32(0))
        instrs += LOCAL_SET(iLocal)

        // while (i != len)
        fctx.whileLoop() {
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
          instrs += CALL(FuncIdx(WasmFunctionName.newArrayObject))

          instrs += ARRAY_SET(TypeIdx(WasmArrayTypeName.anyArray))

          // i += 1
          instrs += LOCAL_GET(iLocal)
          instrs += I32_CONST(I32(1))
          instrs += I32_ADD
          instrs += LOCAL_SET(iLocal)
        }
      }

      // load underlying; struct.new ObjectArray
      instrs += LOCAL_GET(underlyingLocal)
      instrs += STRUCT_NEW(TypeIdx(WasmStructTypeName(arrayTypeRef)))
    }

    fctx.buildAndAddToContext()
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
  def genInstanceTest(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    import WasmImmediate._
    assert(clazz.kind == ClassKind.Interface)

    val fctx = WasmFunctionContext(
      Names.WasmFunctionName.instanceTest(clazz.name.name),
      List("expr" -> WasmAnyRef),
      List(WasmInt32)
    )
    val List(exprParam) = fctx.paramIndices

    import fctx.instrs

    val itables = fctx.addSyntheticLocal(
      Types.WasmRefNullType(Types.WasmHeapType.Type(WasmArrayType.itables.name))
    )
    val itableIdx = ctx.getItableIdx(clazz.name.name)
    fctx.block(WasmRefNullType(WasmHeapType.Simple.Any)) { testFail =>
      // if expr is not an instance of Object, return false
      instrs += LOCAL_GET(exprParam)
      instrs += BR_ON_CAST_FAIL(
        CastFlags(true, false),
        testFail,
        HeapType(Types.WasmHeapType.Simple.Any),
        HeapType(Types.WasmHeapType.ObjectType)
      )

      // get itables and store
      instrs += STRUCT_GET(TypeIdx(Types.WasmHeapType.ObjectType.typ), StructFieldIdx.itables)
      instrs += LOCAL_SET(itables)

      // Dummy return value from the block
      instrs += REF_NULL(HeapType(WasmHeapType.Simple.Any))

      // if the itables is null (no interfaces are implemented)
      instrs += LOCAL_GET(itables)
      instrs += BR_ON_NULL(testFail)

      instrs += LOCAL_GET(itables)
      instrs += I32_CONST(I32(itableIdx))
      instrs += ARRAY_GET(TypeIdx(WasmTypeName.WasmArrayTypeName.itables))
      instrs += BR_ON_NULL(testFail)

      instrs += I32_CONST(I32(1))
      instrs += RETURN
    } // test fail
    instrs += DROP
    instrs += I32_CONST(I32(0)) // false
    fctx.buildAndAddToContext()
  }

  /** Generate clone function for the given class, if it is concrete and implements the Cloneable
    * interface. The generated clone function will be registered in the typeData of the class (which
    * resides in the vtable of the class), and will be invoked when the `super.clone()` method is
    * called on the class instance.
    */
  def genCloneFunction(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    import WasmImmediate._
    val info = ctx.getClassInfo(clazz.name.name)
    if (info.ancestors.contains(IRNames.CloneableClass) && !info.isAbstract) {
      val heapType =
        WasmHeapType.Type(WasmTypeName.WasmStructTypeName(clazz.name.name))
      val fctx = WasmFunctionContext(
        Names.WasmFunctionName.clone(clazz.name.name),
        List("from" -> WasmRefType(WasmHeapType.ObjectType)),
        List(WasmRefType(WasmHeapType.ObjectType))
      )
      val List(fromParam) = fctx.paramIndices
      import fctx.instrs

      val from = fctx.addSyntheticLocal(WasmRefNullType(heapType))
      val result = fctx.addSyntheticLocal(WasmRefNullType(heapType))

      instrs += LOCAL_GET(fromParam)
      instrs += REF_CAST(HeapType(heapType))
      instrs += LOCAL_SET(from)

      instrs += CALL(FuncIdx(WasmFunctionName.newDefault(clazz.name.name)))
      instrs += LOCAL_SET(result)
      info.allFieldDefs.foreach { field =>
        val fieldIdx = info.getFieldIdx(field.name.name)
        instrs += LOCAL_GET(result)
        instrs += LOCAL_GET(from)
        instrs += STRUCT_GET(TypeIdx(WasmTypeName.WasmStructTypeName(clazz.name.name)), fieldIdx)
        instrs += STRUCT_SET(TypeIdx(WasmTypeName.WasmStructTypeName(clazz.name.name)), fieldIdx)
      }
      instrs += LOCAL_GET(result)
      instrs += REF_AS_NOT_NULL
      val fun = fctx.buildAndAddToContext()
      // fun.typ
    }
  }

  def genNewDefault(clazz: LinkedClass)(implicit ctx: TypeDefinableWasmContext): Unit = {
    val className = clazz.name.name
    val classInfo = ctx.getClassInfo(className)
    assert(clazz.hasDirectInstances)

    val structName = WasmTypeName.WasmStructTypeName(className)
    val fctx = WasmFunctionContext(
      Names.WasmFunctionName.newDefault(className),
      Nil,
      List(WasmRefType(WasmHeapType.Type(structName)))
    )

    import fctx.instrs

    instrs +=
      GLOBAL_GET(WasmImmediate.GlobalIdx(WasmGlobalName.WasmGlobalVTableName(className)))

    val interfaces = classInfo.ancestors.map(ctx.getClassInfo(_)).filter(_.isInterface)
    if (!interfaces.isEmpty)
      instrs +=
        GLOBAL_GET(WasmImmediate.GlobalIdx(WasmGlobalName.WasmGlobalITableName(className)))
    else
      instrs +=
        REF_NULL(WasmImmediate.HeapType(WasmHeapType.Type(WasmArrayType.itables.name)))

    classInfo.allFieldDefs.foreach { f =>
      val ty = transformType(f.ftpe)
      instrs += Defaults.defaultValue(ty)
    }
    instrs += STRUCT_NEW(WasmImmediate.TypeIdx(structName))

    fctx.buildAndAddToContext()
  }

}
