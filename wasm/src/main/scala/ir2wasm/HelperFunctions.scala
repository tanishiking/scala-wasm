package wasm.ir2wasm

import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{ClassKind, Position}
import org.scalajs.linker.standard.LinkedClass

import wasm.wasm4s._
import wasm.wasm4s.WasmContext._
import wasm.wasm4s.Names._
import wasm.wasm4s.Names.WasmTypeName._
import wasm.wasm4s.Types._
import wasm.wasm4s.WasmInstr._

import EmbeddedConstants._
import VarGen._
import TypeTransformer._

object HelperFunctions {
  private implicit val noPos: Position = Position.NoPosition

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
    genIdentityHashCode()
    genSearchReflectiveProxy()
  }

  private def genStringLiteral()(implicit ctx: WasmContext): Unit = {
    val fctx = WasmFunctionContext(
      genFunctionName.stringLiteral,
      List("offset" -> WasmInt32, "size" -> WasmInt32, "stringIndex" -> WasmInt32),
      List(WasmRefType.any)
    )
    val List(offsetParam, sizeParam, stringIndexParam) = fctx.paramIndices
    val str = fctx.addLocal("str", WasmRefType.any)

    import fctx.instrs

    fctx.block(WasmRefType.any) { cacheHit =>
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

    fctx.buildAndAddToContext()
  }

  /** `createStringFromData: (ref array u16) -> (ref any)` (representing a `string`). */
  private def genCreateStringFromData()(implicit ctx: WasmContext): Unit = {
    val dataType = WasmRefType(genTypeName.i16Array)

    val fctx = WasmFunctionContext(
      genFunctionName.createStringFromData,
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
    instrs += I32_CONST(0)
    instrs += LOCAL_SET(iLocal)

    // result := ""
    instrs += CALL(genFunctionName.emptyString)
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

    fctx.buildAndAddToContext()
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

    val fctx = WasmFunctionContext(
      genFunctionName.typeDataName,
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
      instrs += STRUCT_GET(genTypeName.typeData, genFieldIdx.typeData.nameIdx)
      instrs += BR_ON_NON_NULL(alreadyInitializedLabel)

      // for the STRUCT_SET typeData.name near the end
      instrs += LOCAL_GET(typeDataParam)

      // if typeData.kind == KindArray
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(genTypeName.typeData, genFieldIdx.typeData.kindIdx)
      instrs += I32_CONST(KindArray)
      instrs += I32_EQ
      fctx.ifThenElse(WasmRefType.any) {
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
        fctx.switch(WasmRefType.any) { () =>
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
    val typeDataType = WasmRefType(genTypeName.typeData)

    val fctx = WasmFunctionContext(
      genFunctionName.createClassOf,
      List("typeData" -> typeDataType),
      List(WasmRefType(WasmHeapType.ClassType))
    )

    val List(typeDataParam) = fctx.paramIndices

    import fctx.instrs

    val classInstanceLocal = fctx.addLocal("classInstance", WasmRefType(WasmHeapType.ClassType))

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
    val typeDataType = WasmRefType(genTypeName.typeData)

    val fctx = WasmFunctionContext(
      genFunctionName.getClassOf,
      List("typeData" -> typeDataType),
      List(WasmRefType(WasmHeapType.ClassType))
    )

    val List(typeDataParam) = fctx.paramIndices

    import fctx.instrs

    fctx.block(WasmRefType(WasmHeapType.ClassType)) { alreadyInitializedLabel =>
      // fast path
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(genTypeName.typeData, genFieldIdx.typeData.classOfIdx)
      instrs += BR_ON_NON_NULL(alreadyInitializedLabel)
      // slow path
      instrs += LOCAL_GET(typeDataParam)
      instrs += CALL(genFunctionName.createClassOf)
    } // end bock alreadyInitializedLabel

    fctx.buildAndAddToContext()
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

    val fctx = WasmFunctionContext(
      genFunctionName.arrayTypeData,
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
        fctx.switch(WasmRefType(ctx.cloneFunctionTypeName)) { () =>
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
    import genFieldIdx.typeData._

    val typeDataType = WasmRefType(genTypeName.typeData)
    val objectRefType = WasmRefType(genTypeName.forClass(IRNames.ObjectClass))

    val fctx = WasmFunctionContext(
      genFunctionName.isInstance,
      List("typeData" -> typeDataType, "value" -> WasmRefType.anyref),
      List(WasmInt32)
    )

    val List(typeDataParam, valueParam) = fctx.paramIndices

    import fctx.instrs

    val valueNonNullLocal = fctx.addLocal("valueNonNull", WasmRefType.any)
    val specialInstanceTypesLocal = fctx.addLocal("specialInstanceTypes", WasmInt32)

    // switch (typeData.kind)
    fctx.switch(WasmInt32) { () =>
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
        fctx.block(WasmRefType.anyref) { isJSClassInstanceIsNull =>
          // Load value as the argument to the function
          instrs += LOCAL_GET(valueParam)

          // Load the function reference; break if null
          instrs += LOCAL_GET(typeDataParam)
          instrs += STRUCT_GET(genTypeName.typeData, isJSClassInstanceIdx)
          instrs += BR_ON_NULL(isJSClassInstanceIsNull)

          // Call the function
          instrs += CALL_REF(ctx.isJSClassInstanceFuncTypeName)
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
      fctx.block(WasmRefType.any) { nonNullLabel =>
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
      fctx.ifThen() {
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
        fctx.ifThen() {
          // then return true
          instrs += I32_CONST(1)
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

    fctx.buildAndAddToContext()
  }

  /** `isAssignableFromExternal: (ref typeData), anyref -> i32` (a boolean).
    *
    * This is the underlying func for the `isAssignableFrom()` closure inside class data objects.
    */
  private def genIsAssignableFromExternal()(implicit ctx: WasmContext): Unit = {
    val typeDataType = WasmRefType(genTypeName.typeData)

    val fctx = WasmFunctionContext(
      genFunctionName.isAssignableFromExternal,
      List("typeData" -> typeDataType, "from" -> WasmRefType.anyref),
      List(WasmInt32)
    )

    val List(typeDataParam, fromParam) = fctx.paramIndices

    import fctx.instrs

    // load typeData
    instrs += LOCAL_GET(typeDataParam)

    // load ref.cast<typeData> from["__typeData"] (as a JS selection)
    instrs += LOCAL_GET(fromParam)
    instrs ++= ctx.getConstantStringInstr("__typeData")
    instrs += CALL(genFunctionName.jsSelect)
    instrs += REF_CAST(WasmRefType(typeDataType.heapType))

    // delegate to isAssignableFrom
    instrs += CALL(genFunctionName.isAssignableFrom)

    fctx.buildAndAddToContext()
  }

  /** `isAssignableFrom: (ref typeData), (ref typeData) -> i32` (a boolean).
    *
    * Specified by `java.lang.Class.isAssignableFrom(Class)`.
    */
  private def genIsAssignableFrom()(implicit ctx: WasmContext): Unit = {
    import genFieldIdx.typeData._

    val typeDataType = WasmRefType(genTypeName.typeData)

    val fctx = WasmFunctionContext(
      genFunctionName.isAssignableFrom,
      List("typeData" -> typeDataType, "fromTypeData" -> typeDataType),
      List(WasmInt32)
    )

    val List(typeDataParam, fromTypeDataParam) = fctx.paramIndices

    import fctx.instrs

    val fromAncestorsLocal = fctx.addLocal(
      "fromAncestorsLocal",
      WasmRefType(genTypeName.typeDataArray)
    )
    val lenLocal = fctx.addLocal("len", WasmInt32)
    val iLocal = fctx.addLocal("i", WasmInt32)

    // if (fromTypeData eq typeData)
    instrs += LOCAL_GET(fromTypeDataParam)
    instrs += LOCAL_GET(typeDataParam)
    instrs += REF_EQ
    fctx.ifThen() {
      // then return true
      instrs += I32_CONST(1)
      instrs += RETURN
    }

    // "Tail call" loop for diving into array component types
    fctx.loop(WasmInt32) { loopForArrayLabel =>
      // switch (typeData.kind)
      fctx.switch(WasmInt32) { () =>
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
          fctx.block() { fromComponentTypeIsNullLabel =>
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

        fctx.block() { fromAncestorsIsNullLabel =>
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
          fctx.whileLoop() {
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
            fctx.ifThen() {
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

    fctx.buildAndAddToContext()
  }

  /** `checkCast: (ref typeData), anyref -> anyref`.
    *
    * Casts the given value to the given type; subject to undefined behaviors.
    */
  private def genCheckCast()(implicit ctx: WasmContext): Unit = {
    val typeDataType = WasmRefType(genTypeName.typeData)

    val fctx = WasmFunctionContext(
      genFunctionName.checkCast,
      List("typeData" -> typeDataType, "value" -> WasmRefType.anyref),
      List(WasmRefType.anyref)
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
    val typeDataType = WasmRefType(genTypeName.typeData)

    val fctx = WasmFunctionContext(
      genFunctionName.getComponentType,
      List("typeData" -> typeDataType),
      List(WasmRefType.nullable(WasmHeapType.ClassType))
    )

    val List(typeDataParam) = fctx.paramIndices

    import fctx.instrs

    val componentTypeDataLocal = fctx.addLocal("componentTypeData", typeDataType)

    fctx.block() { nullResultLabel =>
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
    instrs += REF_NULL(WasmHeapType.ClassType)

    fctx.buildAndAddToContext()
  }

  /** `newArrayOfThisClass: (ref typeData), anyref -> (ref jlObject)`.
    *
    * This is the underlying func for the `newArrayOfThisClass()` closure inside class data objects.
    */
  private def genNewArrayOfThisClass()(implicit ctx: WasmContext): Unit = {
    val typeDataType = WasmRefType(genTypeName.typeData)
    val i32ArrayType = WasmRefType(genTypeName.i32Array)

    val fctx = WasmFunctionContext(
      genFunctionName.newArrayOfThisClass,
      List("typeData" -> typeDataType, "lengths" -> WasmRefType.anyref),
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
    val typeDataType = WasmRefType(genTypeName.typeData)

    val fctx = WasmFunctionContext(
      genFunctionName.anyGetClass,
      List("value" -> WasmRefType.any),
      List(WasmRefType.nullable(WasmHeapType.ClassType))
    )

    val List(valueParam) = fctx.paramIndices

    import fctx.instrs

    val typeDataLocal = fctx.addLocal("typeData", typeDataType)
    val doubleValueLocal = fctx.addLocal("doubleValue", WasmFloat64)
    val intValueLocal = fctx.addLocal("intValue", WasmInt32)
    val ourObjectLocal = fctx.addLocal("ourObject", WasmRefType(WasmHeapType.ObjectType))

    def getHijackedClassTypeDataInstr(className: IRNames.ClassName): WasmInstr =
      GLOBAL_GET(genGlobalName.forVTable(className))

    fctx.block(WasmRefType.nullable(WasmHeapType.ClassType)) { nonNullClassOfLabel =>
      fctx.block(typeDataType) { gotTypeDataLabel =>
        fctx.block(WasmRefType(WasmHeapType.ObjectType)) { ourObjectLabel =>
          // if value is our object, jump to $ourObject
          instrs += LOCAL_GET(valueParam)
          instrs += BR_ON_CAST(
            ourObjectLabel,
            WasmRefType.any,
            WasmRefType(WasmHeapType.ObjectType)
          )

          // switch(jsValueType(value)) { ... }
          fctx.switch(typeDataType) { () =>
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
            instrs += REF_NULL(WasmHeapType.ClassType)
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
        fctx.ifThenElse(typeDataType) {
          instrs += getHijackedClassTypeDataInstr(IRNames.BoxedCharacterClass)
        } {
          instrs += LOCAL_GET(ourObjectLocal)
          instrs += REF_TEST(WasmRefType(genTypeName.forClass(SpecialNames.LongBoxClass)))
          fctx.ifThenElse(typeDataType) {
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
    import genFieldIdx.typeData._

    val typeDataType = WasmRefType(genTypeName.typeData)
    val i32ArrayType = WasmRefType(genTypeName.i32Array)
    val objectVTableType = WasmRefType(genTypeName.ObjectVTable)
    val arrayTypeDataType = objectVTableType
    val itablesType = WasmRefType.nullable(genTypeName.itables)
    val nonNullObjectType = WasmRefType(WasmHeapType.ObjectType)
    val anyArrayType = WasmRefType(genTypeName.anyArray)

    val fctx = WasmFunctionContext(
      genFunctionName.newArrayObject,
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
    fctx.switch(switchClauseSig) { () =>
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
      fctx.ifThen() {
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

    fctx.buildAndAddToContext()
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
  def genIdentityHashCode()(implicit ctx: WasmContext): Unit = {
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

    val fctx = WasmFunctionContext(
      genFunctionName.identityHashCode,
      List("obj" -> WasmRefType.anyref),
      List(WasmInt32)
    )

    val List(objParam) = fctx.paramIndices

    import fctx.instrs

    val objNonNullLocal = fctx.addLocal("objNonNull", WasmRefType.any)
    val resultLocal = fctx.addLocal("result", WasmInt32)

    // If `obj` is `null`, return 0 (by spec)
    fctx.block(WasmRefType.any) { nonNullLabel =>
      instrs += LOCAL_GET(objParam)
      instrs += BR_ON_NON_NULL(nonNullLabel)
      instrs += I32_CONST(0)
      instrs += RETURN
    }
    instrs += LOCAL_TEE(objNonNullLocal)

    // If `obj` is one of our objects, skip all the jsValueType tests
    instrs += REF_TEST(WasmRefType(WasmHeapType.ObjectType))
    instrs += I32_EQZ
    fctx.ifThen() {
      fctx.switch() { () =>
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
          fctx.block() { descriptionIsNullLabel =>
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
    fctx.ifThen() {
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

    fctx.buildAndAddToContext()
  }

  /** Search for a reflective proxy function with the given `methodId` in the `reflectiveProxies`
    * field in `typeData` and returns the corresponding function reference.
    *
    * `searchReflectiveProxy`: [typeData, i32] -> [(ref func)]
    */
  def genSearchReflectiveProxy()(implicit ctx: WasmContext): Unit = {
    import genFieldIdx.typeData._

    val typeDataType = WasmRefType(genTypeName.typeData)

    val fctx = WasmFunctionContext(
      genFunctionName.searchReflectiveProxy,
      List(
        "typeData" -> typeDataType,
        "methodId" -> WasmInt32
      ),
      List(WasmRefType(WasmHeapType.Func))
    )

    val List(typeDataParam, methodIdParam) = fctx.paramIndices

    import fctx.instrs

    val reflectiveProxies =
      fctx.addLocal("reflectiveProxies", Types.WasmRefType(genTypeName.reflectiveProxies))
    val size = fctx.addLocal("size", Types.WasmInt32)
    val i = fctx.addLocal("i", Types.WasmInt32)

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

    fctx.whileLoop() {
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

      fctx.ifThen() {
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
    assert(clazz.kind == ClassKind.Interface)

    val classInfo = ctx.getClassInfo(clazz.className)

    val fctx = WasmFunctionContext(
      genFunctionName.instanceTest(clazz.name.name),
      List("expr" -> WasmRefType.anyref),
      List(WasmInt32)
    )
    val List(exprParam) = fctx.paramIndices

    import fctx.instrs

    val itables = fctx.addLocal("itables", WasmRefType.nullable(genTypeName.itables))
    val exprNonNullLocal = fctx.addLocal("exprNonNull", WasmRefType.any)

    val itableIdx = ctx.getItableIdx(classInfo)
    fctx.block(WasmRefType.anyref) { testFail =>
      // if expr is not an instance of Object, return false
      instrs += LOCAL_GET(exprParam)
      instrs += BR_ON_CAST_FAIL(
        testFail,
        WasmRefType.anyref,
        WasmRefType(Types.WasmHeapType.ObjectType)
      )

      // get itables and store
      instrs += STRUCT_GET(Types.WasmHeapType.ObjectType.typ, genFieldIdx.objStruct.itables)
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

      fctx.block(anyRefToVoidSig) { isNullLabel =>
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

    fctx.buildAndAddToContext()
  }

  /** Generate clone function for the given class, if it is concrete and implements the Cloneable
    * interface. The generated clone function will be registered in the typeData of the class (which
    * resides in the vtable of the class), and will be invoked when the `super.clone()` method is
    * called on the class instance.
    */
  def genCloneFunction(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    val info = ctx.getClassInfo(clazz.name.name)
    if (info.ancestors.contains(IRNames.CloneableClass) && !info.isAbstract) {
      val heapType =
        WasmHeapType(genTypeName.forClass(clazz.name.name))
      val fctx = WasmFunctionContext(
        genFunctionName.clone(clazz.name.name),
        List("from" -> WasmRefType(WasmHeapType.ObjectType)),
        List(WasmRefType(WasmHeapType.ObjectType))
      )
      val List(fromParam) = fctx.paramIndices
      import fctx.instrs

      val from = fctx.addSyntheticLocal(WasmRefType.nullable(heapType))
      val result = fctx.addSyntheticLocal(WasmRefType.nullable(heapType))

      instrs += LOCAL_GET(fromParam)
      instrs += REF_CAST(WasmRefType(heapType))
      instrs += LOCAL_SET(from)

      instrs += CALL(genFunctionName.newDefault(clazz.name.name))
      instrs += LOCAL_SET(result)
      info.allFieldDefs.foreach { field =>
        val fieldIdx = info.getFieldIdx(field.name.name)
        instrs += LOCAL_GET(result)
        instrs += LOCAL_GET(from)
        instrs += STRUCT_GET(
          genTypeName.forClass(clazz.name.name),
          fieldIdx
        )
        instrs += STRUCT_SET(
          genTypeName.forClass(clazz.name.name),
          fieldIdx
        )
      }
      instrs += LOCAL_GET(result)
      instrs += REF_AS_NOT_NULL
      val fun = fctx.buildAndAddToContext(ctx.cloneFunctionTypeName)
      // fun.typ
    }
  }

  /** Generates the clone function for the given array class. */
  def genArrayCloneFunction(arrayTypeRef: IRTypes.ArrayTypeRef)(implicit ctx: WasmContext): Unit = {
    assert(
      arrayTypeRef.dimensions == 1,
      s"Should not create a specific clone function for the multi-dims array type $arrayTypeRef"
    )

    val fctx = WasmFunctionContext(
      genFunctionName.clone(arrayTypeRef.base),
      List("from" -> WasmRefType(WasmHeapType.ObjectType)),
      List(WasmRefType(WasmHeapType.ObjectType))
    )
    val List(fromParam) = fctx.paramIndices
    import fctx.instrs

    val arrayStructTypeName = genTypeName.forArrayClass(arrayTypeRef)
    val arrayClassType = WasmRefType(arrayStructTypeName)

    val underlyingArrayTypeName = genTypeName.underlyingOf(arrayTypeRef)
    val underlyingArrayType = WasmRefType(underlyingArrayTypeName)

    val fromLocal = fctx.addSyntheticLocal(arrayClassType)
    val fromUnderlyingLocal = fctx.addSyntheticLocal(underlyingArrayType)
    val lengthLocal = fctx.addSyntheticLocal(WasmInt32)
    val resultUnderlyingLocal = fctx.addSyntheticLocal(underlyingArrayType)

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

    fctx.buildAndAddToContext(ctx.cloneFunctionTypeName)
  }

  def genNewDefault(clazz: LinkedClass)(implicit ctx: TypeDefinableWasmContext): Unit = {
    val className = clazz.name.name
    val classInfo = ctx.getClassInfo(className)
    assert(clazz.hasDirectInstances)

    val structName = genTypeName.forClass(className)
    implicit val fctx = WasmFunctionContext(
      genFunctionName.newDefault(className),
      Nil,
      List(WasmRefType(structName))
    )

    import fctx.instrs

    instrs += GLOBAL_GET(genGlobalName.forVTable(className))

    val interfaces = classInfo.ancestors.map(ctx.getClassInfo(_)).filter(_.isInterface)
    if (!interfaces.isEmpty)
      instrs += GLOBAL_GET(genGlobalName.forITable(className))
    else
      instrs += REF_NULL(WasmHeapType(genTypeName.itables))

    classInfo.allFieldDefs.foreach { f =>
      WasmExpressionBuilder.generateIRBody(IRTypes.zeroOf(f.ftpe)(clazz.pos), f.ftpe)
    }
    instrs += STRUCT_NEW(structName)

    fctx.buildAndAddToContext()
  }

}
