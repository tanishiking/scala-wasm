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

import TypeTransformer._
import wasm4s.Defaults

object HelperFunctions {

  def genGlobalHelpers()(implicit ctx: WasmContext): Unit = {
    genCreateStringFromData()
    genTypeDataName()
    genCreateClassOf()
    genGetClassOf()
    genArrayTypeData()
    genGetComponentType()
    genAnyGetClass()
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
    val resultLocal = fctx.addLocal("restul", WasmRefType.any)

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

      // if typeData.kind == 2 (isArray)
      instrs += LOCAL_GET(typeDataParam)
      instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.kindIdx)
      instrs += I32_CONST(I32(2))
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
        instrs += LOCAL_TEE(componentTypeDataLocal)

        // if componentTypeData.kind == 1 (primitive)
        instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.kindIdx)
        instrs += I32_CONST(I32(1))
        instrs += I32_EQ
        fctx.ifThenElse(WasmRefType.any) { // returns the string that must come after "["
          // the component type is a primitive; compute its charCode from its name's first and second chars

          // componentNameData := componentTypeData.nameData
          instrs += LOCAL_GET(componentTypeDataLocal)
          instrs += STRUCT_GET(
            TypeIdx(WasmStructTypeName.typeData),
            WasmFieldName.typeData.nameDataIdx
          )
          instrs += REF_AS_NOT_NULL
          instrs += LOCAL_TEE(componentNameDataLocal)

          // firstChar := componentNameData(0)
          instrs += I32_CONST(I32(0))
          instrs += ARRAY_GET_U(u16ArrayIdx)
          instrs += LOCAL_TEE(firstCharLocal)

          // if firstChar == 'b'
          instrs += I32_CONST(I32('b'.toInt))
          instrs += I32_EQ
          fctx.ifThenElse(WasmInt32) {
            // 'b' can be 'byte' or 'boolean'; check second char

            // if componentNameData(1) == 'o'
            instrs += LOCAL_GET(componentNameDataLocal)
            instrs += I32_CONST(I32(1)) // second char
            instrs += ARRAY_GET_U(u16ArrayIdx)
            instrs += I32_CONST(I32('o'.toInt))
            instrs += I32_EQ
            fctx.ifThenElse(WasmInt32) {
              // if 'o', it's 'boolean'
              instrs += I32_CONST(I32('Z'.toInt))
            } {
              // otherwise, it is 'byte'
              instrs += I32_CONST(I32('B'.toInt))
            }
          } {
            // if firstChar == 'l'
            instrs += LOCAL_GET(firstCharLocal)
            instrs += I32_CONST(I32('l'.toInt))
            instrs += I32_EQ
            fctx.ifThenElse(WasmInt32) {
              // 'l' is 'long', which must become 'J'
              instrs += I32_CONST(I32('J'.toInt))
            } {
              // Other letters are turned into their uppercase, which is 32 less
              instrs += LOCAL_GET(firstCharLocal)
              instrs += I32_CONST(I32(32))
              instrs += I32_SUB
            }
          }

          // convert the charCode to string
          instrs += CALL(FuncIdx(WasmFunctionName.charToString))
        } {
          // the component type is not a primitive

          // if componentTypeData.kind == 2 (array)
          instrs += LOCAL_GET(componentTypeDataLocal)
          instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.kindIdx)
          instrs += I32_CONST(I32(2))
          instrs += I32_EQ
          fctx.ifThenElse(WasmRefType.any) {
            // the component type is an array; get its own name
            instrs += LOCAL_GET(componentTypeDataLocal)
            instrs += CALL(FuncIdx(WasmFunctionName.typeDataName))
          } {
            // the component type is neither a primitive nor an array;
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
    // "name": typeDataName(typeData)
    instrs += ctx.getConstantStringInstr("name")
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(FuncIdx(WasmFunctionName.typeDataName))
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    // "isPrimitive": (typeData.kind == 1)
    instrs += ctx.getConstantStringInstr("isPrimitive")
    instrs += LOCAL_GET(typeDataParam)
    instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.kindIdx)
    instrs += I32_CONST(I32(1))
    instrs += I32_EQ
    instrs += CALL(FuncIdx(WasmFunctionName.box(IRTypes.BooleanRef)))
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    // "isArrayClass": (typeData.kind == 2)
    instrs += ctx.getConstantStringInstr("isArrayClass")
    instrs += LOCAL_GET(typeDataParam)
    instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.kindIdx)
    instrs += I32_CONST(I32(2))
    instrs += I32_EQ
    instrs += CALL(FuncIdx(WasmFunctionName.box(IRTypes.BooleanRef)))
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    // "isInterface": (typeData.kind == 3)
    instrs += ctx.getConstantStringInstr("isInterface")
    instrs += LOCAL_GET(typeDataParam)
    instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.kindIdx)
    instrs += I32_CONST(I32(3))
    instrs += I32_EQ
    instrs += CALL(FuncIdx(WasmFunctionName.box(IRTypes.BooleanRef)))
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    // "getComponentType": closure(getComponentType, typeData)
    instrs += ctx.getConstantStringInstr("getComponentType")
    instrs += ctx.refFuncWithDeclaration(WasmFunctionName.getComponentType)
    instrs += LOCAL_GET(typeDataParam)
    instrs += CALL(FuncIdx(WasmFunctionName.closure))
    instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    // TODO: "isInstance", "isAssignableFrom", "checkCast", "newArrayOfThisClass"

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
    import WasmTypeName.WasmStructTypeName

    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))
    val objectVTableType = WasmRefType(
      WasmHeapType.Type(WasmTypeName.WasmVTableTypeName.ObjectVTable)
    )

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
        instrs += I32_CONST(I32(2)) // kind = isArray
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
            // case typeFalse, typeTrue =>
            List(0, 1) -> { () =>
              instrs += getHijackedClassTypeDataInstr(IRNames.BoxedBooleanClass)
            },
            // case typeString =>
            List(2) -> { () =>
              instrs += getHijackedClassTypeDataInstr(IRNames.BoxedStringClass)
            },
            // case typeNumber =>
            List(3) -> { () =>
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
            // case typeUndefined =>
            List(4) -> { () =>
              instrs += getHijackedClassTypeDataInstr(IRNames.BoxedUnitClass)
            }
          ) { () =>
            // case _ (typeOther) =>
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
