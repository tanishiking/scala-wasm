package wasm.ir2wasm

import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Names => IRNames}

import wasm.wasm4s._
import wasm.wasm4s.WasmContext._
import wasm.wasm4s.Names._
import wasm.wasm4s.Types._
import wasm.wasm4s.WasmInstr._

import TypeTransformer._

object HelperFunctions {

  def genGlobalHelpers()(implicit ctx: WasmContext): Unit = {
    genCreateStringFromData()
    genTypeDataName()
    genCreateClassOf()
    genArrayTypeData()
    genGetComponentType()
  }

  /** `createStringFromData: (ref array u16) -> (ref any)` (representing a `string`). */
  private def genCreateStringFromData()(implicit ctx: WasmContext): Unit = {
    import WasmImmediate._
    import WasmTypeName.WasmArrayTypeName

    val dataType = WasmRefType(WasmHeapType.Type(WasmArrayTypeName.u16Array))

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
      instrs += ARRAY_GET_U(TypeIdx(WasmArrayTypeName.u16Array))
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
   *  Initializes the `name` field of the given `typeData` if that was not done
   *  yet, and returns its value.
   *
   *  The computed value is specified by `java.lang.Class.getName()`. See also
   *  the documentation on [[Names.WasmFieldName.typeData.name]] for details.
   *
   *  @see [[https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Class.html#getName()]]
   */
  private def genTypeDataName()(implicit ctx: WasmContext): Unit = {
    import WasmImmediate._
    import WasmTypeName._

    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))
    val nameDataType = WasmRefType(WasmHeapType.Type(WasmArrayTypeName.u16Array))

    val u16ArrayIdx = TypeIdx(WasmArrayTypeName.u16Array)

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
        instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.componentTypeIdx)
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
          instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.nameDataIdx)
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
        instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.nameDataIdx)
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
   *  Creates the unique `java.lang.Class` instance associated with the given
   *  `typeData`, stores it in its `classOfValue` field, and returns it.
   *
   *  Must be called only if the `classOfValue` of the typeData is null. All
   *  call sites must deal with the non-null case as a fast-path.
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
    instrs += CALL(FuncIdx(WasmFunctionName(IRNames.ClassClass, SpecialNames.ClassCtor)))

    // typeData.classOf := classInstance
    instrs += LOCAL_GET(typeDataParam)
    instrs += LOCAL_GET(classInstanceLocal)
    instrs += STRUCT_SET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.classOfIdx)

    // <top-of-stack> := classInstance for the implicit return
    instrs += LOCAL_GET(classInstanceLocal)

    fctx.buildAndAddToContext()
  }

  /** `arrayTypeData: (ref typeData), i32 -> (ref typeData)`.
   *
   *  Returns the typeData of an array with `dims` dimensions over the given
   *  typeData.
   */
  private def genArrayTypeData()(implicit ctx: WasmContext): Unit = {
    import WasmImmediate._
    import WasmTypeName.WasmStructTypeName

    val typeDataType = WasmRefType(WasmHeapType.Type(WasmStructType.typeData.name))

    val fctx = WasmFunctionContext(
      WasmFunctionName.arrayTypeData,
      List("typeData" -> typeDataType, "dims" -> WasmInt32),
      List(typeDataType)
    )

    val List(typeDataParam, dimsParam) = fctx.paramIndices

    import fctx.instrs

    fctx.loop() { loopLabel =>
      // if dims == 0 then
      //   return typeData
      instrs += LOCAL_GET(dimsParam)
      instrs += I32_EQZ
      fctx.ifThen() {
        instrs += LOCAL_GET(typeDataParam)
        instrs += RETURN
      }

      // dims := dims - 1
      instrs += LOCAL_GET(dimsParam)
      instrs += I32_CONST(I32(1))
      instrs += I32_SUB
      instrs += LOCAL_SET(dimsParam)

      fctx.block(typeDataType) { arrayOfIsNonNullLabel =>
        // br_on_non_null $arrayOfIsNonNull typeData.arrayOf
        instrs += LOCAL_GET(typeDataParam)
        instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.arrayOfIdx)
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
        instrs += STRUCT_NEW(TypeIdx(WasmStructTypeName.typeData))
        instrs += LOCAL_TEE(typeDataParam)

        // <old typeData>.arrayOf := typeData
        instrs += STRUCT_SET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.arrayOfIdx)

        // loop back to the beginning
        instrs += BR(loopLabel)
      } // end block $arrayOfIsNonNullLabel

      // typeData := typeData.arrayOf (which is on the stack), then loop back to the beginning
      instrs += LOCAL_SET(typeDataParam)
      instrs += BR(loopLabel)
    } // end loop $loop
    instrs += UNREACHABLE

    fctx.buildAndAddToContext()
  }

  /** `getComponentType: (ref typeData) -> (ref null jlClass)`.
   *
   *  This is the underlying func for the `getComponentType()` closure inside
   *  class data objects.
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
      fctx.block(Types.WasmRefType(Types.WasmHeapType.ClassType)) { nonNullClassOfLabel =>
        // Try and extract non-null component type data
        instrs += LOCAL_GET(typeDataParam)
        instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.componentTypeIdx)
        instrs += BR_ON_NULL(nullResultLabel)
        // fast path
        instrs += LOCAL_TEE(componentTypeDataLocal)
        instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.classOfIdx)
        instrs += BR_ON_NON_NULL(nonNullClassOfLabel)
        // slow path
        instrs += LOCAL_GET(componentTypeDataLocal)
        instrs += CALL(FuncIdx(WasmFunctionName.createClassOf))
      } // end bock nonNullClassOfLabel
      instrs += RETURN
    } // end block nullResultLabel
    instrs += REF_NULL(HeapType(WasmHeapType.ClassType))

    fctx.buildAndAddToContext()
  }

}
