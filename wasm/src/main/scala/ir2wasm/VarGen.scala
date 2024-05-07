package wasm.ir2wasm

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.{JSUnaryOp, JSBinaryOp, MemberNamespace}
import org.scalajs.ir.Types._

import wasm.wasm4s.Names._

/** Manages name generation for non-local, generated variable names. */
object VarGen {

  object genGlobalName {
    def forImportedModule(moduleName: String): WasmGlobalName =
      WasmGlobalName(s"imported.$moduleName")

    def forModuleInstance(className: ClassName): WasmGlobalName =
      WasmGlobalName(s"modinstace.${className.nameString}")

    def forJSClassValue(className: ClassName): WasmGlobalName =
      WasmGlobalName(s"jsclass.${className.nameString}")

    def forVTable(className: ClassName): WasmGlobalName =
      WasmGlobalName(s"vtable.L${className.nameString}")

    def forVTable(typeRef: NonArrayTypeRef): WasmGlobalName = typeRef match {
      case typeRef: PrimRef    => WasmGlobalName(s"vtable.${typeRef.charCode}")
      case ClassRef(className) => forVTable(className)
    }

    def forITable(className: ClassName): WasmGlobalName =
      WasmGlobalName(s"itable.L${className.nameString}")

    def forStaticField(fieldName: FieldName): WasmGlobalName =
      WasmGlobalName(s"static.${fieldName.nameString}")

    def forTopLevelExport(exportName: String): WasmGlobalName =
      WasmGlobalName(s"export.$exportName")

    def forJSPrivateField(fieldName: FieldName): WasmGlobalName =
      WasmGlobalName(s"jspfield.${fieldName.nameString}")

    val stringLiteralCache: WasmGlobalName =
      WasmGlobalName("string_literal")

    val arrayClassITable: WasmGlobalName =
      WasmGlobalName("itable.A")

    val lastIDHashCode: WasmGlobalName =
      WasmGlobalName("lastIDHashCode")

    val idHashCodeMap: WasmGlobalName =
      WasmGlobalName("idHashCodeMap")
  }

  object genFunctionName {
    private def make(str: String): WasmFunctionName =
      WasmFunctionName(str)

    private def make(namespace: String, simpleName: String): WasmFunctionName =
      make(namespace + "#" + simpleName)

    def forMethod(
        namespace: MemberNamespace,
        clazz: ClassName,
        method: MethodName
    ): WasmFunctionName =
      make(namespaceString(namespace) + "#" + clazz.nameString + "#" + method.nameString)

    private def namespaceString(namespace: MemberNamespace): String = {
      import MemberNamespace._

      // These strings are the same ones that the JS back-end uses
      namespace match {
        case Public            => "f"
        case Private           => "p"
        case PublicStatic      => "s"
        case PrivateStatic     => "ps"
        case Constructor       => "ct"
        case StaticConstructor => "sct"
      }
    }

    def forTableEntry(clazz: ClassName, method: MethodName): WasmFunctionName =
      make("t#" + clazz.nameString, method.nameString)

    def forExport(exportedName: String): WasmFunctionName =
      make("export", exportedName)

    def loadModule(clazz: ClassName): WasmFunctionName =
      make("loadModule", clazz.nameString)
    def newDefault(clazz: ClassName): WasmFunctionName =
      make("new", clazz.nameString)
    def instanceTest(clazz: ClassName): WasmFunctionName =
      make("instanceTest", clazz.nameString)
    def clone(clazz: ClassName): WasmFunctionName =
      make("clone", clazz.nameString)

    def clone(arrayBaseRef: NonArrayTypeRef): WasmFunctionName = {
      val simpleName = arrayBaseRef match {
        case ClassRef(_)  => "O"
        case PrimRef(tpe) => tpe.primRef.charCode.toString()
      }
      make("cloneArray", simpleName)
    }

    def isJSClassInstance(clazz: ClassName): WasmFunctionName =
      make("isJSClassInstance", clazz.nameString)
    def loadJSClass(clazz: ClassName): WasmFunctionName =
      make("loadJSClass", clazz.nameString)
    def createJSClassOf(clazz: ClassName): WasmFunctionName =
      make("createJSClassOf", clazz.nameString)
    def preSuperStats(clazz: ClassName): WasmFunctionName =
      make("preSuperStats", clazz.nameString)
    def superArgs(clazz: ClassName): WasmFunctionName =
      make("superArgs", clazz.nameString)
    def postSuperStats(clazz: ClassName): WasmFunctionName =
      make("postSuperStats", clazz.nameString)

    val start = make("start")

    // JS helpers

    val is = make("is")

    val undef = make("undef")
    val isUndef = make("isUndef")

    def box(primRef: PrimRef): WasmFunctionName = make("b" + primRef.charCode)
    def unbox(primRef: PrimRef): WasmFunctionName = make("u" + primRef.charCode)
    def unboxOrNull(primRef: PrimRef): WasmFunctionName = make("uN" + primRef.charCode)
    def typeTest(primRef: PrimRef): WasmFunctionName = make("t" + primRef.charCode)

    val fmod = make("fmod")

    val closure = make("closure")
    val closureThis = make("closureThis")
    val closureRest = make("closureRest")
    val closureThisRest = make("closureThisRest")
    val closureRestNoData = make("closureRestNoData")

    val emptyString = make("emptyString")
    val stringLength = make("stringLength")
    val stringCharAt = make("stringCharAt")
    val jsValueToString = make("jsValueToString") // for actual toString() call
    val jsValueToStringForConcat = make("jsValueToStringForConcat")
    val booleanToString = make("booleanToString")
    val charToString = make("charToString")
    val intToString = make("intToString")
    val longToString = make("longToString")
    val doubleToString = make("doubleToString")
    val stringConcat = make("stringConcat")
    val isString = make("isString")

    val jsValueType = make("jsValueType")
    val bigintHashCode = make("bigintHashCode")
    val symbolDescription = make("symbolDescription")
    val idHashCodeGet = make("idHashCodeGet")
    val idHashCodeSet = make("idHashCodeSet")

    val jsGlobalRefGet = make("jsGlobalRefGet")
    val jsGlobalRefSet = make("jsGlobalRefSet")
    val jsGlobalRefTypeof = make("jsGlobalRefTypeof")
    val jsNewArray = make("jsNewArray")
    val jsArrayPush = make("jsArrayPush")
    val jsArraySpreadPush = make("jsArraySpreadPush")
    val jsNewObject = make("jsNewObject")
    val jsObjectPush = make("jsObjectPush")
    val jsSelect = make("jsSelect")
    val jsSelectSet = make("jsSelectSet")
    val jsNew = make("jsNew")
    val jsFunctionApply = make("jsFunctionApply")
    val jsMethodApply = make("jsMethodApply")
    val jsImportCall = make("jsImportCall")
    val jsImportMeta = make("jsImportMeta")
    val jsDelete = make("jsDelete")
    val jsForInSimple = make("jsForInSimple")
    val jsIsTruthy = make("jsIsTruthy")
    val jsLinkingInfo = make("jsLinkingInfo")

    val jsUnaryOps: Map[JSUnaryOp.Code, WasmFunctionName] = {
      Map(
        JSUnaryOp.+ -> make("jsUnaryPlus"),
        JSUnaryOp.- -> make("jsUnaryMinus"),
        JSUnaryOp.~ -> make("jsUnaryTilde"),
        JSUnaryOp.! -> make("jsUnaryBang"),
        JSUnaryOp.typeof -> make("jsUnaryTypeof")
      )
    }

    val jsBinaryOps: Map[JSBinaryOp.Code, WasmFunctionName] = {
      Map(
        JSBinaryOp.=== -> make("jsStrictEquals"),
        JSBinaryOp.!== -> make("jsNotStrictEquals"),
        JSBinaryOp.+ -> make("jsPlus"),
        JSBinaryOp.- -> make("jsMinus"),
        JSBinaryOp.* -> make("jsTimes"),
        JSBinaryOp./ -> make("jsDivide"),
        JSBinaryOp.% -> make("jsModulus"),
        JSBinaryOp.| -> make("jsBinaryOr"),
        JSBinaryOp.& -> make("jsBinaryAnd"),
        JSBinaryOp.^ -> make("jsBinaryXor"),
        JSBinaryOp.<< -> make("jsShiftLeft"),
        JSBinaryOp.>> -> make("jsArithmeticShiftRight"),
        JSBinaryOp.>>> -> make("jsLogicalShiftRight"),
        JSBinaryOp.< -> make("jsLessThan"),
        JSBinaryOp.<= -> make("jsLessEqual"),
        JSBinaryOp.> -> make("jsGreaterThan"),
        JSBinaryOp.>= -> make("jsGreaterEqual"),
        JSBinaryOp.in -> make("jsIn"),
        JSBinaryOp.instanceof -> make("jsInstanceof"),
        JSBinaryOp.** -> make("jsExponent")
      )
    }

    val newSymbol = make("newSymbol")
    val createJSClass = make("createJSClass")
    val createJSClassRest = make("createJSClassRest")
    val installJSField = make("installJSField")
    val installJSMethod = make("installJSMethod")
    val installJSStaticMethod = make("installJSStaticMethod")
    val installJSProperty = make("installJSProperty")
    val installJSStaticProperty = make("installJSStaticProperty")
    val jsSuperGet = make("jsSuperGet")
    val jsSuperSet = make("jsSuperSet")
    val jsSuperCall = make("jsSuperCall")

    // Wasm internal makes

    val createStringFromData = make("createStringFromData")
    val stringLiteral = make("stringLiteral")
    val typeDataName = make("typeDataName")
    val createClassOf = make("createClassOf")
    val getClassOf = make("getClassOf")
    val arrayTypeData = make("arrayTypeData")
    val isInstance = make("isInstance")
    val isAssignableFromExternal = make("isAssignableFromExternal")
    val isAssignableFrom = make("isAssignableFrom")
    val checkCast = make("checkCast")
    val getComponentType = make("getComponentType")
    val newArrayOfThisClass = make("newArrayOfThisClass")
    val anyGetClass = make("anyGetClass")
    val newArrayObject = make("newArrayObject")
    val identityHashCode = make("identityHashCode")
    val searchReflectiveProxy = make("searchReflectiveProxy")
  }

  object genFieldName {
    def forClassInstanceField(name: FieldName): WasmFieldName =
      WasmFieldName(name.nameString)

    /** For class itable fields, each fields point to an itable of the interfaces */
    def forITable(className: ClassName): WasmFieldName =
      WasmFieldName(className.nameString)

    def forMethodTableEntry(name: MethodName): WasmFieldName =
      WasmFieldName("m." + name.nameString)

    def captureParam(i: Int): WasmFieldName =
      WasmFieldName("c" + i)

    object objStruct {
      val vtable = WasmFieldName("vtable")
      val itables = WasmFieldName("itables")
      val arrayUnderlying = WasmFieldName("underlying")
    }

    object reflectiveProxy {
      val func_name = WasmFieldName("reflective_proxy_func_name")
      val func_ref = WasmFieldName("reflective_proxy_func_ref")
    }

    // Fields of the typeData structs
    object typeData {

      /** The name data as the 3 arguments to `stringLiteral`.
        *
        * It is only meaningful for primitives and for classes. For array types, they are all 0, as
        * array types compute their `name` from the `name` of their component type.
        */
      val nameOffset = WasmFieldName("nameOffset")

      /** See `nameOffset`. */
      val nameSize = WasmFieldName("nameSize")

      /** See `nameOffset`. */
      val nameStringIndex = WasmFieldName("nameStringIndex")

      /** The kind of type data, an `i32`.
        *
        * Possible values are the the `KindX` constants in `EmbeddedConstants`.
        */
      val kind = WasmFieldName("kind")

      /** A bitset of special (primitive) instance types that are instances of this type, an `i32`.
        *
        * From 0 to 5, the bits correspond to the values returned by the helper `jsValueType`. In
        * addition, bits 6 and 7 represent `char` and `long`, respectively.
        */
      val specialInstanceTypes = WasmFieldName("specialInstanceTypes")

      /** Array of the strict ancestor classes of this class.
        *
        * This is `null` for primitive and array types. For all other types, including JS types, it
        * contains an array of the typeData of their ancestors that:
        *
        *   - are not themselves (hence the *strict* ancestors),
        *   - have typeData to begin with.
        */
      val strictAncestors = WasmFieldName("strictAncestors")

      /** The typeData of a component of this array type, or `null` if this is not an array type.
        *
        * For example:
        *   - the `componentType` for class `Foo` is `null`,
        *   - the `componentType` for the array type `Array[Foo]` is the `typeData` of `Foo`.
        */
      val componentType = WasmFieldName("componentType")

      /** The name as nullable string (`anyref`), lazily initialized from the nameData.
        *
        * This field is initialized by the `typeDataName` helper.
        *
        * The contents of this value is specified by `java.lang.Class.getName()`. In particular, for
        * array types, it obeys the following rules:
        *
        *   - `Array[prim]` where `prim` is a one of the primitive types with `charCode` `X` is
        *     `"[X"`, for example, `"[I"` for `Array[Int]`.
        *   - `Array[pack.Cls]` where `Cls` is a class is `"[Lpack.Cls;"`.
        *   - `Array[nestedArray]` where `nestedArray` is an array type with name `nested` is
        *     `"[nested"`, for example `"[[I"` for `Array[Array[Int]]` and `"[[Ljava.lang.String;"`
        *     for `Array[Array[String]]`.
        */
      val name = WasmFieldName("name")

      /** The `classOf` value, a nullable `java.lang.Class`, lazily initialized from this typeData.
        *
        * This field is initialized by the `createClassOf` helper.
        */
      val classOfValue = WasmFieldName("classOf")

      /** The typeData/vtable of an array of this type, a nullable `typeData`, lazily initialized.
        *
        * This field is initialized by the `arrayTypeData` helper.
        *
        * For example, once initialized,
        *   - in the `typeData` of class `Foo`, it contains the `typeData` of `Array[Foo]`,
        *   - in the `typeData` of `Array[Int]`, it contains the `typeData` of `Array[Array[Int]]`.
        */
      val arrayOf = WasmFieldName("arrayOf")

      /** The function to clone the object of this type, a nullable function reference. This field
        * is instantiated only with the classes that implement java.lang.Cloneable.
        */
      val cloneFunction = WasmFieldName("clone")

      /** `isInstance` func ref for top-level JS classes. */
      val isJSClassInstance = WasmFieldName("isJSClassInstance")

      /** The reflective proxies in this type, used for reflective call on the class at runtime.
        * This field contains an array of reflective proxy structs, where each struct contains the
        * ID of the reflective proxy and a reference to the actual method implementation. Reflective
        * call site should walk through the array to look up a method to call.
        *
        * See `genSearchReflectivePRoxy` in `HelperFunctions`
        */
      val reflectiveProxies = WasmFieldName("reflectiveProxies")
    }
  }

  object genFieldIdx {
    object objStruct {
      val vtable = WasmFieldIdx(0)
      val itables = WasmFieldIdx(1)
      val uniqueRegularField = WasmFieldIdx(2)
    }

    object typeData {
      val nameOffsetIdx = WasmFieldIdx(0)
      val nameSizeIdx = WasmFieldIdx(1)
      val nameStringIndexIdx = WasmFieldIdx(2)
      val kindIdx = WasmFieldIdx(3)
      val specialInstanceTypesIdx = WasmFieldIdx(4)
      val strictAncestorsIdx = WasmFieldIdx(5)
      val componentTypeIdx = WasmFieldIdx(6)
      val nameIdx = WasmFieldIdx(7)
      val classOfIdx = WasmFieldIdx(8)
      val arrayOfIdx = WasmFieldIdx(9)
      val cloneFunctionIdx = WasmFieldIdx(10)
      val isJSClassInstanceIdx = WasmFieldIdx(11)
      val reflectiveProxiesIdx = WasmFieldIdx(12)

      /** Index of a method in the actual vtable. */
      def vtableMethodIdx(methodIdx: Int): WasmFieldIdx = WasmFieldIdx(13 + methodIdx)
    }

    object reflectiveProxy {
      val nameIdx = WasmFieldIdx(0)
      val funcIdx = WasmFieldIdx(1)
    }
  }

  object genTypeName {
    def forClass(name: ClassName): WasmTypeName =
      WasmTypeName(s"c.L${name.nameString}")

    def captureData(index: Int): WasmTypeName =
      WasmTypeName(s"captureData.$index")

    val typeData = WasmTypeName("typeData")
    val reflectiveProxy = WasmTypeName("reflective_proxy")

    // Array types -- they extend j.l.Object
    val BooleanArray = WasmTypeName("c.AZ")
    val CharArray = WasmTypeName("c.AC")
    val ByteArray = WasmTypeName("c.AB")
    val ShortArray = WasmTypeName("c.AS")
    val IntArray = WasmTypeName("c.AI")
    val LongArray = WasmTypeName("c.AJ")
    val FloatArray = WasmTypeName("c.AF")
    val DoubleArray = WasmTypeName("c.AD")
    val ObjectArray = WasmTypeName("c.AO")

    def forArrayClass(arrayTypeRef: ArrayTypeRef): WasmTypeName = {
      if (arrayTypeRef.dimensions > 1) {
        ObjectArray
      } else {
        arrayTypeRef.base match {
          case BooleanRef => BooleanArray
          case CharRef    => CharArray
          case ByteRef    => ByteArray
          case ShortRef   => ShortArray
          case IntRef     => IntArray
          case LongRef    => LongArray
          case FloatRef   => FloatArray
          case DoubleRef  => DoubleArray
          case _          => ObjectArray
        }
      }
    }

    def forVTable(className: ClassName): WasmTypeName =
      WasmTypeName(s"v.${className.nameString}")

    val ObjectVTable: WasmTypeName = forVTable(ObjectClass)

    def forITable(className: ClassName): WasmTypeName =
      WasmTypeName(s"i.${className.nameString}")

    val typeDataArray = WasmTypeName("a.typeDataArray")
    val itables = WasmTypeName("a.itable")
    val reflectiveProxies = WasmTypeName("a.reflectiveProxies")

    // primitive array types, underlying the Array[T] classes
    val i8Array = WasmTypeName("a.i8Array")
    val i16Array = WasmTypeName("a.i16Array")
    val i32Array = WasmTypeName("a.i32Array")
    val i64Array = WasmTypeName("a.i64Array")
    val f32Array = WasmTypeName("a.f32Array")
    val f64Array = WasmTypeName("a.f64Array")
    val anyArray = WasmTypeName("a.anyArray")

    def underlyingOf(arrayTypeRef: ArrayTypeRef): WasmTypeName = {
      if (arrayTypeRef.dimensions > 1) {
        anyArray
      } else {
        arrayTypeRef.base match {
          case BooleanRef => i8Array
          case CharRef    => i16Array
          case ByteRef    => i8Array
          case ShortRef   => i16Array
          case IntRef     => i32Array
          case LongRef    => i64Array
          case FloatRef   => f32Array
          case DoubleRef  => f64Array
          case _          => anyArray
        }
      }
    }

    def forFunction(idx: Int): WasmTypeName = WasmTypeName(s"f.$idx")

    def forRecFunction(idx: Int): WasmTypeName = WasmTypeName(s"recf.$idx")
  }

  object genTagName {
    val exceptionTagName: WasmTagName = WasmTagName("exception")
  }

  object genDataName {
    val string = WasmDataName("string")
  }

}
