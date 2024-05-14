package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names.{FieldName => IRFieldName, _}
import org.scalajs.ir.Trees.{JSUnaryOp, JSBinaryOp, MemberNamespace}
import org.scalajs.ir.Types._

import org.scalajs.linker.backend.webassembly.Names._

/** Manages name generation for non-local, generated variable names. */
object VarGen {

  object genGlobalName {
    def forImportedModule(moduleName: String): GlobalName =
      GlobalName(s"imported.$moduleName")

    def forModuleInstance(className: ClassName): GlobalName =
      GlobalName(s"modinstace.${className.nameString}")

    def forJSClassValue(className: ClassName): GlobalName =
      GlobalName(s"jsclass.${className.nameString}")

    def forVTable(className: ClassName): GlobalName =
      GlobalName(s"vtable.L${className.nameString}")

    def forVTable(typeRef: NonArrayTypeRef): GlobalName = typeRef match {
      case typeRef: PrimRef    => GlobalName(s"vtable.${typeRef.charCode}")
      case ClassRef(className) => forVTable(className)
    }

    def forITable(className: ClassName): GlobalName =
      GlobalName(s"itable.L${className.nameString}")

    def forStaticField(fieldName: IRFieldName): GlobalName =
      GlobalName(s"static.${fieldName.nameString}")

    def forTopLevelExport(exportName: String): GlobalName =
      GlobalName(s"export.$exportName")

    def forJSPrivateField(fieldName: IRFieldName): GlobalName =
      GlobalName(s"jspfield.${fieldName.nameString}")

    val undef: GlobalName =
      GlobalName("undef")

    val bFalse: GlobalName =
      GlobalName("bFalse")

    val bZero: GlobalName =
      GlobalName("bZero")

    val bZeroChar: GlobalName =
      GlobalName("bZeroChar")

    val bZeroLong: GlobalName =
      GlobalName("bZeroLong")

    val emptyString: GlobalName =
      GlobalName("emptyString")

    val stringLiteralCache: GlobalName =
      GlobalName("string_literal")

    val arrayClassITable: GlobalName =
      GlobalName("itable.A")

    val lastIDHashCode: GlobalName =
      GlobalName("lastIDHashCode")

    val idHashCodeMap: GlobalName =
      GlobalName("idHashCodeMap")
  }

  object genFunctionName {
    private def make(str: String): FunctionName =
      FunctionName(str)

    private def make(namespace: String, simpleName: String): FunctionName =
      make(namespace + "#" + simpleName)

    def forMethod(
        namespace: MemberNamespace,
        clazz: ClassName,
        method: MethodName
    ): FunctionName =
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

    def forTableEntry(clazz: ClassName, method: MethodName): FunctionName =
      make("t#" + clazz.nameString, method.nameString)

    def forExport(exportedName: String): FunctionName =
      make("export", exportedName)

    def loadModule(clazz: ClassName): FunctionName =
      make("loadModule", clazz.nameString)
    def newDefault(clazz: ClassName): FunctionName =
      make("new", clazz.nameString)
    def instanceTest(clazz: ClassName): FunctionName =
      make("instanceTest", clazz.nameString)
    def clone(clazz: ClassName): FunctionName =
      make("clone", clazz.nameString)

    def clone(arrayBaseRef: NonArrayTypeRef): FunctionName = {
      val simpleName = arrayBaseRef match {
        case ClassRef(_)  => "O"
        case PrimRef(tpe) => tpe.primRef.charCode.toString()
      }
      make("cloneArray", simpleName)
    }

    def isJSClassInstance(clazz: ClassName): FunctionName =
      make("isJSClassInstance", clazz.nameString)
    def loadJSClass(clazz: ClassName): FunctionName =
      make("loadJSClass", clazz.nameString)
    def createJSClassOf(clazz: ClassName): FunctionName =
      make("createJSClassOf", clazz.nameString)
    def preSuperStats(clazz: ClassName): FunctionName =
      make("preSuperStats", clazz.nameString)
    def superArgs(clazz: ClassName): FunctionName =
      make("superArgs", clazz.nameString)
    def postSuperStats(clazz: ClassName): FunctionName =
      make("postSuperStats", clazz.nameString)

    val start = make("start")

    // JS helpers

    val is = make("is")

    val isUndef = make("isUndef")

    def box(primRef: PrimRef): FunctionName = make("b" + primRef.charCode)
    def unbox(primRef: PrimRef): FunctionName = make("u" + primRef.charCode)
    def unboxOrNull(primRef: PrimRef): FunctionName = make("uN" + primRef.charCode)
    def typeTest(primRef: PrimRef): FunctionName = make("t" + primRef.charCode)

    val fmod = make("fmod")

    val closure = make("closure")
    val closureThis = make("closureThis")
    val closureRest = make("closureRest")
    val closureThisRest = make("closureThisRest")
    val closureRestNoData = make("closureRestNoData")

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

    val jsUnaryOps: Map[JSUnaryOp.Code, FunctionName] = {
      Map(
        JSUnaryOp.+ -> make("jsUnaryPlus"),
        JSUnaryOp.- -> make("jsUnaryMinus"),
        JSUnaryOp.~ -> make("jsUnaryTilde"),
        JSUnaryOp.! -> make("jsUnaryBang"),
        JSUnaryOp.typeof -> make("jsUnaryTypeof")
      )
    }

    val jsBinaryOps: Map[JSBinaryOp.Code, FunctionName] = {
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
    def forClassInstanceField(name: IRFieldName): FieldName =
      FieldName(name.nameString)

    /** For class itable fields, each fields point to an itable of the interfaces */
    def forITable(className: ClassName): FieldName =
      FieldName(className.nameString)

    def forMethodTableEntry(name: MethodName): FieldName =
      FieldName("m." + name.nameString)

    def captureParam(i: Int): FieldName =
      FieldName("c" + i)

    object objStruct {
      val vtable = FieldName("vtable")
      val itables = FieldName("itables")
      val arrayUnderlying = FieldName("underlying")
    }

    object reflectiveProxy {
      val func_name = FieldName("reflective_proxy_func_name")
      val func_ref = FieldName("reflective_proxy_func_ref")
    }

    // Fields of the typeData structs
    object typeData {

      /** The name data as the 3 arguments to `stringLiteral`.
        *
        * It is only meaningful for primitives and for classes. For array types, they are all 0, as
        * array types compute their `name` from the `name` of their component type.
        */
      val nameOffset = FieldName("nameOffset")

      /** See `nameOffset`. */
      val nameSize = FieldName("nameSize")

      /** See `nameOffset`. */
      val nameStringIndex = FieldName("nameStringIndex")

      /** The kind of type data, an `i32`.
        *
        * Possible values are the the `KindX` constants in `EmbeddedConstants`.
        */
      val kind = FieldName("kind")

      /** A bitset of special (primitive) instance types that are instances of this type, an `i32`.
        *
        * From 0 to 5, the bits correspond to the values returned by the helper `jsValueType`. In
        * addition, bits 6 and 7 represent `char` and `long`, respectively.
        */
      val specialInstanceTypes = FieldName("specialInstanceTypes")

      /** Array of the strict ancestor classes of this class.
        *
        * This is `null` for primitive and array types. For all other types, including JS types, it
        * contains an array of the typeData of their ancestors that:
        *
        *   - are not themselves (hence the *strict* ancestors),
        *   - have typeData to begin with.
        */
      val strictAncestors = FieldName("strictAncestors")

      /** The typeData of a component of this array type, or `null` if this is not an array type.
        *
        * For example:
        *   - the `componentType` for class `Foo` is `null`,
        *   - the `componentType` for the array type `Array[Foo]` is the `typeData` of `Foo`.
        */
      val componentType = FieldName("componentType")

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
        *     `"[nested"`, for example `"⟦I"` for `Array[Array[Int]]` and `"⟦Ljava.lang.String;"`
        *     for `Array[Array[String]]`.¹
        *
        * ¹ We use the Unicode character `⟦` to represent two consecutive `[` characters in order
        * not to confuse Scaladoc.
        */
      val name = FieldName("name")

      /** The `classOf` value, a nullable `java.lang.Class`, lazily initialized from this typeData.
        *
        * This field is initialized by the `createClassOf` helper.
        */
      val classOfValue = FieldName("classOf")

      /** The typeData/vtable of an array of this type, a nullable `typeData`, lazily initialized.
        *
        * This field is initialized by the `arrayTypeData` helper.
        *
        * For example, once initialized,
        *   - in the `typeData` of class `Foo`, it contains the `typeData` of `Array[Foo]`,
        *   - in the `typeData` of `Array[Int]`, it contains the `typeData` of `Array[Array[Int]]`.
        */
      val arrayOf = FieldName("arrayOf")

      /** The function to clone the object of this type, a nullable function reference. This field
        * is instantiated only with the classes that implement java.lang.Cloneable.
        */
      val cloneFunction = FieldName("clone")

      /** `isInstance` func ref for top-level JS classes. */
      val isJSClassInstance = FieldName("isJSClassInstance")

      /** The reflective proxies in this type, used for reflective call on the class at runtime.
        * This field contains an array of reflective proxy structs, where each struct contains the
        * ID of the reflective proxy and a reference to the actual method implementation. Reflective
        * call site should walk through the array to look up a method to call.
        *
        * See `genSearchReflectivePRoxy` in `HelperFunctions`
        */
      val reflectiveProxies = FieldName("reflectiveProxies")
    }
  }

  object genFieldIdx {
    object objStruct {
      val vtable = FieldIdx(0)
      val itables = FieldIdx(1)
      val uniqueRegularField = FieldIdx(2)
    }

    object typeData {
      val nameOffsetIdx = FieldIdx(0)
      val nameSizeIdx = FieldIdx(1)
      val nameStringIndexIdx = FieldIdx(2)
      val kindIdx = FieldIdx(3)
      val specialInstanceTypesIdx = FieldIdx(4)
      val strictAncestorsIdx = FieldIdx(5)
      val componentTypeIdx = FieldIdx(6)
      val nameIdx = FieldIdx(7)
      val classOfIdx = FieldIdx(8)
      val arrayOfIdx = FieldIdx(9)
      val cloneFunctionIdx = FieldIdx(10)
      val isJSClassInstanceIdx = FieldIdx(11)
      val reflectiveProxiesIdx = FieldIdx(12)

      /** Index of a method in the actual vtable. */
      def vtableMethodIdx(methodIdx: Int): FieldIdx = FieldIdx(13 + methodIdx)
    }

    object reflectiveProxy {
      val nameIdx = FieldIdx(0)
      val funcIdx = FieldIdx(1)
    }
  }

  object genTypeName {
    def forClass(name: ClassName): TypeName =
      TypeName(s"c.L${name.nameString}")

    val ObjectStruct = forClass(ObjectClass)
    val ClassStruct = forClass(ClassClass)
    val ThrowableStruct = forClass(ThrowableClass)
    val JSExceptionStruct = forClass(SpecialNames.JSExceptionClass)

    def captureData(index: Int): TypeName =
      TypeName(s"captureData.$index")

    val typeData = TypeName("typeData")
    val reflectiveProxy = TypeName("reflective_proxy")

    // Array types -- they extend j.l.Object
    val BooleanArray = TypeName("c.AZ")
    val CharArray = TypeName("c.AC")
    val ByteArray = TypeName("c.AB")
    val ShortArray = TypeName("c.AS")
    val IntArray = TypeName("c.AI")
    val LongArray = TypeName("c.AJ")
    val FloatArray = TypeName("c.AF")
    val DoubleArray = TypeName("c.AD")
    val ObjectArray = TypeName("c.AO")

    def forArrayClass(arrayTypeRef: ArrayTypeRef): TypeName = {
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

    def forVTable(className: ClassName): TypeName =
      TypeName(s"v.${className.nameString}")

    val ObjectVTable: TypeName = forVTable(ObjectClass)

    def forITable(className: ClassName): TypeName =
      TypeName(s"i.${className.nameString}")

    val typeDataArray = TypeName("a.typeDataArray")
    val itables = TypeName("a.itable")
    val reflectiveProxies = TypeName("a.reflectiveProxies")

    // primitive array types, underlying the Array[T] classes
    val i8Array = TypeName("a.i8Array")
    val i16Array = TypeName("a.i16Array")
    val i32Array = TypeName("a.i32Array")
    val i64Array = TypeName("a.i64Array")
    val f32Array = TypeName("a.f32Array")
    val f64Array = TypeName("a.f64Array")
    val anyArray = TypeName("a.anyArray")

    def underlyingOf(arrayTypeRef: ArrayTypeRef): TypeName = {
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

    def forFunction(idx: Int): TypeName = TypeName(s"f.$idx")

    val cloneFunctionType = TypeName("cloneFuncType")
    val isJSClassInstanceFuncType = TypeName("isJSClassInstanceFuncType")

    def forTableFunctionType(methodName: MethodName): TypeName =
      TypeName("m." + methodName.nameString)
  }

  object genTagName {
    val exceptionTagName: TagName = TagName("exception")
  }

  object genDataName {
    val string = DataName("string")
  }

}
