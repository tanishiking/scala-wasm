package wasm.wasm4s

import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import wasm.converters.WasmTextWriter

object Names {
  // private[wasm4s] because we don't wanna access it from converters
  sealed abstract class WasmName(private[wasm4s] val name: String) {
    def show: String =
      s"$$${WasmName.sanitizeWatIdentifier(this.name)}"
  }
  object WasmName {

    /** @see https://webassembly.github.io/spec/core/text/values.html#text-id */
    def sanitizeWatIdentifier(indent: String): String =
      if (indent.isEmpty) "_"
      else if (indent.forall(isValidWatIdentifierChar)) indent
      else indent.map(c => if (isValidWatIdentifierChar(c)) c else '_').mkString

    private def isValidWatIdentifierChar(c: Char): Boolean =
      c.isDigit || c.isLetter ||
        "!#$%&'*+-./:<=>?@\\^_`|~".contains(c) ||
        "$.@_".contains(c)
  }

  final case class WasmLocalName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmLocalName {
    def fromIR(name: IRNames.LocalName) = new WasmLocalName(name.nameString)
    def fromStr(str: String) = new WasmLocalName(str)
    def synthetic(id: Int) = new WasmLocalName(s"local___$id")

    val newTarget = new WasmLocalName("new.target")
    val receiver = new WasmLocalName("___<this>")
  }

  final case class WasmLabelName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmLabelName {
    def synthetic(id: Int): WasmLabelName = new WasmLabelName(id.toString())
  }

  final case class WasmGlobalName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmGlobalName {
    def forImportedModule(moduleName: String): WasmGlobalName =
      new WasmGlobalName(s"imported.$moduleName")

    def forModuleInstance(className: IRNames.ClassName): WasmGlobalName =
      new WasmGlobalName(s"modinstace.${className.nameString}")

    def forJSClassValue(className: IRNames.ClassName): WasmGlobalName =
      new WasmGlobalName(s"jsclass.${className.nameString}")

    def forVTable(className: IRNames.ClassName): WasmGlobalName =
      new WasmGlobalName(s"vtable.L${className.nameString}")

    def forVTable(typeRef: IRTypes.NonArrayTypeRef): WasmGlobalName = typeRef match {
      case typeRef: IRTypes.PrimRef    => new WasmGlobalName(s"vtable.${typeRef.charCode}")
      case IRTypes.ClassRef(className) => forVTable(className)
    }

    def forITable(className: IRNames.ClassName): WasmGlobalName =
      new WasmGlobalName(s"itable.L${className.nameString}")

    def forStaticField(fieldName: IRNames.FieldName): WasmGlobalName =
      new WasmGlobalName(s"static.${fieldName.nameString}")

    def forTopLevelExport(exportName: String): WasmGlobalName =
      new WasmGlobalName(s"export.$exportName")

    def forJSPrivateField(fieldName: IRNames.FieldName): WasmGlobalName =
      new WasmGlobalName(s"jspfield.${fieldName.nameString}")

    val stringLiteralCache: WasmGlobalName =
      new WasmGlobalName("string_literal")

    val arrayClassITable: WasmGlobalName =
      new WasmGlobalName("itable.A")

    val lastIDHashCode: WasmGlobalName =
      new WasmGlobalName("lastIDHashCode")

    val idHashCodeMap: WasmGlobalName =
      new WasmGlobalName("idHashCodeMap")
  }

  case class WasmFunctionName private (
      val namespace: String,
      val simpleName: String
  ) extends WasmName(namespace + "#" + simpleName)

  object WasmFunctionName {
    def apply(
        namespace: IRTrees.MemberNamespace,
        clazz: IRNames.ClassName,
        method: IRNames.MethodName
    ): WasmFunctionName = {
      new WasmFunctionName(
        namespaceString(namespace) + "#" + clazz.nameString,
        method.nameString
      )
    }

    private def namespaceString(namespace: IRTrees.MemberNamespace): String = {
      import IRTrees.MemberNamespace._

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

    def forExport(exportedName: String): WasmFunctionName =
      new WasmFunctionName("export", exportedName)

    def loadModule(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("loadModule", clazz.nameString)
    def newDefault(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("new", clazz.nameString)
    def instanceTest(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("instanceTest", clazz.nameString)
    def clone(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("clone", clazz.nameString)

    def clone(arrayBaseRef: IRTypes.NonArrayTypeRef): WasmFunctionName = {
      val simpleName = arrayBaseRef match {
        case IRTypes.ClassRef(_)  => "O"
        case IRTypes.PrimRef(tpe) => tpe.primRef.charCode.toString()
      }
      new WasmFunctionName("cloneArray", simpleName)
    }

    def isJSClassInstance(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("isJSClassInstance", clazz.nameString)
    def loadJSClass(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("loadJSClass", clazz.nameString)
    def createJSClassOf(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("createJSClassOf", clazz.nameString)
    def preSuperStats(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("preSuperStats", clazz.nameString)
    def superArgs(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("superArgs", clazz.nameString)
    def postSuperStats(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName("postSuperStats", clazz.nameString)

    val start = new WasmFunctionName("start", "start")

    private def helper(name: String): WasmFunctionName =
      new WasmFunctionName("__scalaJSHelpers", name)

    // JS helpers

    val is = helper("is")

    val undef = helper("undef")
    val isUndef = helper("isUndef")

    def box(primRef: IRTypes.PrimRef): WasmFunctionName = helper("b" + primRef.charCode)
    def unbox(primRef: IRTypes.PrimRef): WasmFunctionName = helper("u" + primRef.charCode)
    def unboxOrNull(primRef: IRTypes.PrimRef): WasmFunctionName = helper("uN" + primRef.charCode)
    def typeTest(primRef: IRTypes.PrimRef): WasmFunctionName = helper("t" + primRef.charCode)

    val fmod = helper("fmod")

    val closure = helper("closure")
    val closureThis = helper("closureThis")
    val closureRest = helper("closureRest")
    val closureThisRest = helper("closureThisRest")
    val closureRestNoData = helper("closureRestNoData")

    val emptyString = helper("emptyString")
    val stringLength = helper("stringLength")
    val stringCharAt = helper("stringCharAt")
    val jsValueToString = helper("jsValueToString") // for actual toString() call
    val jsValueToStringForConcat = helper("jsValueToStringForConcat")
    val booleanToString = helper("booleanToString")
    val charToString = helper("charToString")
    val intToString = helper("intToString")
    val longToString = helper("longToString")
    val doubleToString = helper("doubleToString")
    val stringConcat = helper("stringConcat")
    val isString = helper("isString")

    val jsValueType = helper("jsValueType")
    val bigintHashCode = helper("bigintHashCode")
    val symbolDescription = helper("symbolDescription")
    val idHashCodeGet = helper("idHashCodeGet")
    val idHashCodeSet = helper("idHashCodeSet")

    val jsGlobalRefGet = helper("jsGlobalRefGet")
    val jsGlobalRefSet = helper("jsGlobalRefSet")
    val jsGlobalRefTypeof = helper("jsGlobalRefTypeof")
    val jsNewArray = helper("jsNewArray")
    val jsArrayPush = helper("jsArrayPush")
    val jsArraySpreadPush = helper("jsArraySpreadPush")
    val jsNewObject = helper("jsNewObject")
    val jsObjectPush = helper("jsObjectPush")
    val jsSelect = helper("jsSelect")
    val jsSelectSet = helper("jsSelectSet")
    val jsNew = helper("jsNew")
    val jsFunctionApply = helper("jsFunctionApply")
    val jsMethodApply = helper("jsMethodApply")
    val jsImportCall = helper("jsImportCall")
    val jsImportMeta = helper("jsImportMeta")
    val jsDelete = helper("jsDelete")
    val jsForInSimple = helper("jsForInSimple")
    val jsIsTruthy = helper("jsIsTruthy")
    val jsLinkingInfo = helper("jsLinkingInfo")

    val jsUnaryOps: Map[IRTrees.JSUnaryOp.Code, WasmFunctionName] = {
      import IRTrees.JSUnaryOp
      Map(
        JSUnaryOp.+ -> helper("jsUnaryPlus"),
        JSUnaryOp.- -> helper("jsUnaryMinus"),
        JSUnaryOp.~ -> helper("jsUnaryTilde"),
        JSUnaryOp.! -> helper("jsUnaryBang"),
        JSUnaryOp.typeof -> helper("jsUnaryTypeof")
      )
    }

    val jsBinaryOps: Map[IRTrees.JSBinaryOp.Code, WasmFunctionName] = {
      import IRTrees.JSBinaryOp
      Map(
        JSBinaryOp.=== -> helper("jsStrictEquals"),
        JSBinaryOp.!== -> helper("jsNotStrictEquals"),
        JSBinaryOp.+ -> helper("jsPlus"),
        JSBinaryOp.- -> helper("jsMinus"),
        JSBinaryOp.* -> helper("jsTimes"),
        JSBinaryOp./ -> helper("jsDivide"),
        JSBinaryOp.% -> helper("jsModulus"),
        JSBinaryOp.| -> helper("jsBinaryOr"),
        JSBinaryOp.& -> helper("jsBinaryAnd"),
        JSBinaryOp.^ -> helper("jsBinaryXor"),
        JSBinaryOp.<< -> helper("jsShiftLeft"),
        JSBinaryOp.>> -> helper("jsArithmeticShiftRight"),
        JSBinaryOp.>>> -> helper("jsLogicalShiftRight"),
        JSBinaryOp.< -> helper("jsLessThan"),
        JSBinaryOp.<= -> helper("jsLessEqual"),
        JSBinaryOp.> -> helper("jsGreaterThan"),
        JSBinaryOp.>= -> helper("jsGreaterEqual"),
        JSBinaryOp.in -> helper("jsIn"),
        JSBinaryOp.instanceof -> helper("jsInstanceof"),
        JSBinaryOp.** -> helper("jsExponent")
      )
    }

    val newSymbol = helper("newSymbol")
    val createJSClass = helper("createJSClass")
    val createJSClassRest = helper("createJSClassRest")
    val installJSField = helper("installJSField")
    val installJSMethod = helper("installJSMethod")
    val installJSStaticMethod = helper("installJSStaticMethod")
    val installJSProperty = helper("installJSProperty")
    val installJSStaticProperty = helper("installJSStaticProperty")
    val jsSuperGet = helper("jsSuperGet")
    val jsSuperSet = helper("jsSuperSet")
    val jsSuperCall = helper("jsSuperCall")

    // Wasm internal helpers

    val createStringFromData = helper("createStringFromData")
    val stringLiteral = helper("stringLiteral")
    val typeDataName = helper("typeDataName")
    val createClassOf = helper("createClassOf")
    val getClassOf = helper("getClassOf")
    val arrayTypeData = helper("arrayTypeData")
    val isInstance = helper("isInstance")
    val isAssignableFromExternal = helper("isAssignableFromExternal")
    val isAssignableFrom = helper("isAssignableFrom")
    val checkCast = helper("checkCast")
    val getComponentType = helper("getComponentType")
    val newArrayOfThisClass = helper("newArrayOfThisClass")
    val anyGetClass = helper("anyGetClass")
    val newArrayObject = helper("newArrayObject")
    val identityHashCode = helper("identityHashCode")
    val searchReflectiveProxy = helper("searchReflectiveProxy")
  }

  final case class WasmFieldName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmFieldName {
    def forClassInstanceField(name: IRNames.FieldName): WasmFieldName =
      new WasmFieldName(name.nameString)

    /** For class itable fields, each fields point to an itable of the interfaces */
    def forITable(className: IRNames.ClassName): WasmFieldName =
      new WasmFieldName(className.nameString)

    def forMethodTableEntry(name: WasmFunctionName): WasmFieldName =
      new WasmFieldName(name.name)

    def captureParam(i: Int): WasmFieldName = new WasmFieldName("c" + i)

    val vtable = new WasmFieldName("vtable")
    val itable = new WasmFieldName("itable")
    val itables = new WasmFieldName("itables")
    val arrayItem = new WasmFieldName("array_item")
    val arrayField = new WasmFieldName("array_field")
    val reflectiveProxyField = new WasmFieldName("reflective_proxy_field")
    object reflectiveProxy {
      val func_name = new WasmFieldName("reflective_proxy_func_name")
      val func_ref = new WasmFieldName("reflective_proxy_func_ref")
    }

    // Fields of the typeData structs
    object typeData {

      /** The name data as `(ref null (array u16))` so that it can be initialized as a constant.
        *
        * It is non-null for primitives and for classes. It is null for array types, as array types
        * compute their `name` from the `name` of their component type.
        */
      val nameData = new WasmFieldName("nameData")

      /** The kind of type data, an `i32`.
        *
        * Possible values are the the `KindX` constants in `EmbeddedConstants`.
        */
      val kind = new WasmFieldName("kind")

      /** A bitset of special (primitive) instance types that are instances of this type, an `i32`.
        *
        * From 0 to 5, the bits correspond to the values returned by the helper `jsValueType`. In
        * addition, bits 6 and 7 represent `char` and `long`, respectively.
        */
      val specialInstanceTypes = new WasmFieldName("specialInstanceTypes")

      /** Array of the strict ancestor classes of this class.
        *
        * This is `null` for primitive and array types. For all other types, including JS types, it
        * contains an array of the typeData of their ancestors that:
        *
        *   - are not themselves (hence the *strict* ancestors),
        *   - have typeData to begin with.
        */
      val strictAncestors = new WasmFieldName("strictAncestors")

      /** The typeData of a component of this array type, or `null` if this is not an array type.
        *
        * For example:
        *   - the `componentType` for class `Foo` is `null`,
        *   - the `componentType` for the array type `Array[Foo]` is the `typeData` of `Foo`.
        */
      val componentType = new WasmFieldName("componentType")

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
      val name = new WasmFieldName("name")

      /** The `classOf` value, a nullable `java.lang.Class`, lazily initialized from this typeData.
        *
        * This field is initialized by the `createClassOf` helper.
        */
      val classOfValue = new WasmFieldName("classOf")

      /** The typeData/vtable of an array of this type, a nullable `typeData`, lazily initialized.
        *
        * This field is initialized by the `arrayTypeData` helper.
        *
        * For example, once initialized,
        *   - in the `typeData` of class `Foo`, it contains the `typeData` of `Array[Foo]`,
        *   - in the `typeData` of `Array[Int]`, it contains the `typeData` of `Array[Array[Int]]`.
        */
      val arrayOf = new WasmFieldName("arrayOf")

      /** The function to clone the object of this type, a nullable function reference. This field
        * is instantiated only with the classes that implement java.lang.Cloneable.
        */
      val cloneFunction = new WasmFieldName("clone")

      /** `isInstance` func ref for top-level JS classes. */
      val isJSClassInstance = new WasmFieldName("isJSClassInstance")

      /** The reflective proxies in this type, used for reflective call on the class at runtime.
        * This field contains an array of reflective proxy structs, where each struct contains the
        * ID of the reflective proxy and a reference to the actual method implementation. Reflective
        * call site should walk through the array to look up a method to call.
        *
        * See `genSearchReflectivePRoxy` in `HelperFunctions`
        */
      val reflectiveProxies = new WasmFieldName("reflectiveProxies")
    }
  }

  final case class WasmFieldIdx(value: Int)

  object WasmFieldIdx {
    val vtable = WasmFieldIdx(0)
    val itables = WasmFieldIdx(1)
    val uniqueRegularField = WasmFieldIdx(2)

    object typeData {
      val nameDataIdx = WasmFieldIdx(0)
      val kindIdx = WasmFieldIdx(1)
      val specialInstanceTypesIdx = WasmFieldIdx(2)
      val strictAncestorsIdx = WasmFieldIdx(3)
      val componentTypeIdx = WasmFieldIdx(4)
      val nameIdx = WasmFieldIdx(5)
      val classOfIdx = WasmFieldIdx(6)
      val arrayOfIdx = WasmFieldIdx(7)
      val cloneFunctionIdx = WasmFieldIdx(8)
      val isJSClassInstanceIdx = WasmFieldIdx(9)
      val reflectiveProxiesIdx = WasmFieldIdx(10)
    }

    object reflectiveProxy {
      val nameIdx = WasmFieldIdx(0)
      val funcIdx = WasmFieldIdx(1)
    }
  }

  // GC types ====
  sealed abstract class WasmTypeName(override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmTypeName {
    final case class WasmStructTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name)

    object WasmStructTypeName {
      def forClass(name: IRNames.ClassName): WasmStructTypeName =
        new WasmStructTypeName(s"c.L${name.nameString}")

      def captureData(index: Int): WasmStructTypeName =
        new WasmStructTypeName(s"captureData.$index")

      val typeData = new WasmStructTypeName("typeData")
      val reflectiveProxy = new WasmStructTypeName("reflective_proxy")

      // Array types -- they extend j.l.Object
      val BooleanArray = new WasmStructTypeName("c.AZ")
      val CharArray = new WasmStructTypeName("c.AC")
      val ByteArray = new WasmStructTypeName("c.AB")
      val ShortArray = new WasmStructTypeName("c.AS")
      val IntArray = new WasmStructTypeName("c.AI")
      val LongArray = new WasmStructTypeName("c.AJ")
      val FloatArray = new WasmStructTypeName("c.AF")
      val DoubleArray = new WasmStructTypeName("c.AD")
      val ObjectArray = new WasmStructTypeName("c.AO")

      def forArrayClass(arrayTypeRef: IRTypes.ArrayTypeRef): WasmStructTypeName = {
        if (arrayTypeRef.dimensions > 1) {
          ObjectArray
        } else {
          arrayTypeRef.base match {
            case IRTypes.BooleanRef => BooleanArray
            case IRTypes.CharRef    => CharArray
            case IRTypes.ByteRef    => ByteArray
            case IRTypes.ShortRef   => ShortArray
            case IRTypes.IntRef     => IntArray
            case IRTypes.LongRef    => LongArray
            case IRTypes.FloatRef   => FloatArray
            case IRTypes.DoubleRef  => DoubleArray
            case _                  => ObjectArray
          }
        }
      }

      def forVTable(className: IRNames.ClassName): WasmStructTypeName =
        new WasmStructTypeName(s"v.${className.nameString}")

      val ObjectVTable: WasmStructTypeName = forVTable(IRNames.ObjectClass)

      def forITable(className: IRNames.ClassName): WasmStructTypeName =
        new WasmStructTypeName(s"i.${className.nameString}")
    }

    /** Array type's name */
    final case class WasmArrayTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name)
    object WasmArrayTypeName {
      val typeDataArray = new WasmArrayTypeName("a.typeDataArray")
      val itables = new WasmArrayTypeName("a.itable")
      val reflectiveProxies = new WasmArrayTypeName("a.reflectiveProxies")

      // primitive array types, underlying the Array[T] classes
      val i8Array = new WasmArrayTypeName("a.i8Array")
      val i16Array = new WasmArrayTypeName("a.i16Array")
      val i32Array = new WasmArrayTypeName("a.i32Array")
      val i64Array = new WasmArrayTypeName("a.i64Array")
      val f32Array = new WasmArrayTypeName("a.f32Array")
      val f64Array = new WasmArrayTypeName("a.f64Array")
      val anyArray = new WasmArrayTypeName("a.anyArray")

      def underlyingOf(arrayTypeRef: IRTypes.ArrayTypeRef): WasmArrayTypeName = {
        if (arrayTypeRef.dimensions > 1) {
          anyArray
        } else {
          arrayTypeRef.base match {
            case IRTypes.BooleanRef => i8Array
            case IRTypes.CharRef    => i16Array
            case IRTypes.ByteRef    => i8Array
            case IRTypes.ShortRef   => i16Array
            case IRTypes.IntRef     => i32Array
            case IRTypes.LongRef    => i64Array
            case IRTypes.FloatRef   => f32Array
            case IRTypes.DoubleRef  => f64Array
            case _                  => anyArray
          }
        }
      }
    }

    final case class WasmFunctionTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name)
    object WasmFunctionTypeName {
      def apply(idx: Int) = new WasmFunctionTypeName(s"f.$idx")
    }

  }

  final case class WasmTagName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmTagName {
    def fromStr(str: String): WasmTagName = new WasmTagName(str)
  }

  final case class WasmDataName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmDataName {
    val string = WasmDataName("string")
  }

  final case class WasmExportName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmExportName {
    def fromStr(str: String) = new WasmExportName(str)
  }

}
