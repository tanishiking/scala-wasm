package wasm.wasm4s

import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import wasm.converters.WasmTextWriter

object Names {
  // private[wasm4s] because we don't wanna access it from converters
  sealed abstract class WasmName(private[wasm4s] val name: String) {
    def show: String = {
      val suffix = this match {
        case _: WasmLocalName                               => "local"
        case _: WasmGlobalName.WasmImportedModuleName       => "g_imported"
        case _: WasmGlobalName.WasmModuleInstanceName       => "g_instance"
        case _: WasmGlobalName.WasmJSClassName              => "g_jsclass"
        case _: WasmGlobalName.WasmGlobalVTableName         => "g_vtable"
        case _: WasmGlobalName.WasmGlobalITableName         => "g_itable"
        case _: WasmGlobalName.WasmGlobalConstantStringName => "str_const"
        case _: WasmGlobalName.WasmGlobalStaticFieldName    => "f_static"
        case _: WasmGlobalName.WasmGlobalJSPrivateFieldName => "g_jspfield"
        case _: WasmFunctionName                            => "fun"
        case _: WasmFieldName                               => "field"
        case _: WasmTagName                                 => "tag"
        case _: WasmDataName                                => "data"
        case _: WasmExportName                              => "export"
        case _: WasmTypeName.WasmFunctionTypeName           => "ty"
        case _: WasmTypeName.WasmStructTypeName             => "struct"
        case _: WasmTypeName.WasmArrayTypeName              => "arr"
        case _: WasmTypeName.WasmVTableTypeName             => "vtable"
        case _: WasmTypeName.WasmITableTypeName             => "itable"
      }
      s"$$${WasmName.sanitizeWatIdentifier(this.name)}___$suffix"
    }
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

  sealed abstract class WasmGlobalName(override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmGlobalName {
    final case class WasmImportedModuleName private (
        override private[wasm4s] val name: String
    ) extends WasmGlobalName(name)
    object WasmImportedModuleName {
      def apply(moduleName: String): WasmImportedModuleName =
        new WasmImportedModuleName(s"imported___$moduleName")
    }

    final case class WasmModuleInstanceName private (override private[wasm4s] val name: String)
        extends WasmGlobalName(name)
    object WasmModuleInstanceName {
      def fromIR(name: IRNames.ClassName): WasmModuleInstanceName = new WasmModuleInstanceName(
        name.nameString
      )
    }

    final case class WasmJSClassName private (override private[wasm4s] val name: String)
        extends WasmGlobalName(name)
    object WasmJSClassName {
      def apply(name: IRNames.ClassName): WasmJSClassName =
        new WasmJSClassName(name.nameString)
    }

    final case class WasmGlobalVTableName private (override private[wasm4s] val name: String)
        extends WasmGlobalName(name)
    object WasmGlobalVTableName {
      def apply(name: IRNames.ClassName): WasmGlobalVTableName =
        new WasmGlobalVTableName("L" + name.nameString)

      def apply(typeRef: IRTypes.NonArrayTypeRef): WasmGlobalVTableName = typeRef match {
        case typeRef: IRTypes.PrimRef    => new WasmGlobalVTableName(typeRef.charCode.toString())
        case IRTypes.ClassRef(className) => apply(className)
      }

      def apply(name: WasmTypeName): WasmGlobalVTableName = new WasmGlobalVTableName(
        name.name
      )
    }

    final case class WasmGlobalITableName private (override private[wasm4s] val name: String)
        extends WasmGlobalName(name)
    object WasmGlobalITableName {
      def apply(name: IRNames.ClassName): WasmGlobalITableName = new WasmGlobalITableName(
        name.nameString
      )
    }

    final case class WasmGlobalConstantStringName private (
        override private[wasm4s] val name: String
    ) extends WasmGlobalName(name)
    object WasmGlobalConstantStringName {
      def apply(index: Int): WasmGlobalConstantStringName =
        new WasmGlobalConstantStringName(s"conststring___$index")
    }

    final case class WasmGlobalStaticFieldName private (
        override private[wasm4s] val name: String
    ) extends WasmGlobalName(name)
    object WasmGlobalStaticFieldName {
      def apply(fieldName: IRNames.FieldName): WasmGlobalStaticFieldName =
        new WasmGlobalStaticFieldName(s"static___${fieldName.nameString}")
    }

    final case class WasmGlobalJSPrivateFieldName private (
        override private[wasm4s] val name: String
    ) extends WasmGlobalName(name)
    object WasmGlobalJSPrivateFieldName {
      def apply(fieldName: IRNames.FieldName): WasmGlobalJSPrivateFieldName =
        new WasmGlobalJSPrivateFieldName(s"jspfield___${fieldName.nameString}")
    }
  }

  // final case class WasmGlobalName private (val name: String) extends WasmName(name) {
  //     def apply(n: IRNames.LocalName): WasmLocalName = new WasmLocalName(n.nameString)
  // }

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

    val emptyString = helper("emptyString")
    val stringLength = helper("stringLength")
    val stringCharAt = helper("stringCharAt")
    val jsValueToString = helper("jsValueToString")
    val booleanToString = helper("booleanToString")
    val charToString = helper("charToString")
    val intToString = helper("intToString")
    val longToString = helper("longToString")
    val doubleToString = helper("doubleToString")
    val stringConcat = helper("stringConcat")
    val isString = helper("isString")

    val jsValueType = helper("jsValueType")
    val jsValueHashCode = helper("jsValueHashCode")

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
    val installJSField = helper("installJSField")
    val installJSMethod = helper("installJSMethod")
    val installJSProperty = helper("installJSProperty")
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
  }

  final case class WasmFieldName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmFieldName {
    def apply(name: IRNames.FieldName) = new WasmFieldName(name.nameString)

    /** For class itable fields, each fields point to an itable of the interfaces */
    def apply(name: WasmTypeName.WasmITableTypeName) = new WasmFieldName(name.name)
    def apply(name: IRNames.MethodName) = new WasmFieldName(name.nameString)
    def apply(name: WasmFunctionName) = new WasmFieldName(name.name)

    def captureParam(i: Int): WasmFieldName = new WasmFieldName("c" + i)

    val vtable = new WasmFieldName("vtable")
    val itable = new WasmFieldName("itable")
    val itables = new WasmFieldName("itables")
    val arrayItem = new WasmFieldName("array_item")
    val arrayField = new WasmFieldName("array_field")

    // Fields of the typeData structs
    object typeData {
      // Values for `kind`
      final val KindVoid = 0
      final val KindBoolean = 1
      final val KindChar = 2
      final val KindByte = 3
      final val KindShort = 4
      final val KindInt = 5
      final val KindLong = 6
      final val KindFloat = 7
      final val KindDouble = 8
      final val KindArray = 9
      final val KindObject = 10 // j.l.Object
      final val KindBoxedUnit = 11
      final val KindBoxedBoolean = 12
      final val KindBoxedCharacter = 13
      final val KindBoxedByte = 14
      final val KindBoxedShort = 15
      final val KindBoxedInteger = 16
      final val KindBoxedLong = 17
      final val KindBoxedFloat = 18
      final val KindBoxedDouble = 19
      final val KindBoxedString = 20
      final val KindClass = 21
      final val KindInterface = 22
      final val KindJSType = 23

      final val KindLastPrimitive = KindDouble

      /** The name data as `(ref null (array u16))` so that it can be initialized as a constant.
        *
        * It is non-null for primitives and for classes. It is null for array types, as array types
        * compute their `name` from the `name` of their component type.
        */
      val nameData = new WasmFieldName("nameData")

      /** The kind of type data, an `i32`. */
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

      val nameDataIdx = WasmImmediate.StructFieldIdx(0)
      val kindIdx = WasmImmediate.StructFieldIdx(1)
      val specialInstanceTypesIdx = WasmImmediate.StructFieldIdx(2)
      val strictAncestorsIdx = WasmImmediate.StructFieldIdx(3)
      val componentTypeIdx = WasmImmediate.StructFieldIdx(4)
      val nameIdx = WasmImmediate.StructFieldIdx(5)
      val classOfIdx = WasmImmediate.StructFieldIdx(6)
      val arrayOfIdx = WasmImmediate.StructFieldIdx(7)
      val cloneFunctionIdx = WasmImmediate.StructFieldIdx(8)
    }
  }

  // GC types ====
  sealed abstract class WasmTypeName(override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmTypeName {
    final case class WasmStructTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name) {
      def toIRName = IRNames.ClassName(name)
    }
    object WasmStructTypeName {
      def apply(name: IRNames.ClassName) = new WasmStructTypeName(name.nameString)

      def captureData(index: Int): WasmStructTypeName = new WasmStructTypeName(
        "captureData__" + index
      )

      val typeData = new WasmStructTypeName("typeData")

      // Array types -- they extend j.l.Object
      val BooleanArray = new WasmStructTypeName("Array_Boolean")
      val CharArray = new WasmStructTypeName("Array_Char")
      val ByteArray = new WasmStructTypeName("Array_Byte")
      val ShortArray = new WasmStructTypeName("Array_Short")
      val IntArray = new WasmStructTypeName("Array_Int")
      val LongArray = new WasmStructTypeName("Array_Long")
      val FloatArray = new WasmStructTypeName("Array_Float")
      val DoubleArray = new WasmStructTypeName("Array_Double")
      val ObjectArray = new WasmStructTypeName("Array_Object")

      def apply(arrayTypeRef: IRTypes.ArrayTypeRef): WasmStructTypeName = {
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
    }

    /** Array type's name */
    final case class WasmArrayTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name)
    object WasmArrayTypeName {
      val typeDataArray = new WasmArrayTypeName("typeDataArray")
      val itables = new WasmArrayTypeName("itable")

      // primitive array types, underlying the Array[T] classes
      val i8Array = new WasmArrayTypeName("i8Array")
      val i16Array = new WasmArrayTypeName("i16Array")
      val i32Array = new WasmArrayTypeName("i32Array")
      val i64Array = new WasmArrayTypeName("i64Array")
      val f32Array = new WasmArrayTypeName("f32Array")
      val f64Array = new WasmArrayTypeName("f64Array")
      val anyArray = new WasmArrayTypeName("anyArray")

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
      def apply(idx: Int) = new WasmFunctionTypeName(s"fun_type___$idx")
      // val cloneFunction = new WasmFunctionTypeName("clone_function_type")
    }

    /** Vtable type's name */
    final case class WasmVTableTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name)
    object WasmVTableTypeName {
      def apply(ir: IRNames.ClassName) = new WasmVTableTypeName(ir.nameString)

      val ObjectVTable = apply(IRNames.ObjectClass)
    }

    final case class WasmITableTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name)
    object WasmITableTypeName {
      def apply(ir: IRNames.ClassName) = new WasmITableTypeName(ir.nameString)
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
