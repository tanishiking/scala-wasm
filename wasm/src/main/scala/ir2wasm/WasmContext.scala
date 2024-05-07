package wasm.ir2wasm

import scala.annotation.tailrec

import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap

import wasm.wasm4s._
import wasm.wasm4s.Names._
import wasm.wasm4s.Names.WasmTypeName._
import wasm.wasm4s.Types._

import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{ClassKind, Position}

import VarGen._

import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
import org.scalajs.linker.standard.LinkedTopLevelExport
import org.scalajs.linker.standard.LinkedClass

abstract class ReadOnlyWasmContext {
  import WasmContext._

  protected val classInfo = mutable.Map[IRNames.ClassName, WasmClassInfo]()

  val cloneFunctionTypeName: WasmTypeName
  val isJSClassInstanceFuncTypeName: WasmTypeName

  protected var _itablesLength: Int = 0
  def itablesLength = _itablesLength

  /** Get an index of the itable for the given interface. The itable instance must be placed at the
    * index in the array of itables (whose size is `itablesLength`).
    */
  def getItableIdx(iface: WasmClassInfo): Int = {
    val idx = iface.itableIdx
    if (idx < 0) throw new IllegalArgumentException(s"Interface $iface is not registed.")
    idx
  }

  def getClassInfoOption(name: IRNames.ClassName): Option[WasmClassInfo] =
    classInfo.get(name)

  def getClassInfo(name: IRNames.ClassName): WasmClassInfo =
    classInfo.getOrElse(name, throw new Error(s"Class not found: $name"))

  def inferTypeFromTypeRef(typeRef: IRTypes.TypeRef): IRTypes.Type = typeRef match {
    case IRTypes.PrimRef(tpe) =>
      tpe
    case IRTypes.ClassRef(className) =>
      if (className == IRNames.ObjectClass || getClassInfo(className).kind.isJSType)
        IRTypes.AnyType
      else
        IRTypes.ClassType(className)
    case typeRef: IRTypes.ArrayTypeRef =>
      IRTypes.ArrayType(typeRef)
  }
}

case class StringData(
    constantStringIndex: Int,
    offset: Int
)

abstract class TypeDefinableWasmContext extends ReadOnlyWasmContext { this: WasmContext =>
  private val functionTypes = LinkedHashMap.empty[WasmFunctionSignature, WasmTypeName]
  private val recFunctionTypes = LinkedHashMap.empty[WasmFunctionSignature, WasmTypeName]
  private val tableFunctionTypes = mutable.HashMap.empty[IRNames.MethodName, WasmTypeName]
  private val constantStringGlobals = LinkedHashMap.empty[String, StringData]
  protected val classItableGlobals = LinkedHashMap.empty[IRNames.ClassName, WasmGlobalName]
  private val closureDataTypes = LinkedHashMap.empty[List[IRTypes.Type], WasmTypeName]
  private val reflectiveProxies = LinkedHashMap.empty[IRNames.MethodName, Int]

  val moduleBuilder: ModuleBuilder = {
    new ModuleBuilder(new ModuleBuilder.FunctionSignatureProvider {
      def signatureToTypeName(sig: WasmFunctionSignature): WasmTypeName =
        addFunctionType(sig)
    })
  }

  protected var stringPool = new mutable.ArrayBuffer[Byte]()
  protected var nextConstantStringIndex: Int = 0
  private var nextConstatnStringOffset: Int = 0
  private var nextArrayTypeIndex: Int = 1
  private var nextClosureDataTypeIndex: Int = 1
  private var nextReflectiveProxyIdx: Int = 0

  def addFunction(fun: WasmFunction): Unit
  protected def addGlobal(g: WasmGlobal): Unit
  def getImportedModuleGlobal(moduleName: String): WasmGlobalName
  protected def addFuncDeclaration(name: WasmFunctionName): Unit

  /** Retrieves a unique identifier for a reflective proxy with the given name */
  def getReflectiveProxyId(name: IRNames.MethodName): Int =
    reflectiveProxies.getOrElseUpdate(
      name, {
        val idx = nextReflectiveProxyIdx
        nextReflectiveProxyIdx += 1
        idx
      }
    )

  /** Adds or reuses a function type for the given signature. */
  def addFunctionType(sig: WasmFunctionSignature): WasmTypeName = {
    functionTypes.getOrElseUpdate(
      sig, {
        val typeName = genTypeName.forFunction(functionTypes.size)
        moduleBuilder.addRecType(typeName, WasmFunctionType(sig))
        typeName
      }
    )
  }

  /** Adds or reuses a function type for the given signature that is part of the main `rectype`.
    *
    * This should be used for function types that are used inside other type declarations that are
    * part of the main `rectype`. In particular, it should be used for the function types appearing
    * in vtables and itables.
    */
  def addFunctionTypeInMainRecType(sig: WasmFunctionSignature): WasmTypeName = {
    recFunctionTypes.getOrElseUpdate(
      sig, {
        val typeName = genTypeName.forRecFunction(recFunctionTypes.size)
        mainRecType.addSubType(typeName, WasmFunctionType(sig))
        typeName
      }
    )
  }

  def tableFunctionType(methodName: IRNames.MethodName): WasmTypeName = {
    tableFunctionTypes.getOrElseUpdate(
      methodName, {
        val regularParamTyps = methodName.paramTypeRefs.map { typeRef =>
          TypeTransformer.transformType(inferTypeFromTypeRef(typeRef))(this)
        }
        val resultTyp =
          TypeTransformer.transformResultType(inferTypeFromTypeRef(methodName.resultTypeRef))(this)
        val sig = WasmFunctionSignature(WasmRefType.any :: regularParamTyps, resultTyp)
        addFunctionTypeInMainRecType(sig)
      }
    )
  }

  def addConstantStringGlobal(str: String): StringData = {
    constantStringGlobals.get(str) match {
      case Some(data) =>
        data

      case None =>
        val bytes = str.toCharArray.flatMap { char =>
          Array((char & 0xFF).toByte, (char >> 8).toByte)
        }
        val offset = nextConstatnStringOffset
        val data = StringData(nextConstantStringIndex, offset)
        constantStringGlobals(str) = data

        stringPool ++= bytes
        nextConstantStringIndex += 1
        nextConstatnStringOffset += bytes.length
        data
    }
  }

  def getConstantStringInstr(str: String): List[WasmInstr] =
    getConstantStringDataInstr(str) :+ WasmInstr.CALL(genFunctionName.stringLiteral)

  def getConstantStringDataInstr(str: String): List[WasmInstr.I32_CONST] = {
    val data = addConstantStringGlobal(str)
    List(
      WasmInstr.I32_CONST(data.offset),
      // Assuming that the stringLiteral method will instantiate the
      // constant string from the data section using "array.newData $i16Array ..."
      // The length of the array should be equal to the length of the WTF-16 encoded string
      WasmInstr.I32_CONST(str.length()),
      WasmInstr.I32_CONST(data.constantStringIndex)
    )
  }

  def getClosureDataStructType(captureParamTypes: List[IRTypes.Type]): WasmTypeName = {
    closureDataTypes.getOrElseUpdate(
      captureParamTypes, {
        val fields: List[WasmStructField] =
          for ((tpe, i) <- captureParamTypes.zipWithIndex)
            yield WasmStructField(
              genFieldName.captureParam(i),
              TypeTransformer.transformType(tpe)(this),
              isMutable = false
            )
        val structTypeName = genTypeName.captureData(nextClosureDataTypeIndex)
        nextClosureDataTypeIndex += 1
        val structType = WasmStructType(fields)
        moduleBuilder.addRecType(structTypeName, structType)
        structTypeName
      }
    )
  }

  def refFuncWithDeclaration(name: WasmFunctionName): WasmInstr.REF_FUNC = {
    addFuncDeclaration(name)
    WasmInstr.REF_FUNC(name)
  }

  private def extractArrayElemType(typeRef: IRTypes.ArrayTypeRef): IRTypes.Type = {
    if (typeRef.dimensions > 1) IRTypes.ArrayType(typeRef.copy(dimensions = typeRef.dimensions - 1))
    else inferTypeFromTypeRef(typeRef.base)
  }
}

final class WasmContext extends TypeDefinableWasmContext {
  import WasmContext._
  import WasmRefType.anyref

  private val _importedModules: mutable.LinkedHashSet[String] =
    new mutable.LinkedHashSet()

  def assignBuckets(classes: List[LinkedClass]): Unit =
    _itablesLength = assignBuckets0(classes.filterNot(_.kind.isJSType))

  private val _jsPrivateFieldNames: mutable.ListBuffer[IRNames.FieldName] =
    new mutable.ListBuffer()
  private val _funcDeclarations: mutable.LinkedHashSet[WasmFunctionName] =
    new mutable.LinkedHashSet()

  /** The main `rectype` containing the object model types. */
  val mainRecType: ModuleBuilder.RecTypeBuilder = new ModuleBuilder.RecTypeBuilder

  def addExport(exprt: WasmExport): Unit =
    moduleBuilder.addExport(exprt)

  def addFunction(fun: WasmFunction): Unit =
    moduleBuilder.addFunction(fun)

  def addGlobal(g: WasmGlobal): Unit =
    moduleBuilder.addGlobal(g)

  def addGlobalITable(name: IRNames.ClassName, g: WasmGlobal): Unit = {
    classItableGlobals.put(name, g.name)
    addGlobal(g)
  }

  def getImportedModuleGlobal(moduleName: String): WasmGlobalName = {
    val name = genGlobalName.forImportedModule(moduleName)
    if (_importedModules.add(moduleName)) {
      moduleBuilder.addImport(
        WasmImport(
          "__scalaJSImports",
          moduleName,
          WasmImportDesc.Global(name, anyref, isMutable = false)
        )
      )
    }
    name
  }

  def allImportedModules: List[String] = _importedModules.toList

  def addFuncDeclaration(name: WasmFunctionName): Unit =
    _funcDeclarations += name

  def putClassInfo(name: IRNames.ClassName, info: WasmClassInfo): Unit =
    classInfo.put(name, info)

  def addJSPrivateFieldName(fieldName: IRNames.FieldName): Unit =
    _jsPrivateFieldNames += fieldName

  private def addHelperImport(
      name: WasmFunctionName,
      params: List[WasmType],
      results: List[WasmType]
  ): Unit = {
    val sig = WasmFunctionSignature(params, results)
    val typeName = addFunctionType(sig)
    moduleBuilder.addImport(
      WasmImport("__scalaJSHelpers", name.name, WasmImportDesc.Func(name, typeName))
    )
  }

  private def addGlobalHelperImport(
      name: WasmGlobalName,
      typ: WasmType,
      isMutable: Boolean
  ): Unit = {
    moduleBuilder.addImport(
      WasmImport(
        "__scalaJSHelpers",
        name.name,
        WasmImportDesc.Global(name, typ, isMutable)
      )
    )
  }

  locally {
    moduleBuilder.addRecType(genTypeName.i8Array, WasmArrayType(WasmFieldType(WasmInt8, true)))
    moduleBuilder.addRecType(genTypeName.i16Array, WasmArrayType(WasmFieldType(WasmInt16, true)))
    moduleBuilder.addRecType(genTypeName.i32Array, WasmArrayType(WasmFieldType(WasmInt32, true)))
    moduleBuilder.addRecType(genTypeName.i64Array, WasmArrayType(WasmFieldType(WasmInt64, true)))
    moduleBuilder.addRecType(genTypeName.f32Array, WasmArrayType(WasmFieldType(WasmFloat32, true)))
    moduleBuilder.addRecType(genTypeName.f64Array, WasmArrayType(WasmFieldType(WasmFloat64, true)))
    moduleBuilder.addRecType(genTypeName.anyArray, WasmArrayType(WasmFieldType(anyref, true)))

    moduleBuilder.addRecTypeBuilder(mainRecType)
  }

  val cloneFunctionTypeName: WasmTypeName =
    addFunctionTypeInMainRecType(
      WasmFunctionSignature(
        List(WasmRefType(WasmHeapType.ObjectType)),
        List(WasmRefType(WasmHeapType.ObjectType))
      )
    )

  val isJSClassInstanceFuncTypeName: WasmTypeName =
    addFunctionTypeInMainRecType(WasmFunctionSignature(List(WasmRefType.anyref), List(WasmInt32)))

  /** Run-time type data of a `TypeRef`.
    *
    * Support for `j.l.Class` methods and other reflective operations.
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
      WasmStructField(name, anyref, isMutable = true),
      WasmStructField(classOfValue, nullable(WasmHeapType.ClassType), isMutable = true),
      WasmStructField(arrayOf, nullable(genTypeName.ObjectVTable), isMutable = true),
      WasmStructField(cloneFunction, nullable(cloneFunctionTypeName), isMutable = false),
      WasmStructField(
        isJSClassInstance,
        nullable(isJSClassInstanceFuncTypeName),
        isMutable = false
      ),
      WasmStructField(
        reflectiveProxies,
        WasmRefType(genTypeName.reflectiveProxies),
        isMutable = false
      )
    )
  }

  locally {
    mainRecType.addSubType(
      genTypeName.typeDataArray,
      WasmArrayType(WasmFieldType(WasmRefType(genTypeName.typeData), isMutable = false))
    )
    mainRecType.addSubType(
      genTypeName.itables,
      WasmArrayType(WasmFieldType(WasmRefType.nullable(WasmHeapType.Struct), isMutable = true))
    )
    mainRecType.addSubType(
      genTypeName.reflectiveProxies,
      WasmArrayType(WasmFieldType(WasmRefType(genTypeName.reflectiveProxy), isMutable = false))
    )

    mainRecType.addSubType(
      WasmSubType(genTypeName.typeData, isFinal = false, None, WasmStructType(typeDataStructFields))
    )

    mainRecType.addSubType(
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

  locally {
    val exceptionSig = WasmFunctionSignature(List(WasmRefType.externref), Nil)
    val typeName = addFunctionType(exceptionSig)
    moduleBuilder.addImport(
      WasmImport(
        "__scalaJSHelpers",
        "JSTag",
        WasmImportDesc.Tag(genTagName.exceptionTagName, typeName)
      )
    )
  }

  addHelperImport(genFunctionName.is, List(anyref, anyref), List(WasmInt32))

  addHelperImport(genFunctionName.undef, List(), List(WasmRefType.any))
  addHelperImport(genFunctionName.isUndef, List(anyref), List(WasmInt32))

  locally {
    import IRTypes._
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

  addHelperImport(genFunctionName.emptyString, List(), List(WasmRefType.any))
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
    List(anyref, anyref, WasmRefType.func, WasmRefType.func, WasmRefType.func),
    List(WasmRefType.any)
  )
  addHelperImport(
    genFunctionName.createJSClassRest,
    List(anyref, anyref, WasmRefType.func, WasmRefType.func, WasmRefType.func, WasmInt32),
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

  addGlobalHelperImport(genGlobalName.idHashCodeMap, WasmRefType.extern, isMutable = false)

  def complete(
      moduleInitializers: List[ModuleInitializer.Initializer],
      classesWithStaticInit: List[IRNames.ClassName],
      topLevelExportDefs: List[LinkedTopLevelExport]
  ): Unit = {
    /* Before generating the string globals in `genStartFunction()`, make sure
     * to allocate the ones that will be required by the module initializers.
     */
    for (init <- moduleInitializers) {
      ModuleInitializerImpl.fromInitializer(init) match {
        case ModuleInitializerImpl.MainMethodWithArgs(_, _, args) =>
          args.foreach(addConstantStringGlobal(_))
        case ModuleInitializerImpl.VoidMainMethod(_, _) =>
          () // nothing to do
      }
    }

    // string
    moduleBuilder.addData(WasmData(genDataName.string, stringPool.toArray, WasmData.Mode.Passive))
    addGlobal(
      WasmGlobal(
        genGlobalName.stringLiteralCache,
        WasmRefType(genTypeName.anyArray),
        WasmExpr(
          List(
            WasmInstr.I32_CONST(nextConstantStringIndex),
            WasmInstr.ARRAY_NEW_DEFAULT(genTypeName.anyArray)
          )
        ),
        isMutable = false
      )
    )

    genStartFunction(moduleInitializers, classesWithStaticInit, topLevelExportDefs)
    genDeclarativeElements()
  }

  private def genStartFunction(
      moduleInitializers: List[ModuleInitializer.Initializer],
      classesWithStaticInit: List[IRNames.ClassName],
      topLevelExportDefs: List[LinkedTopLevelExport]
  ): Unit = {
    import WasmInstr._
    import WasmTypeName._

    implicit val pos = Position.NoPosition

    val fctx = WasmFunctionContext(genFunctionName.start, Nil, Nil)(this, pos)

    import fctx.instrs

    // Initialize itables
    for ((name, globalName) <- classItableGlobals) {
      val classInfo = getClassInfo(name)
      val interfaces = classInfo.ancestors.map(getClassInfo(_)).filter(_.isInterface)
      val resolvedMethodInfos = classInfo.resolvedMethodInfos

      interfaces.foreach { iface =>
        val idx = getItableIdx(iface)
        instrs += WasmInstr.GLOBAL_GET(globalName)
        instrs += WasmInstr.I32_CONST(idx)

        for (method <- iface.tableEntries)
          instrs += refFuncWithDeclaration(resolvedMethodInfos(method).tableEntryName)
        instrs += WasmInstr.STRUCT_NEW(genTypeName.forITable(iface.name))
        instrs += WasmInstr.ARRAY_SET(genTypeName.itables)
      }
    }

    locally {
      // For array classes, resolve methods in jl.Object
      val globalName = genGlobalName.arrayClassITable
      val resolvedMethodInfos = getClassInfo(IRNames.ObjectClass).resolvedMethodInfos

      for {
        interfaceName <- List(IRNames.SerializableClass, IRNames.CloneableClass)
        // Use getClassInfoOption in case the reachability analysis got rid of those interfaces
        interfaceInfo <- getClassInfoOption(interfaceName)
      } {
        instrs += GLOBAL_GET(globalName)
        instrs += I32_CONST(getItableIdx(interfaceInfo))

        for (method <- interfaceInfo.tableEntries)
          instrs += refFuncWithDeclaration(resolvedMethodInfos(method).tableEntryName)
        instrs += STRUCT_NEW(genTypeName.forITable(interfaceName))
        instrs += ARRAY_SET(genTypeName.itables)
      }
    }

    // Initialize the JS private field symbols

    for (fieldName <- _jsPrivateFieldNames) {
      instrs += WasmInstr.CALL(genFunctionName.newSymbol)
      instrs += WasmInstr.GLOBAL_SET(genGlobalName.forJSPrivateField(fieldName))
    }

    // Emit the static initializers

    for (className <- classesWithStaticInit) {
      val funcName = genFunctionName.forMethod(
        IRTrees.MemberNamespace.StaticConstructor,
        className,
        IRNames.StaticInitializerName
      )
      instrs += WasmInstr.CALL(funcName)
    }

    // Initialize the top-level exports that require it

    for (tle <- topLevelExportDefs) {
      tle.tree match {
        case IRTrees.TopLevelJSClassExportDef(_, exportName) =>
          instrs += CALL(genFunctionName.loadJSClass(tle.owningClass))
          instrs += GLOBAL_SET(genGlobalName.forTopLevelExport(tle.exportName))
        case IRTrees.TopLevelModuleExportDef(_, exportName) =>
          instrs += CALL(genFunctionName.loadModule(tle.owningClass))
          instrs += GLOBAL_SET(genGlobalName.forTopLevelExport(tle.exportName))
        case IRTrees.TopLevelMethodExportDef(_, methodDef) =>
          // We only need initialization if there is a restParam
          if (methodDef.restParam.isDefined) {
            instrs += refFuncWithDeclaration(genFunctionName.forExport(tle.exportName))
            instrs += I32_CONST(methodDef.args.size)
            instrs += CALL(genFunctionName.closureRestNoData)
            instrs += GLOBAL_SET(genGlobalName.forTopLevelExport(tle.exportName))
          }
        case IRTrees.TopLevelFieldExportDef(_, _, _) =>
          // Nothing to do
          ()
      }
    }

    // Emit the module initializers

    moduleInitializers.foreach { init =>
      def genCallStatic(className: IRNames.ClassName, methodName: IRNames.MethodName): Unit = {
        val functionName =
          genFunctionName.forMethod(IRTrees.MemberNamespace.PublicStatic, className, methodName)
        instrs += WasmInstr.CALL(functionName)
      }

      val stringArrayTypeRef = IRTypes.ArrayTypeRef(IRTypes.ClassRef(IRNames.BoxedStringClass), 1)

      val callTree = ModuleInitializerImpl.fromInitializer(init) match {
        case ModuleInitializerImpl.MainMethodWithArgs(className, encodedMainMethodName, args) =>
          IRTrees.ApplyStatic(
            IRTrees.ApplyFlags.empty,
            className,
            IRTrees.MethodIdent(encodedMainMethodName),
            List(IRTrees.ArrayValue(stringArrayTypeRef, args.map(IRTrees.StringLiteral(_))))
          )(IRTypes.NoType)

        case ModuleInitializerImpl.VoidMainMethod(className, encodedMainMethodName) =>
          IRTrees.ApplyStatic(
            IRTrees.ApplyFlags.empty,
            className,
            IRTrees.MethodIdent(encodedMainMethodName),
            Nil
          )(IRTypes.NoType)
      }

      WasmExpressionBuilder.generateIRBody(callTree, IRTypes.NoType)(this, fctx)
    }

    // Finish the start function

    fctx.buildAndAddToContext()
    moduleBuilder.setStart(genFunctionName.start)
  }

  private def genDeclarativeElements(): Unit = {
    // Aggregated Elements

    if (_funcDeclarations.nonEmpty) {
      /* Functions that are referred to with `ref.func` in the Code section
       * must be declared ahead of time in one of the earlier sections
       * (otherwise the module does not validate). It can be the Global section
       * if they are meaningful there (which is why `ref.func` in the vtables
       * work out of the box). In the absence of any other specific place, an
       * Element section with the declarative mode is the recommended way to
       * introduce these declarations.
       */
      val exprs = _funcDeclarations.toList.map { name =>
        WasmExpr(List(WasmInstr.REF_FUNC(name)))
      }
      moduleBuilder.addElement(
        WasmElement(WasmRefType.funcref, exprs, WasmElement.Mode.Declarative)
      )
    }
  }

  /** Group interface types + types that implements any interfaces into buckets, where no two types
    * in the same bucket can have common subtypes.
    *
    * It allows compressing the itable by reusing itable's index (buckets) for unrelated types,
    * instead of having a 1-1 mapping from type to index. As a result, the itables' length will be
    * the same as the number of buckets).
    *
    * The algorithm separates the type hierarchy into three disjoint subsets,
    *
    *   - join types: types with multiple parents (direct supertypes) that have only single
    *     subtyping descendants: `join(T) = {x ∈ multis(T) | ∄ y ∈ multis(T) : y <: x}` where
    *     multis(T) means types with multiple direct supertypes.
    *   - spine types: all ancestors of join types: `spine(T) = {x ∈ T | ∃ y ∈ join(T) : x ∈
    *     ancestors(y)}`
    *   - plain types: types that are neither join nor spine types
    *
    * The bucket assignment process consists of two parts:
    *
    * **1. Assign buckets to spine types**
    *
    * Two spine types can share the same bucket only if they do not have any common join type
    * descendants.
    *
    * Visit spine types in reverse topological order because (from leaves to root) when assigning a
    * a spine type to bucket, the algorithm already has the complete information about the
    * join/spine type descendants of that spine type.
    *
    * Assign a bucket to a spine type if adding it doesn't violate the bucket assignment rule: two
    * spine types can share a bucket only if they don't have any common join type descendants. If no
    * existing bucket satisfies the rule, create a new bucket.
    *
    * **2. Assign buckets to non-spine types (plain and join types)**
    *
    * Visit these types in level order (from root to leaves) For each type, compute the set of
    * buckets already used by its ancestors. Assign the type to any available bucket not in this
    * set. If no available bucket exists, create a new one.
    *
    * To test if type A is a subtype of type B: load the bucket index of type B (we do this by
    * `getItableIdx`), load the itable at that index from A, and check if the itable is an itable
    * for B.
    *
    * @see
    *   This algorithm is based on the "packed encoding" presented in the paper "Efficient Type
    *   Inclusion Tests"
    *   [[https://www.researchgate.net/publication/2438441_Efficient_Type_Inclusion_Tests]]
    */
  private def assignBuckets0(classes: List[LinkedClass]): Int = {
    var nextIdx = 0
    def newBucket(): Bucket = {
      val idx = nextIdx
      nextIdx += 1
      new Bucket(idx)
    }
    def getAllInterfaces(info: WasmClassInfo): List[IRNames.ClassName] =
      info.ancestors.filter(getClassInfo(_).isInterface)

    val buckets = new mutable.ListBuffer[Bucket]()

    /** All join type descendants of the class */
    val joinsOf =
      new mutable.HashMap[IRNames.ClassName, mutable.HashSet[IRNames.ClassName]]()

    /** the buckets that have been assigned to any of the ancestors of the class */
    val usedOf = new mutable.HashMap[IRNames.ClassName, mutable.HashSet[Bucket]]()
    val spines = new mutable.HashSet[IRNames.ClassName]()

    for (clazz <- classes.reverseIterator) {
      val info = getClassInfo(clazz.name.name)
      val ifaces = getAllInterfaces(info)
      if (ifaces.nonEmpty) {
        val joins = joinsOf.getOrElse(clazz.name.name, new mutable.HashSet())

        if (joins.nonEmpty) { // spine type
          var found = false
          val bs = buckets.iterator
          // look for an existing bucket to add the spine type to
          while (!found && bs.hasNext) {
            val b = bs.next()
            // two spine types can share a bucket only if they don't have any common join type descendants
            if (!b.joins.exists(joins)) {
              found = true
              b.add(info)
              b.joins ++= joins
            }
          }
          if (!found) { // there's no bucket to add, create new bucket
            val b = newBucket()
            b.add(info)
            buckets.append(b)
            b.joins ++= joins
          }
          for (iface <- ifaces) {
            joinsOf.getOrElseUpdate(iface, new mutable.HashSet()) ++= joins
          }
          spines.add(clazz.name.name)
        } else if (ifaces.length > 1) { // join type, add to joins map, bucket assignment is done later
          ifaces.foreach { iface =>
            joinsOf.getOrElseUpdate(iface, new mutable.HashSet()) += clazz.name.name
          }
        }
        // else: plain, do nothing
      }

    }

    for (clazz <- classes) {
      val info = getClassInfo(clazz.name.name)
      val ifaces = getAllInterfaces(info)
      if (ifaces.nonEmpty && !spines.contains(clazz.name.name)) {
        val used = usedOf.getOrElse(clazz.name.name, new mutable.HashSet())
        for {
          iface <- ifaces
          parentUsed <- usedOf.get(iface)
        } { used ++= parentUsed }

        var found = false
        val bs = buckets.iterator
        while (!found && bs.hasNext) {
          val b = bs.next()
          if (!used.contains(b)) {
            found = true
            b.add(info)
            used.add(b)
          }
        }
        if (!found) {
          val b = newBucket()
          buckets.append(b)
          b.add(info)
          used.add(b)
        }
      }
    }
    buckets.length
  }
}

object WasmContext {
  private val classFieldOffset = 2 // vtable, itables

  final class WasmClassInfo(
      ctx: WasmContext,
      val name: IRNames.ClassName,
      val kind: ClassKind,
      val jsClassCaptures: Option[List[IRTrees.ParamDef]],
      classConcretePublicMethodNames: List[IRNames.MethodName],
      val allFieldDefs: List[IRTrees.FieldDef],
      val superClass: Option[IRNames.ClassName],
      val interfaces: List[IRNames.ClassName],
      val ancestors: List[IRNames.ClassName],
      private var _hasInstances: Boolean,
      val isAbstract: Boolean,
      val hasRuntimeTypeInfo: Boolean,
      val jsNativeLoadSpec: Option[IRTrees.JSNativeLoadSpec],
      val jsNativeMembers: Map[IRNames.MethodName, IRTrees.JSNativeLoadSpec],
      private var _itableIdx: Int
  ) {
    private val fieldIdxByName: Map[IRNames.FieldName, Int] =
      allFieldDefs.map(_.name.name).zipWithIndex.map(p => p._1 -> (p._2 + classFieldOffset)).toMap

    val resolvedMethodInfos: Map[IRNames.MethodName, ConcreteMethodInfo] = {
      if (kind.isClass || kind == ClassKind.HijackedClass) {
        val inherited: Map[IRNames.MethodName, ConcreteMethodInfo] = superClass match {
          case Some(superClass) => ctx.getClassInfo(superClass).resolvedMethodInfos
          case None             => Map.empty
        }

        for (methodName <- classConcretePublicMethodNames)
          inherited.get(methodName).foreach(_.markOverridden())

        classConcretePublicMethodNames.foldLeft(inherited) { (prev, methodName) =>
          prev.updated(methodName, new ConcreteMethodInfo(name, methodName))
        }
      } else {
        Map.empty
      }
    }

    private val methodsCalledDynamically = mutable.HashSet.empty[IRNames.MethodName]

    private var _tableEntries: List[IRNames.MethodName] = null
    private var _tableMethodInfos: Map[IRNames.MethodName, TableMethodInfo] = null

    // See caller in Preprocessor.preprocess
    def setHasInstances(): Unit =
      _hasInstances = true

    def hasInstances: Boolean = _hasInstances

    def setItableIdx(idx: Int): Unit = _itableIdx = idx

    /** Returns the index of this interface's itable in the classes' interface tables.
      */
    def itableIdx: Int = _itableIdx

    private var _specialInstanceTypes: Int = 0

    def addSpecialInstanceType(jsValueType: Int): Unit =
      _specialInstanceTypes |= (1 << jsValueType)

    /** A bitset of the `jsValueType`s corresponding to hijacked classes that extend this class.
      *
      * This value is used for instance tests against this class. A JS value `x` is an instance of
      * this type iff `jsValueType(x)` is a member of this bitset. Because of how a bitset works,
      * this means testing the following formula:
      *
      * {{{
      * ((1 << jsValueType(x)) & specialInstanceTypes) != 0
      * }}}
      *
      * For example, if this class is `Comparable`, we want the bitset to contain the values for
      * `boolean`, `string` and `number` (but not `undefined`), because `jl.Boolean`, `jl.String`
      * and `jl.Double` implement `Comparable`.
      *
      * This field is initialized with 0, and augmented during preprocessing by calls to
      * `addSpecialInstanceType`.
      *
      * This technique is used both for static `isInstanceOf` tests as well as reflective tests
      * through `Class.isInstance`. For the latter, this value is stored in
      * `typeData.specialInstanceTypes`. For the former, it is embedded as a constant in the
      * generated code.
      *
      * See the `isInstance` and `genInstanceTest` helpers.
      *
      * Special cases: this value remains 0 for all the numeric hijacked classes except `jl.Double`,
      * since `jsValueType(x) == JSValueTypeNumber` is not enough to deduce that
      * `x.isInstanceOf[Int]`, for example.
      */
    def specialInstanceTypes: Int = _specialInstanceTypes

    /** Is this class an ancestor of any hijacked class?
      *
      * This includes but is not limited to the hijacked classes themselves, as well as `jl.Object`.
      */
    def isAncestorOfHijackedClass: Boolean =
      specialInstanceTypes != 0 || kind == ClassKind.HijackedClass

    def isInterface = kind == ClassKind.Interface

    def registerDynamicCall(methodName: IRNames.MethodName): Unit =
      methodsCalledDynamically += methodName

    def buildMethodTable(): Unit = {
      if (_tableEntries != null)
        throw new IllegalStateException(s"Duplicate call to buildMethodTable() for $name")

      kind match {
        case ClassKind.Class | ClassKind.ModuleClass | ClassKind.HijackedClass =>
          val superClassInfo = superClass.map(ctx.getClassInfo(_))
          val superTableEntries =
            superClassInfo.fold[List[IRNames.MethodName]](Nil)(_.tableEntries)
          val superTableMethodInfos =
            superClassInfo.fold[Map[IRNames.MethodName, TableMethodInfo]](Map.empty)(
              _.tableMethodInfos
            )

          /* When computing the table entries to add for this class, exclude:
           * - methods that are already in the super class' table entries, and
           * - methods that are effectively final, since they will always be
           *   statically resolved instead of using the table dispatch.
           */
          val newTableEntries = methodsCalledDynamically.toList
            .filter(!superTableMethodInfos.contains(_))
            .filterNot(m => resolvedMethodInfos.get(m).exists(_.isEffectivelyFinal))
            .sorted // for stability

          val baseIndex = superTableMethodInfos.size
          val newTableMethodInfos = newTableEntries.zipWithIndex.map { case (m, index) =>
            m -> new TableMethodInfo(m, baseIndex + index)
          }

          _tableEntries = superTableEntries ::: newTableEntries
          _tableMethodInfos = superTableMethodInfos ++ newTableMethodInfos

        case ClassKind.Interface =>
          _tableEntries = methodsCalledDynamically.toList.sorted // for stability
          _tableMethodInfos = tableEntries.zipWithIndex.map { case (m, index) =>
            m -> new TableMethodInfo(m, index)
          }.toMap

        case _ =>
          _tableEntries = Nil
          _tableMethodInfos = Map.empty
      }

      methodsCalledDynamically.clear() // gc
    }

    def tableEntries: List[IRNames.MethodName] = {
      if (_tableEntries == null)
        throw new IllegalStateException(s"Table not yet built for $name")
      _tableEntries
    }

    def tableMethodInfos: Map[IRNames.MethodName, TableMethodInfo] = {
      if (_tableMethodInfos == null)
        throw new IllegalStateException(s"Table not yet built for $name")
      _tableMethodInfos
    }

    def getFieldIdx(name: IRNames.FieldName): WasmFieldIdx = {
      WasmFieldIdx(
        fieldIdxByName.getOrElse(
          name, {
            throw new AssertionError(
              s"Unknown field ${name.nameString} in class ${this.name.nameString}"
            )
          }
        )
      )
    }
  }

  final class ConcreteMethodInfo(
      val ownerClass: IRNames.ClassName,
      val methodName: IRNames.MethodName
  ) {
    val tableEntryName = genFunctionName.forTableEntry(ownerClass, methodName)

    private var effectivelyFinal: Boolean = true

    private[WasmContext] def markOverridden(): Unit =
      effectivelyFinal = false

    def isEffectivelyFinal: Boolean = effectivelyFinal
  }

  final class TableMethodInfo(val methodName: IRNames.MethodName, val tableIndex: Int)

  private[WasmContext] class Bucket(idx: Int) {
    def add(clazz: WasmClassInfo) = clazz.setItableIdx((idx))

    /** A set of join types that are descendants of the types assigned to that bucket */
    val joins = new mutable.HashSet[IRNames.ClassName]()
  }
}
