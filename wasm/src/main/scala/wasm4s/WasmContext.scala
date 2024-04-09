package wasm.wasm4s

import scala.annotation.tailrec

import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap

import Names._
import Names.WasmTypeName._
import Types._

import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{ClassKind, Position}

import wasm.ir2wasm.TypeTransformer
import wasm.ir2wasm.WasmExpressionBuilder

import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
import java.nio.charset.StandardCharsets

trait ReadOnlyWasmContext {
  import WasmContext._
  protected val gcTypes = new WasmSymbolTable[WasmTypeName, WasmGCTypeDefinition]()
  protected val functions = new WasmSymbolTable[WasmFunctionName, WasmFunction]()
  protected val globals = new WasmSymbolTable[WasmGlobalName, WasmGlobal]()

  protected val itableIdx = mutable.Map[IRNames.ClassName, Int]()
  protected val classInfo = mutable.Map[IRNames.ClassName, WasmClassInfo]()
  private val vtablesCache = mutable.Map[IRNames.ClassName, WasmVTable]()
  protected var nextItableIdx: Int

  val cloneFunctionTypeName: WasmFunctionTypeName

  def itablesLength = nextItableIdx

  /** Get an index of the itable for the given interface. The itable instance must be placed at the
    * index in the array of itables (whose size is `itablesLength`).
    */
  def getItableIdx(iface: IRNames.ClassName): Int =
    itableIdx.getOrElse(
      iface,
      throw new IllegalArgumentException(s"Interface $iface is not registed.")
    )

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

  /** Collects all methods declared and inherited by the given class, super-class.
    *
    * @param className
    *   class to collect methods from
    * @param includeAbstractMethods
    *   whether to include abstract methods
    * @return
    *   list of methods in order that "collectVTableMethods(superClass) ++ methods from the class"
    */
  private def collectVTableMethods(
      className: IRNames.ClassName,
      includeAbstractMethods: Boolean
  ): List[WasmFunctionInfo] = {
    val info = classInfo.getOrElse(className, throw new Error(s"Class not found: $className"))
    assert(
      info.kind.isClass || info.kind == ClassKind.HijackedClass,
      s"collectVTableMethods cannot be called for non-class ${className.nameString}"
    )
    val fromSuperClass =
      info.superClass.map(collectVTableMethods(_, includeAbstractMethods)).getOrElse(Nil)
    fromSuperClass ++
      (if (includeAbstractMethods) info.methods
       else info.methods.filterNot(_.isAbstract))
  }

  def calculateGlobalVTable(name: IRNames.ClassName): List[WasmFunctionInfo] = {
    val vtableType = calculateVtableType(name)
    // Do not include abstract methods when calculating vtable instance,
    // all slots should be filled with the function reference to the concrete methods
    val methodsReverse = collectVTableMethods(name, includeAbstractMethods = false).reverse
    vtableType.functions.map { slot =>
      methodsReverse
        .find(_.name.simpleName == slot.name.simpleName)
        .getOrElse(throw new Error(s"No implementation found for ${slot.name} in ${name}"))
    }
  }

  def calculateVtableType(name: IRNames.ClassName): WasmVTable = {
    vtablesCache.getOrElseUpdate(
      name, {
        val functions =
          collectVTableMethods(name, includeAbstractMethods = true)
            .foldLeft(Array.empty[WasmFunctionInfo]) { case (acc, m) =>
              acc.indexWhere(_.name.simpleName == m.name.simpleName) match {
                case i if i < 0 => acc :+ m
                case i          => if (m.isAbstract) acc else acc.updated(i, m)
              }
            }
            .toList
        WasmVTable(functions)
      }
    )
  }
}

case class StringData(
    constantStringIndex: Int,
    offset: Int
)

trait TypeDefinableWasmContext extends ReadOnlyWasmContext { this: WasmContext =>
  protected val functionSignatures = LinkedHashMap.empty[WasmFunctionSignature, Int]
  protected val constantStringGlobals = LinkedHashMap.empty[String, StringData]
  protected val classItableGlobals = LinkedHashMap.empty[IRNames.ClassName, WasmGlobalName]
  protected val closureDataTypes = LinkedHashMap.empty[List[IRTypes.Type], WasmStructType]

  protected var stringPool = new mutable.ArrayBuffer[Byte]()
  protected var nextConstantStringIndex: Int = 0
  private var nextConstatnStringOffset: Int = 0
  private var nextArrayTypeIndex: Int = 1
  private var nextClosureDataTypeIndex: Int = 1

  def addFunction(fun: WasmFunction): Unit
  protected def addGlobal(g: WasmGlobal): Unit
  def getImportedModuleGlobal(moduleName: String): WasmGlobalName
  protected def addFuncDeclaration(name: WasmFunctionName): Unit

  val cloneFunctionTypeName =
    addFunctionType(
      WasmFunctionSignature(
        List(WasmRefType(WasmHeapType.ObjectType)),
        List(WasmRefType(WasmHeapType.ObjectType))
      )
    )

  val exceptionTagName: WasmTagName

  def addFunctionType(sig: WasmFunctionSignature): WasmFunctionTypeName = {
    functionSignatures.get(sig) match {
      case None =>
        val idx = functionSignatures.size
        functionSignatures.update(sig, idx)
        val typeName = WasmFunctionTypeName(idx)
        val ty = WasmFunctionType(typeName, sig)
        module.addFunctionType(ty)
        typeName
      case Some(value) => WasmFunctionTypeName(value)
    }
  }

  def addConstantStringGlobal(str: String): StringData = {
    constantStringGlobals.get(str) match {
      case Some(data) =>
        data

      case None =>
        val bytes = encodeStringToWTF16LE(str)
        val offset = nextConstatnStringOffset
        val data = StringData(nextConstantStringIndex, offset)
        constantStringGlobals(str) = data

        stringPool ++= bytes
        nextConstantStringIndex += 1
        nextConstatnStringOffset += bytes.length
        data
    }
  }

  def getConstantStringInstr(str: String): List[WasmInstr] = {
    val data = addConstantStringGlobal(str)
    List(
      WasmInstr.I32_CONST(data.offset),
      // Assuming that the stringLiteral method will instantiate the
      // constant string from the data section using "array.newData $i16Array ..."
      // The length of the array should be equal to the length of the WTF-16 encoded string
      WasmInstr.I32_CONST(str.length()),
      WasmInstr.I32_CONST(data.constantStringIndex),
      WasmInstr.CALL(WasmFunctionName.stringLiteral)
    )
  }

  def getClosureDataStructType(captureParamTypes: List[IRTypes.Type]): WasmStructType = {
    closureDataTypes.getOrElseUpdate(
      captureParamTypes, {
        val fields: List[WasmStructField] =
          for ((tpe, i) <- captureParamTypes.zipWithIndex)
            yield WasmStructField(
              WasmFieldName.captureParam(i),
              TypeTransformer.transformType(tpe)(this),
              isMutable = false
            )
        val structTypeName = WasmStructTypeName.captureData(nextClosureDataTypeIndex)
        nextClosureDataTypeIndex += 1
        val structType = WasmStructType(structTypeName, fields, superType = None)
        addGCType(structType)
        structType
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

  /** http://simonsapin.github.io/wtf-8/#encoding-ill-formed-utf-16
    */
  private def encodeStringToWTF16LE(input: String): Array[Byte] = {
    val result = scala.collection.mutable.ArrayBuffer[Int]()
    var i = 0
    while (i < input.length) {
      val codePoint = input.codePointAt(i)
      if (codePoint < 0x10000) {
        // BMP code point
        result += codePoint
        i += Character.charCount(codePoint)
      } else {
        // Supplementary code point
        val highSurrogate = ((codePoint - 0x10000) >> 10) + 0xD800
        val lowSurrogate = ((codePoint - 0x10000) & 0x3FF) + 0xDC00
        result += highSurrogate
        result += lowSurrogate
        i += 2
      }
    }

    result
      .flatMap(codeUnit => Seq((codeUnit & 0xFF).toByte, ((codeUnit >> 8) & 0xFF).toByte))
      .toArray
  }
}

class WasmContext(val module: WasmModule) extends TypeDefinableWasmContext {
  import WasmContext._
  import WasmRefType.anyref

  private val _importedModules: mutable.LinkedHashSet[String] =
    new mutable.LinkedHashSet()

  override protected var nextItableIdx: Int = 0

  private val _jsPrivateFieldNames: mutable.ListBuffer[IRNames.FieldName] =
    new mutable.ListBuffer()
  private val _funcDeclarations: mutable.LinkedHashSet[WasmFunctionName] =
    new mutable.LinkedHashSet()

  def addExport(exprt: WasmExport): Unit = module.addExport(exprt)
  def addFunction(fun: WasmFunction): Unit = {
    module.addFunction(fun)
    functions.define(fun)
  }
  def addGCType(ty: WasmStructType): Unit = {
    module.addRecGroupType(ty)
    gcTypes.define(ty)
  }

  def addGlobalITable(name: IRNames.ClassName, g: WasmGlobal): Unit = {
    classItableGlobals.put(name, g.name)
    module.addGlobal(g)
    globals.define(g)
  }
  def addGlobal(g: WasmGlobal): Unit = {
    module.addGlobal(g)
    globals.define(g)
  }

  def getImportedModuleGlobal(moduleName: String): WasmGlobalName = {
    val name = WasmGlobalName.forImportedModule(moduleName)
    if (_importedModules.add(moduleName)) {
      module.addImport(
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

  def putClassInfo(name: IRNames.ClassName, info: WasmClassInfo): Unit = {
    classInfo.put(name, info)
    if (info.isInterface) {
      itableIdx.put(name, nextItableIdx)
      nextItableIdx += 1
    }
  }

  def addJSPrivateFieldName(fieldName: IRNames.FieldName): Unit =
    _jsPrivateFieldNames += fieldName

  val exceptionTagName: WasmTagName = WasmTagName("exception")

  locally {
    val exceptionSig = WasmFunctionSignature(List(anyref), Nil)
    val exceptionFunType = addFunctionType(exceptionSig)
    module.addTag(WasmTag(exceptionTagName, exceptionFunType))
  }

  private def addHelperImport(
      name: WasmFunctionName,
      params: List[WasmType],
      results: List[WasmType]
  ): Unit = {
    val sig = WasmFunctionSignature(params, results)
    val typ = WasmFunctionType(addFunctionType(sig), sig)
    module.addImport(WasmImport(name.namespace, name.simpleName, WasmImportDesc.Func(name, typ)))
  }

  addGCType(WasmStructType.typeData(this))

  addHelperImport(WasmFunctionName.is, List(anyref, anyref), List(WasmInt32))

  addHelperImport(WasmFunctionName.undef, List(), List(WasmRefType.any))
  addHelperImport(WasmFunctionName.isUndef, List(anyref), List(WasmInt32))

  locally {
    import IRTypes._
    for (primRef <- List(BooleanRef, ByteRef, ShortRef, IntRef, FloatRef, DoubleRef)) {
      val wasmType = primRef match {
        case FloatRef  => WasmFloat32
        case DoubleRef => WasmFloat64
        case _         => WasmInt32
      }
      addHelperImport(WasmFunctionName.box(primRef), List(wasmType), List(anyref))
      addHelperImport(WasmFunctionName.unbox(primRef), List(anyref), List(wasmType))
      addHelperImport(WasmFunctionName.unboxOrNull(primRef), List(anyref), List(anyref))
      addHelperImport(WasmFunctionName.typeTest(primRef), List(anyref), List(WasmInt32))
    }
  }

  addHelperImport(WasmFunctionName.fmod, List(WasmFloat64, WasmFloat64), List(WasmFloat64))

  addHelperImport(
    WasmFunctionName.closure,
    List(WasmRefType.func, anyref),
    List(WasmRefType.any)
  )
  addHelperImport(
    WasmFunctionName.closureThis,
    List(WasmRefType.func, anyref),
    List(WasmRefType.any)
  )
  addHelperImport(
    WasmFunctionName.closureRest,
    List(WasmRefType.func, anyref, WasmInt32),
    List(WasmRefType.any)
  )
  addHelperImport(
    WasmFunctionName.closureThisRest,
    List(WasmRefType.func, anyref, WasmInt32),
    List(WasmRefType.any)
  )

  addHelperImport(WasmFunctionName.emptyString, List(), List(WasmRefType.any))
  addHelperImport(WasmFunctionName.stringLength, List(WasmRefType.any), List(WasmInt32))
  addHelperImport(WasmFunctionName.stringCharAt, List(WasmRefType.any, WasmInt32), List(WasmInt32))
  addHelperImport(WasmFunctionName.jsValueToString, List(anyref), List(WasmRefType.any))
  addHelperImport(WasmFunctionName.booleanToString, List(WasmInt32), List(WasmRefType.any))
  addHelperImport(WasmFunctionName.charToString, List(WasmInt32), List(WasmRefType.any))
  addHelperImport(WasmFunctionName.intToString, List(WasmInt32), List(WasmRefType.any))
  addHelperImport(WasmFunctionName.longToString, List(WasmInt64), List(WasmRefType.any))
  addHelperImport(WasmFunctionName.doubleToString, List(WasmFloat64), List(WasmRefType.any))
  addHelperImport(
    WasmFunctionName.stringConcat,
    List(WasmRefType.any, WasmRefType.any),
    List(WasmRefType.any)
  )
  addHelperImport(WasmFunctionName.isString, List(anyref), List(WasmInt32))

  addHelperImport(WasmFunctionName.jsValueType, List(WasmRefType.any), List(WasmInt32))
  addHelperImport(WasmFunctionName.jsValueHashCode, List(WasmRefType.any), List(WasmInt32))

  addHelperImport(WasmFunctionName.jsGlobalRefGet, List(WasmRefType.any), List(anyref))
  addHelperImport(WasmFunctionName.jsGlobalRefSet, List(WasmRefType.any, anyref), Nil)
  addHelperImport(WasmFunctionName.jsGlobalRefTypeof, List(WasmRefType.any), List(WasmRefType.any))
  addHelperImport(WasmFunctionName.jsNewArray, Nil, List(anyref))
  addHelperImport(WasmFunctionName.jsArrayPush, List(anyref, anyref), List(anyref))
  addHelperImport(
    WasmFunctionName.jsArraySpreadPush,
    List(anyref, anyref),
    List(anyref)
  )
  addHelperImport(WasmFunctionName.jsNewObject, Nil, List(anyref))
  addHelperImport(
    WasmFunctionName.jsObjectPush,
    List(anyref, anyref, anyref),
    List(anyref)
  )
  addHelperImport(WasmFunctionName.jsSelect, List(anyref, anyref), List(anyref))
  addHelperImport(WasmFunctionName.jsSelectSet, List(anyref, anyref, anyref), Nil)
  addHelperImport(WasmFunctionName.jsNew, List(anyref, anyref), List(anyref))
  addHelperImport(WasmFunctionName.jsFunctionApply, List(anyref, anyref), List(anyref))
  addHelperImport(
    WasmFunctionName.jsMethodApply,
    List(anyref, anyref, anyref),
    List(anyref)
  )
  addHelperImport(WasmFunctionName.jsImportCall, List(anyref), List(anyref))
  addHelperImport(WasmFunctionName.jsImportMeta, Nil, List(anyref))
  addHelperImport(WasmFunctionName.jsDelete, List(anyref, anyref), Nil)
  addHelperImport(WasmFunctionName.jsForInSimple, List(anyref, anyref), Nil)
  addHelperImport(WasmFunctionName.jsIsTruthy, List(anyref), List(WasmInt32))
  addHelperImport(WasmFunctionName.jsLinkingInfo, Nil, List(anyref))

  for ((op, name) <- WasmFunctionName.jsUnaryOps)
    addHelperImport(name, List(anyref), List(anyref))

  for ((op, name) <- WasmFunctionName.jsBinaryOps) {
    val resultType =
      if (op == IRTrees.JSBinaryOp.=== || op == IRTrees.JSBinaryOp.!==) WasmInt32
      else anyref
    addHelperImport(name, List(anyref, anyref), List(resultType))
  }

  addHelperImport(WasmFunctionName.newSymbol, Nil, List(anyref))
  addHelperImport(
    WasmFunctionName.createJSClass,
    List(anyref, anyref, WasmRefType.func, WasmRefType.func, WasmRefType.func),
    List(WasmRefType.any)
  )
  addHelperImport(
    WasmFunctionName.createJSClassRest,
    List(anyref, anyref, WasmRefType.func, WasmRefType.func, WasmRefType.func, WasmInt32),
    List(WasmRefType.any)
  )
  addHelperImport(
    WasmFunctionName.installJSField,
    List(anyref, anyref, anyref),
    Nil
  )
  addHelperImport(
    WasmFunctionName.installJSMethod,
    List(anyref, anyref, WasmInt32, anyref, WasmRefType.func, WasmInt32),
    Nil
  )
  addHelperImport(
    WasmFunctionName.installJSProperty,
    List(anyref, anyref, WasmInt32, anyref, WasmRefType.funcref, WasmRefType.funcref),
    Nil
  )
  addHelperImport(
    WasmFunctionName.jsSuperGet,
    List(anyref, anyref, anyref),
    List(anyref)
  )
  addHelperImport(
    WasmFunctionName.jsSuperSet,
    List(anyref, anyref, anyref, anyref),
    Nil
  )
  addHelperImport(
    WasmFunctionName.jsSuperCall,
    List(anyref, anyref, anyref, anyref),
    List(anyref)
  )

  def complete(
      moduleInitializers: List[ModuleInitializer.Initializer],
      classesWithStaticInit: List[IRNames.ClassName]
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
    module.addData(WasmData(WasmDataName.string, stringPool.toArray, WasmData.Mode.Passive))
    addGlobal(
      WasmGlobal(
        WasmGlobalName.stringLiteralCache,
        WasmRefType(WasmArrayTypeName.anyArray),
        WasmExpr(
          List(
            WasmInstr.I32_CONST(nextConstantStringIndex),
            WasmInstr.ARRAY_NEW_DEFAULT(WasmArrayTypeName.anyArray)
          )
        ),
        isMutable = false
      )
    )

    genStartFunction(moduleInitializers, classesWithStaticInit)
    genDeclarativeElements()
  }

  private def genStartFunction(
      moduleInitializers: List[ModuleInitializer.Initializer],
      classesWithStaticInit: List[IRNames.ClassName]
  ): Unit = {
    val fctx = WasmFunctionContext(WasmFunctionName.start, Nil, Nil)(this)

    import fctx.instrs

    // Initialize itables

    for ((name, globalName) <- classItableGlobals) {
      val classInfo = getClassInfo(name)
      val interfaces = classInfo.ancestors.map(getClassInfo(_)).filter(_.isInterface)
      val vtable = calculateVtableType(name)
      interfaces.foreach { iface =>
        val idx = getItableIdx(iface.name)
        instrs += WasmInstr.GLOBAL_GET(globalName)
        instrs += WasmInstr.I32_CONST(idx)

        iface.methods.foreach { method =>
          val func = vtable.resolve(method.name)
          instrs += WasmInstr.REF_FUNC(func.name)
        }
        instrs += WasmInstr.STRUCT_NEW(WasmTypeName.WasmStructTypeName.forITable(iface.name))
        instrs += WasmInstr.ARRAY_SET(WasmTypeName.WasmArrayTypeName.itables)
      }
    }

    // Initialize the JS private field symbols

    for (fieldName <- _jsPrivateFieldNames) {
      instrs += WasmInstr.CALL(WasmFunctionName.newSymbol)
      instrs += WasmInstr.GLOBAL_SET(WasmGlobalName.forJSPrivateField(fieldName))
    }

    // Emit the static initializers

    for (className <- classesWithStaticInit) {
      val funcName = WasmFunctionName(
        IRTrees.MemberNamespace.StaticConstructor,
        className,
        IRNames.StaticInitializerName
      )
      instrs += WasmInstr.CALL(funcName)
    }

    // Emit the module initializers

    moduleInitializers.foreach { init =>
      def genCallStatic(className: IRNames.ClassName, methodName: IRNames.MethodName): Unit = {
        val functionName =
          WasmFunctionName(IRTrees.MemberNamespace.PublicStatic, className, methodName)
        instrs += WasmInstr.CALL(functionName)
      }
      implicit val noPos: Position = Position.NoPosition

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

    if (instrs.nonEmpty) {
      fctx.buildAndAddToContext()
      module.setStartFunction(WasmFunctionName.start)
    }
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
      module.addElement(WasmElement(WasmRefType.funcref, exprs, WasmElement.Mode.Declarative))
    }
  }
}

object WasmContext {
  private val classFieldOffset = 2 // vtable, itables

  private val AncestorsOfHijackedClasses: Set[IRNames.ClassName] = {
    // We hard-code this for now, but ideally we should derive it
    IRNames.HijackedClasses ++
      Set(
        IRNames.ObjectClass,
        IRNames.SerializableClass,
        IRNames.ClassName("java.lang.CharSequence"),
        IRNames.ClassName("java.lang.Comparable"),
        IRNames.ClassName("java.lang.Number"),
        IRNames.ClassName("java.lang.constant.Constable"),
        IRNames.ClassName("java.lang.constant.ConstantDesc")
      )
  }

  final class WasmClassInfo(
      val name: IRNames.ClassName,
      val kind: ClassKind,
      val jsClassCaptures: Option[List[IRTrees.ParamDef]],
      private var _methods: List[WasmFunctionInfo],
      val allFieldDefs: List[IRTrees.FieldDef],
      val superClass: Option[IRNames.ClassName],
      val interfaces: List[IRNames.ClassName],
      val ancestors: List[IRNames.ClassName],
      private var _hasInstances: Boolean,
      val isAbstract: Boolean,
      val hasRuntimeTypeInfo: Boolean,
      val jsNativeLoadSpec: Option[IRTrees.JSNativeLoadSpec],
      val jsNativeMembers: Map[IRNames.MethodName, IRTrees.JSNativeLoadSpec]
  ) {
    private val fieldIdxByName: Map[IRNames.FieldName, Int] =
      allFieldDefs.map(_.name.name).zipWithIndex.map(p => p._1 -> (p._2 + classFieldOffset)).toMap

    // See caller in Preprocessor.preprocess
    def setHasInstances(): Unit =
      _hasInstances = true

    def hasInstances: Boolean = _hasInstances

    def isAncestorOfHijackedClass: Boolean = AncestorsOfHijackedClasses.contains(name)

    def isInterface = kind == ClassKind.Interface

    def methods: List[WasmFunctionInfo] = _methods

    def maybeAddAbstractMethod(methodName: IRNames.MethodName, ctx: WasmContext): Unit = {
      if (!methods.exists(_.name.simpleName == methodName.nameString)) {
        val wasmName = WasmFunctionName(IRTrees.MemberNamespace.Public, name, methodName)
        val argTypes = methodName.paramTypeRefs.map(ctx.inferTypeFromTypeRef(_))
        val resultType = ctx.inferTypeFromTypeRef(methodName.resultTypeRef)
        _methods = _methods :+ WasmFunctionInfo(wasmName, argTypes, resultType, isAbstract = true)
      }
    }

    @tailrec
    private def resolvePublicMethodOpt(
        methodName: IRNames.MethodName
    )(implicit ctx: ReadOnlyWasmContext): Option[IRNames.ClassName] = {
      if (methods.exists(_.name.simpleName == methodName.nameString)) {
        Some(name)
      } else {
        superClass match {
          case None =>
            None
          case Some(superClass) =>
            ctx.getClassInfo(superClass).resolvePublicMethodOpt(methodName)
        }
      }
    }

    def resolvePublicMethod(namespace: IRTrees.MemberNamespace, methodName: IRNames.MethodName)(
        implicit ctx: ReadOnlyWasmContext
    ): IRNames.ClassName = {
      if (isInterface || namespace != IRTrees.MemberNamespace.Public) {
        name
      } else {
        resolvePublicMethodOpt(methodName).getOrElse {
          throw new AssertionError(
            s"Cannot find method ${methodName.nameString} in class ${name.nameString}"
          )
        }
      }
    }

    def getFieldIdx(name: IRNames.FieldName): WasmImmediate.StructFieldIdx = {
      WasmImmediate.StructFieldIdx(
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

  case class WasmFunctionInfo(
      name: WasmFunctionName,
      argTypes: List[IRTypes.Type],
      resultType: IRTypes.Type,
      // flags: IRTrees.MemberFlags,
      isAbstract: Boolean
  ) {
    def toWasmFunctionType()(implicit ctx: TypeDefinableWasmContext): WasmFunctionType =
      TypeTransformer.transformFunctionType(this)

  }
  case class WasmFieldInfo(name: WasmFieldName, tpe: Types.WasmType)

  case class WasmVTable(val functions: List[WasmFunctionInfo]) {
    def resolve(name: WasmFunctionName): WasmFunctionInfo =
      functions
        .find(_.name.simpleName == name.simpleName)
        .getOrElse(throw new Error(s"Function not found: $name"))
    def resolveWithIdx(name: WasmFunctionName): (Int, WasmFunctionInfo) = {
      val idx = functions.indexWhere(_.name.simpleName == name.simpleName)
      if (idx < 0)
        throw new Error(s"Function not found: $name among ${functions.map(_.name.simpleName)}")
      else (idx, functions(idx))
    }
  }
}
