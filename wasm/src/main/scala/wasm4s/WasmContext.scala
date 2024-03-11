package wasm.wasm4s

import scala.collection.mutable

import Names._
import Names.WasmTypeName._
import Types._

import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.ClassKind

import scala.collection.mutable.LinkedHashMap
import wasm.ir2wasm.TypeTransformer

trait ReadOnlyWasmContext {
  import WasmContext._
  protected val gcTypes = new WasmSymbolTable[WasmTypeName, WasmGCTypeDefinition]()
  protected val functions = new WasmSymbolTable[WasmFunctionName, WasmFunction]()
  protected val globals = new WasmSymbolTable[WasmGlobalName, WasmGlobal]()

  protected val classInfo = mutable.Map[IRNames.ClassName, WasmClassInfo]()
  private val vtablesCache = mutable.Map[IRNames.ClassName, WasmVTable]()
  private val itablesCache = mutable.Map[IRNames.ClassName, WasmClassItables]()

  def getClassInfo(name: IRNames.ClassName): WasmClassInfo =
    classInfo.getOrElse(name, throw new Error(s"Class not found: $name"))

  def inferTypeFromTypeRef(typeRef: IRTypes.TypeRef): IRTypes.Type = typeRef match {
    case IRTypes.PrimRef(tpe) =>
      tpe
    case IRTypes.ClassRef(className) =>
      if (className == IRNames.ObjectClass) IRTypes.AnyType
      else IRTypes.ClassType(className)
    case typeRef: IRTypes.ArrayTypeRef =>
      IRTypes.ArrayType(typeRef)
  }

  /** Collects all methods declared, inherited, and mixed-in by the given class, super-class, and
    * interfaces.
    *
    * @param className
    *   class to collect methods from
    * @param includeAbstractMethods
    *   whether to include abstract methods
    * @return
    *   list of methods in order that "collectMethods(superClass) ++ methods from interfaces ++
    *   methods from the class"
    */
  private def collectMethods(
      className: IRNames.ClassName,
      includeAbstractMethods: Boolean
  ): List[WasmFunctionInfo] = {
    val info = classInfo.getOrElse(className, throw new Error(s"Class not found: $className"))
    val fromSuperClass =
      info.superClass.map(collectMethods(_, includeAbstractMethods)).getOrElse(Nil)
    val fromInterfaces = info.interfaces.flatMap(collectMethods(_, includeAbstractMethods))
    fromSuperClass ++ fromInterfaces ++
      (if (includeAbstractMethods) info.methods
       else info.methods.filterNot(_.isAbstract))
  }

  private def calculateVtable(
      name: IRNames.ClassName,
      includeAbstractMethods: Boolean
  ): List[WasmFunctionInfo] = {
    collectMethods(name, includeAbstractMethods)
      .foldLeft(Array.empty[WasmFunctionInfo]) { case (acc, m) =>
        acc.indexWhere(_.name.methodName == m.name.methodName) match {
          case i if i < 0 => acc :+ m
          case i          => acc.updated(i, m)
        }
      }
      .toList
  }

  def calculateGlobalVTable(name: IRNames.ClassName): List[WasmFunctionInfo] =
    // Do not include abstract methods when calculating vtable instance,
    // all slots should be filled with the function reference to the concrete methods
    calculateVtable(name, includeAbstractMethods = false)

  def calculateVtableType(name: IRNames.ClassName): WasmVTable = {
    vtablesCache.getOrElseUpdate(
      name, {
        val functions = calculateVtable(name, includeAbstractMethods = true)
        WasmVTable(functions)
      }
    )
  }

  def calculateClassItables(clazz: IRNames.ClassName): WasmClassItables = {
    def collectInterfaces(info: WasmClassInfo): List[WasmClassInfo] = {
      val superInterfaces =
        info.superClass.map(s => collectInterfaces(getClassInfo(s))).getOrElse(Nil)
      val ifaces = info.interfaces.flatMap { iface =>
        collectInterfaces(getClassInfo(iface))
      }

      if (info.isInterface) superInterfaces ++ ifaces :+ info
      else superInterfaces ++ ifaces
    }

    itablesCache.getOrElseUpdate(clazz, WasmClassItables(collectInterfaces(getClassInfo(clazz))))
  }
}

trait FunctionTypeWriterWasmContext extends ReadOnlyWasmContext { this: WasmContext =>
  protected val functionSignatures = LinkedHashMap.empty[WasmFunctionSignature, Int]

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
}

class WasmContext(val module: WasmModule) extends FunctionTypeWriterWasmContext {
  import WasmContext._
  def addExport(export: WasmExport[_]): Unit = module.addExport(export)
  def addFunction(fun: WasmFunction): Unit = {
    module.addFunction(fun)
    functions.define(fun)
  }
  def addGCType(ty: WasmStructType): Unit = {
    module.addRecGroupType(ty)
    gcTypes.define(ty)
  }
  def addGlobal(g: WasmGlobal): Unit = {
    module.addGlobal(g)
    globals.define(g)
  }

  def putClassInfo(name: IRNames.ClassName, info: WasmClassInfo): Unit =
    classInfo.put(name, info)

  addGlobal(
    WasmGlobal(
      WasmGlobalName.WasmUndefName,
      TypeTransformer.transformType(IRTypes.UndefType)(this),
      WasmExpr(
        List(WasmInstr.STRUCT_NEW(WasmImmediate.TypeIdx(WasmTypeName.WasmStructTypeName.undef)))
      ),
      isMutable = false
    )
  )
}

object WasmContext {
  private val classFieldOffset = 2 // vtable, itables
  final class WasmClassInfo(
      val name: IRNames.ClassName,
      val kind: ClassKind,
      private var _methods: List[WasmFunctionInfo],
      private val fields: List[WasmFieldName],
      val superClass: Option[IRNames.ClassName],
      val interfaces: List[IRNames.ClassName],
      val ancestors: List[IRNames.ClassName]
  ) {

    def isInterface = kind == ClassKind.Interface

    def methods: List[WasmFunctionInfo] = _methods

    def maybeAddAbstractMethod(methodName: IRNames.MethodName, ctx: WasmContext): Unit = {
      if (!methods.exists(_.name.methodName == methodName.nameString)) {
        val wasmName = WasmFunctionName(name, methodName)
        val argTypes = methodName.paramTypeRefs.map(ctx.inferTypeFromTypeRef(_))
        val resultType = ctx.inferTypeFromTypeRef(methodName.resultTypeRef)
        _methods = _methods :+ WasmFunctionInfo(wasmName, argTypes, resultType, isAbstract = true)
      }
    }

    def getMethodInfo(methodName: IRNames.MethodName): WasmFunctionInfo = {
      methods.find(_.name.methodName == methodName.nameString).getOrElse {
        throw new IllegalArgumentException(
          s"Cannot find method ${methodName.nameString} in class ${name.nameString}"
        )
      }
    }

    def getFieldIdx(name: WasmFieldName): WasmImmediate.StructFieldIdx =
      fields.indexWhere(_ == name) match {
        case i if i < 0 => throw new Error(s"Field not found: $name")
        case i          => WasmImmediate.StructFieldIdx(i + classFieldOffset)
      }
  }

  case class WasmFunctionInfo(
      name: WasmFunctionName,
      argTypes: List[IRTypes.Type],
      resultType: IRTypes.Type,
      // flags: IRTrees.MemberFlags,
      isAbstract: Boolean
  ) {
    def toWasmFunctionType()(implicit ctx: FunctionTypeWriterWasmContext): WasmFunctionType =
      TypeTransformer.transformFunctionType(this)

  }
  case class WasmFieldInfo(name: WasmFieldName, tpe: Types.WasmType)

  /** itables in order that super class's interface -> interfaces
    */
  case class WasmClassItables(val itables: List[WasmClassInfo]) {
    def isEmpty = itables.isEmpty
    // def resolveWithIdx(name: IRNames.ClassName): (Int, WasmClassInfo) = {
    //   val idx = itables.indexWhere(_.name == name)
    //   if (idx < 0) throw new Error(s"itable not found: $name")
    //   else (idx, itables(idx))
    // }
    /** @param name
      *   method name to find
      * @return
      *   (itableIdx, methodIdx) where itableIdx is the index of the interface in itables and
      *   methodIdx is the index of the method in that
      */
    def resolveMethod(name: IRNames.MethodName): (Int, Int) = {
      var foundMethodIdx = -1
      val itableIdx =
        itables.lastIndexWhere { classInfo =>
          val methodIdx = classInfo.methods.lastIndexWhere { func =>
            func.name.methodName == name.nameString
          }
          if (methodIdx >= 0) {
            foundMethodIdx = methodIdx
            true
          } else false
        }
      if (itableIdx >= 0)
        (itableIdx, foundMethodIdx)
      else throw new Error(s"Method not found: $name")
    }
  }
  case class WasmVTable(val functions: List[WasmFunctionInfo]) {
    def resolve(name: WasmFunctionName): WasmFunctionInfo =
      functions
        .find(_.name.methodName == name.methodName)
        .getOrElse(throw new Error(s"Function not found: $name"))
    def resolveWithIdx(name: WasmFunctionName): (Int, WasmFunctionInfo) = {
      val idx = functions.indexWhere(_.name.methodName == name.methodName)
      if (idx < 0) throw new Error(s"Function not found: $name")
      else (idx, functions(idx))
    }
  }
}
