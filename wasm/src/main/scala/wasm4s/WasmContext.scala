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

  def calculateVtable(name: IRNames.ClassName): WasmVTable = {
    // def collectMethodsFromInterface(iface)
    def collectMethods(className: IRNames.ClassName): List[WasmFunctionInfo] = {
      val info = classInfo.getOrElse(className, throw new Error(s"Class not found: $className"))
      val fromSuperClass = info.superClass.map(collectMethods).getOrElse(Nil)
      val fromInterfaces = info.interfaces.flatMap(collectMethods)
      fromSuperClass ++ fromInterfaces ++ info.methods.filterNot(m => m.isAbstract)
    }

    vtablesCache.getOrElseUpdate(
      name, {
        val functions = collectMethods(name)
          .foldLeft(Array.empty[WasmFunctionInfo]) { case (acc, m) =>
            acc.indexWhere(_.name.methodName == m.name.methodName) match {
              case i if i < 0 => acc :+ m
              case i          => acc.updated(i, m)
            }
          }
          .toList
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

}

object WasmContext {
  private val classFieldOffset = 2 // vtable, itables
  case class WasmClassInfo(
      name: IRNames.ClassName,
      kind: ClassKind,
      methods: List[WasmFunctionInfo],
      private val fields: List[WasmFieldName],
      superClass: Option[IRNames.ClassName],
      interfaces: List[IRNames.ClassName]
  ) {

    def isInterface = kind == ClassKind.Interface

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
    def toVTableEntries(vtableTypeName: WasmTypeName): List[WasmInstr] = {
      functions.map { method =>
        WasmInstr.REF_FUNC(method.name)
      } :+ WasmInstr.STRUCT_NEW(vtableTypeName)
    }

  }
  // object WasmVTable {
  //   def apply(functions: List[WasmFunctionInfo]): WasmVTable =
  //       new WasmVTable(functions.map(f => f.name.methodName -> f).toMap)
  // }
  // type WasmVTable = List[WasmFunctionInfo]
}
