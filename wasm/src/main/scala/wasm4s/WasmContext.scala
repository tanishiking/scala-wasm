package wasm.wasm4s

import scala.collection.mutable

import Names._
import Names.WasmTypeName._

import org.scalajs.ir.{Names => IRNames}

case class WasmContext() {
  import WasmContext._
  val functionTypes = new WasmSymbolTable[WasmFunctionTypeName, WasmFunctionType]()
  val gcTypes = new WasmSymbolTable[WasmTypeName, WasmGCTypeDefinition]()
  val functions = new WasmSymbolTable[WasmFunctionName, WasmFunction]()
  val globals = new WasmSymbolTable[WasmGlobalName, WasmGlobal]()

  // ClassName -> functionNames and type
  private val vtables = mutable.Map[IRNames.ClassName, WasmVTable]()

  val classInfo = mutable.Map[IRNames.ClassName, WasmClassInfo]()

  def getClassinfo(name: IRNames.ClassName): WasmClassInfo =
    classInfo.getOrElse(name, throw new Error(s"Class not found: $name"))

  def getVtable(name: IRNames.ClassName): WasmVTable = vtables.getOrElseUpdate(
    name, {
      collectMethods(name, mutable.ListBuffer.empty)
        .foldLeft(Array.empty[WasmFunctionInfo]) { case (acc, m) =>
          acc.indexWhere(_.name.methodName == m.name.methodName) match {
            case i if i < 0 => acc :+ m
            case i          => acc.updated(i, m)
          }
        }
        .toList
    }
  )

  private def collectMethods(
      className: IRNames.ClassName,
      buf: mutable.ListBuffer[WasmFunctionInfo]
  ): List[WasmFunctionInfo] = {
    val info = classInfo.getOrElse(className, throw new Error(s"Class not found: $className"))
    buf.prependAll(info.methods)
    info.superClass match {
      case None                                          => buf.toList
      case Some(s) if s.nameString == "java.lang.Object" => buf.toList
      case Some(s)                                       => collectMethods(s, buf)
    }
  }

}

object WasmContext {
  private val classFieldOffset = 1 // vtable
  case class WasmClassInfo(
      methods: List[WasmFunctionInfo],
      private val fields: List[WasmFieldName],
      superClass: Option[IRNames.ClassName]
  ) {

    def getFieldIdx(name: WasmFieldName): WasmImmediate.StructFieldIdx =
      fields.indexWhere(_ == name) match {
        case i if i < 0 => throw new Error(s"Field not found: $name")
        case i          => WasmImmediate.StructFieldIdx(i + classFieldOffset)
      }
  }

  case class WasmFunctionInfo(name: WasmFunctionName, tpe: WasmFunctionType)
  case class WasmFieldInfo(name: WasmFieldName, tpe: Types.WasmType)
  type WasmVTable = List[WasmFunctionInfo]
}
