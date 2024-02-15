package wasm.wasm4s

import scala.collection.mutable

import Names._

/** https://webassembly.github.io/spec/core/syntax/modules.html#indices */
// trait WasmDefinitionField {
//   val idx: Long = 0L
// }
trait WasmNamedDefinitionField[N <: WasmName] {
  val name: N
}

// case class WasmSymbol[T <: WasmNamedDefinitionField](ident: Ident) {
//   override def toString(): String = ident.name
// }

class WasmSymbolTable[N <: WasmName, T <: WasmNamedDefinitionField[N]] {
  // private val unbound = mutable.Map[Ident, WasmSymbol[T]]()
  // private val defined = mutable.Map[WasmSymbol[T], T]()
  // TODO: should keep the order
  private val defined = mutable.Map[N, T]()

  // def reference(ident: Ident): WasmSymbol[T] =
  //   unbound.getOrElseUpdate(ident, new WasmSymbol[T](ident))

  def define(field: T): Unit =
    defined.get(field.name) match {
      case Some(f) if f.name.isInstanceOf[WasmTypeName.WasmFunctionTypeName] =>
      case Some(f) => throw new Exception(s"Symbol ${field.name} is already defined")
      case None    => defined.update(field.name, field)
    }

  def resolve(name: N): T =
    defined.getOrElse(name, throw new Exception(s"Symbol ${name} is not defined"))
  def all: List[T] = defined.values.toList
}
