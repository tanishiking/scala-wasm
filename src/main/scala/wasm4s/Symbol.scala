package wasm4s

import scala.collection.mutable

import Names._

/** https://webassembly.github.io/spec/core/syntax/modules.html#indices */
// trait WasmDefinitionField {
//   val idx: Long = 0L
// }
trait WasmNamedDefinitionField[+N <: WasmName] {
  // type NameType = N
  val name: N
}

// case class WasmSymbol[T <: WasmNamedDefinitionField](ident: Ident) {
//   override def toString(): String = ident.name
// }

class WasmSymbolTable[T <: WasmNamedDefinitionField[WasmName]] {
  // private val unbound = mutable.Map[Ident, WasmSymbol[T]]()
  // private val defined = mutable.Map[WasmSymbol[T], T]()
  private val defined = mutable.Map[WasmName, T]()

  // def reference(ident: Ident): WasmSymbol[T] =
  //   unbound.getOrElseUpdate(ident, new WasmSymbol[T](ident))

  def define(field: T): Unit =
    defined.get(field.name) match {
      case Some(f) => throw new Exception(s"Symbol ${field.name} is already defined")
      case None    => defined.update(field.name, field)
    }

  def resolve(name: WasmName): T =
    defined.getOrElse(name, throw new Exception(s"Symbol ${name} is not defined"))
}
