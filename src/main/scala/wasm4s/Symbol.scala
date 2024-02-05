package wasm4s

import scala.collection.mutable

/** https://webassembly.github.io/spec/core/syntax/modules.html#indices */
trait WasmDefinitionField {
  val idx: Long = 0L
}
trait WasmNamedDefinitionField extends WasmDefinitionField {
  val ident: Ident
}

case class WasmSymbol[T <: WasmNamedDefinitionField](ident: Ident) {
  override def toString(): String = ident.name
}

class WasmSymbolTable[T <: WasmNamedDefinitionField] {
  private val unbound = mutable.Map[Ident, WasmSymbol[T]]()
  private val defined = mutable.Map[WasmSymbol[T], T]()

  def reference(ident: Ident): WasmSymbol[T] =
    unbound.getOrElseUpdate(ident, new WasmSymbol[T](ident))

  def define(field: T): WasmSymbol[T] = {
    val sym = unbound.getOrElseUpdate(field.ident, new WasmSymbol[T](field.ident))
    defined.get(sym) match {
      case Some(f) => throw new Exception(s"Symbol ${field.ident} is already defined")
      case None    => defined.update(sym, field)
    }
    sym
  }
  def resolve(sym: WasmSymbol[T]): T =
    defined.getOrElse(sym, throw new Exception(s"Symbol ${sym.ident} is not defined"))
}
