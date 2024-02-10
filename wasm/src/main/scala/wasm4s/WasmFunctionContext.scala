package wasm.wasm4s

import Names.WasmLocalName

class WasmFunctionContext private (private val _receiver: Option[WasmLocal]) {
  val locals = new WasmSymbolTable[WasmLocalName, WasmLocal]()
  def receiver = _receiver.getOrElse(throw new Error("Can access to the receiver in this context."))
}

object WasmFunctionContext {
  def apply(): WasmFunctionContext = new WasmFunctionContext(None)
  def apply(receiver: WasmLocal): WasmFunctionContext = new WasmFunctionContext(Some(receiver))
}
