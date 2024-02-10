package wasm.wasm4s

class WasmFunctionContext private (private val _receiver: Option[WasmLocal]) {
  val locals = new WasmSymbolTable[WasmLocal]()
  def receiver = _receiver.getOrElse(throw new Error("Can access to the receiver in this context."))
}

object WasmFunctionContext {
  def apply(): WasmFunctionContext = new WasmFunctionContext(None)
  def apply(receiver: WasmLocal): WasmFunctionContext = new WasmFunctionContext(Some(receiver))
}
