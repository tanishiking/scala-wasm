package wasm.wasm4s

class WasmFunctionContext(val receiver: WasmLocal) {
  val locals = new WasmSymbolTable[WasmLocal]()
}
