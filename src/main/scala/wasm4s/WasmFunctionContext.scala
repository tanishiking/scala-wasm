package wasm4s

class WasmFunctionContext {
  val locals = new WasmSymbolTable[WasmLocal]()
}
