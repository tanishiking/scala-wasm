package wasm4s

import scala.collection.mutable

case class WasmContext() {
  val functionTypes = new WasmSymbolTable[WasmFunctionType]()
  val gcTypes = new WasmSymbolTable[WasmGCTypeDefinition]()
  val functions = new WasmSymbolTable[WasmFunction]()
}