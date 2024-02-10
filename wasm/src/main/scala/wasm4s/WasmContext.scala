package wasm.wasm4s

import scala.collection.mutable

import Names._

case class WasmContext() {
  val functionTypes = new WasmSymbolTable[WasmFunctionTypeName, WasmFunctionType]()
  val gcTypes = new WasmSymbolTable[WasmGCTypeName, WasmGCTypeDefinition]()
  val functions = new WasmSymbolTable[WasmFunctionName, WasmFunction]()
  val globals = new WasmSymbolTable[WasmGlobalName, WasmGlobal]()
}