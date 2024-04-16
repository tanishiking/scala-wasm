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
