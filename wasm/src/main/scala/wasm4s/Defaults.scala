package wasm4s

import wasm.wasm4s.Types._
import wasm.wasm4s.WasmImmediate._
import wasm.wasm4s.WasmInstr
import wasm.wasm4s.WasmInstr._

object Defaults {
  private def nonDefaultable(t: WasmStorageType) = throw new Error(s"Non defaultable type: $t")
  def defaultValue(t: WasmStorageType): WasmInstr = t match {
    case WasmInt32                   => I32_CONST(I32(0))
    case WasmInt64                   => I64_CONST(I64(0))
    case WasmFloat32                 => F32_CONST(F32(0))
    case WasmFloat64                 => F64_CONST(F64(0))
    case WasmRefType(true, heapType) => REF_NULL(HeapType(heapType))
    case WasmRefType(false, _)       => nonDefaultable(t)
    case WasmInt8                    => nonDefaultable(t)
    case WasmInt16                   => nonDefaultable(t)
  }
}
