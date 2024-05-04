package wasm.wasm4s

object Names {
  sealed abstract class WasmName {
    val name: String
  }

  final case class WasmLocalName(name: String) extends WasmName

  final case class WasmLabelName(name: String) extends WasmName

  final case class WasmGlobalName(name: String) extends WasmName

  final case class WasmFunctionName(name: String) extends WasmName

  final case class WasmFieldName(name: String) extends WasmName

  final case class WasmFieldIdx(value: Int)

  final case class WasmTypeName(name: String) extends WasmName

  final case class WasmTagName(name: String) extends WasmName

  final case class WasmDataName(name: String) extends WasmName

  final case class WasmExportName(name: String) extends WasmName

}
