package wasm4s

import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{Types => IRTypes}

object Names {
  sealed abstract class WasmName(val name: String)
  final class WasmLocalName private (override val name: String) extends WasmName(name)
  object WasmLocalName {
    def fromIR(name: IRNames.LocalName) = new WasmLocalName(name.nameString)
    def fromStr(str: String) = new WasmLocalName(str)
  }

  final class WasmGlobalName private (override val name: String) extends WasmName(name)
  object WasmGlobalName {
    def forModuleClassInstance(name: IRNames.ClassName) = new WasmGlobalName(
      s"${name.nameString}_instance"
    )
  }

  // final class WasmGlobalName private (val name: String) extends WasmName(name) {
  //     def apply(n: IRNames.LocalName): WasmLocalName = new WasmLocalName(n.nameString)
  // }

  final class WasmFunctionName private (override val name: String) extends WasmName(name)
  object WasmFunctionName {
    def fromIR(name: IRNames.MethodName): WasmFunctionName = new WasmFunctionName(name.nameString)
  }

  final class WasmFunctionTypeName private (override val name: String) extends WasmName(name)
  object WasmFunctionTypeName {
    def fromIR(name: IRNames.MethodName) = new WasmFunctionTypeName(name.nameString)
  }

  final class WasmFieldName private (override val name: String) extends WasmName(name)
  object WasmFieldName {
    def fromIR(name: IRNames.FieldName) = new WasmFieldName(name.nameString)
  }

  final class WasmGCTypeName private (override val name: String) extends WasmName(name)
  object WasmGCTypeName {
    def fromIR(name: IRNames.ClassName) = new WasmGCTypeName(name.nameString)
    def fromIR(ty: IRTypes.ArrayType) = {
      val ref = ty.arrayTypeRef
      // TODO: better naming?
      new WasmGCTypeName(s"${ref.base.displayName}_${ref.dimensions}___array")
    }
  }
}
