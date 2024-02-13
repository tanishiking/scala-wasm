package wasm.wasm4s

import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import wasm.converters.WasmTextWriter

object Names {
  // private[wasm4s] because we don't wanna access it from converters
  sealed abstract class WasmName(private[wasm4s] val name: String) {
    def show: String = {
      val suffix = this match {
        case _: WasmLocalName                         => "l"
        case _: WasmGlobalName.WasmModuleInstanceName => "g_instance"
        case _: WasmGlobalName.WasmGlobalVTableName   => "g_vtable"
        case _: WasmFunctionName                      => "fun"
        case _: WasmFunctionTypeName                  => "ty"
        case _: WasmFieldName                         => "f"
        case _: WasmExportName                        => "ex"
        case _: WasmGCTypeName.WasmStructTypeName     => "str"
        case _: WasmGCTypeName.WasmArrayTypeName      => "arr"
        case _: WasmGCTypeName.WasmVTableTypeName     => "ty_vtable"
      }
      s"${WasmName.sanitizeWatIdentifier(this.name)}__$suffix"
    }
  }
  object WasmName {

    /** @see https://webassembly.github.io/spec/core/text/values.html#text-id */
    def sanitizeWatIdentifier(indent: String): String =
      if (indent.isEmpty) "_"
      else if (indent.forall(isValidWatIdentifierChar)) indent
      else indent.map(c => if (isValidWatIdentifierChar(c)) c else '_').mkString

    private def isValidWatIdentifierChar(c: Char): Boolean =
      c.isDigit || c.isLetter ||
        "!#$%&'*+-./:<=>?@\\^_`|~".contains(c) ||
        "$.@_".contains(c)
  }

  final class WasmLocalName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmLocalName {
    def fromIR(name: IRNames.LocalName) = new WasmLocalName(name.nameString)
    def fromStr(str: String) = new WasmLocalName(str)
  }

  sealed abstract class WasmGlobalName(override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmGlobalName {
    final class WasmModuleInstanceName private (override private[wasm4s] val name: String)
        extends WasmGlobalName(name)
    object WasmModuleInstanceName {
      def fromIR(name: IRNames.ClassName): WasmModuleInstanceName = new WasmModuleInstanceName(
        name.nameString
      )
    }
    final class WasmGlobalVTableName private (override private[wasm4s] val name: String)
        extends WasmGlobalName(name)
    object WasmGlobalVTableName {
      def fromIR(name: IRNames.ClassName): WasmGlobalVTableName = new WasmGlobalVTableName(
        name.nameString
      )
    }
  }

  // final class WasmGlobalName private (val name: String) extends WasmName(name) {
  //     def apply(n: IRNames.LocalName): WasmLocalName = new WasmLocalName(n.nameString)
  // }

  final class WasmFunctionName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmFunctionName {
    def fromIR(name: IRNames.MethodName): WasmFunctionName = new WasmFunctionName(name.nameString)
    def fromLiteral(lit: IRTrees.StringLiteral): WasmFunctionName = new WasmFunctionName(lit.value)
  }

  final class WasmFunctionTypeName private (name: String) extends WasmName(name)
  object WasmFunctionTypeName {
    def fromIR(name: IRNames.MethodName) = new WasmFunctionTypeName(name.nameString)
    def fromLiteral(lit: IRTrees.StringLiteral): WasmFunctionTypeName = new WasmFunctionTypeName(
      lit.value
    )
  }

  final class WasmFieldName private (name: String) extends WasmName(name)
  object WasmFieldName {
    def fromIR(name: IRNames.FieldName) = new WasmFieldName(name.nameString)
    def fromIR(name: IRNames.MethodName) = new WasmFieldName(name.nameString)
    val vtable = new WasmFieldName("vtable")
  }

  // GC types ====
  sealed abstract class WasmGCTypeName(override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmGCTypeName {
    final class WasmStructTypeName private (override private[wasm4s] val name: String)
        extends WasmGCTypeName(name)
    object WasmStructTypeName {
      def fromIR(name: IRNames.ClassName) = new WasmStructTypeName(name.nameString)
    }

    /** Array type's name */
    final class WasmArrayTypeName private (override private[wasm4s] val name: String)
        extends WasmGCTypeName(name)
    object WasmArrayTypeName {
      def fromIR(ty: IRTypes.ArrayType) = {
        val ref = ty.arrayTypeRef
        // TODO: better naming?
        new WasmArrayTypeName(s"${ref.base.displayName}_${ref.dimensions}")
      }
    }

    /** Vtable type's name */
    final class WasmVTableTypeName private (override private[wasm4s] val name: String)
        extends WasmGCTypeName(name)
    object WasmVTableTypeName {
      def fromIR(ir: IRNames.ClassName) = new WasmVTableTypeName(ir.nameString)
    }

  }

  final class WasmExportName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmExportName {
    def fromStr(str: String) = new WasmExportName(str)
  }

}
