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
        case _: WasmLocalName                         => "local"
        case _: WasmGlobalName.WasmModuleInstanceName => "g_instance"
        case _: WasmGlobalName.WasmGlobalVTableName   => "g_vtable"
        case _: WasmFunctionName                      => "fun"
        case _: WasmFieldName                         => "field"
        case _: WasmExportName                        => "export"
        case _: WasmTypeName.WasmFunctionTypeName     => "ty"
        case _: WasmTypeName.WasmStructTypeName       => "struct"
        case _: WasmTypeName.WasmArrayTypeName        => "arr"
        case _: WasmTypeName.WasmVTableTypeName       => "vtable"
      }
      s"$$${WasmName.sanitizeWatIdentifier(this.name)}___$suffix"
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

  final case class WasmLocalName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmLocalName {
    def fromIR(name: IRNames.LocalName) = new WasmLocalName(name.nameString)
    def fromStr(str: String) = new WasmLocalName(str)
    def synthetic(id: Int) = new WasmLocalName(s"local___$id")
  }

  sealed abstract class WasmGlobalName(override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmGlobalName {
    final case class WasmModuleInstanceName private (override private[wasm4s] val name: String)
        extends WasmGlobalName(name)
    object WasmModuleInstanceName {
      def fromIR(name: IRNames.ClassName): WasmModuleInstanceName = new WasmModuleInstanceName(
        name.nameString
      )
    }
    final case class WasmGlobalVTableName private (override private[wasm4s] val name: String)
        extends WasmGlobalName(name)
    object WasmGlobalVTableName {
      def fromIR(name: IRNames.ClassName): WasmGlobalVTableName = new WasmGlobalVTableName(
        name.nameString
      )
    }
  }

  // final case class WasmGlobalName private (val name: String) extends WasmName(name) {
  //     def apply(n: IRNames.LocalName): WasmLocalName = new WasmLocalName(n.nameString)
  // }

  final case class WasmFunctionName private (
      val className: String,
      val methodName: String
  ) extends WasmName(s"$className#$methodName")
  object WasmFunctionName {
    def apply(clazz: IRNames.ClassName, method: IRNames.MethodName): WasmFunctionName =
      new WasmFunctionName(clazz.nameString, method.nameString)
    def apply(lit: IRTrees.StringLiteral): WasmFunctionName = new WasmFunctionName(lit.value, "")
  }

  final case class WasmFieldName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmFieldName {
    def apply(name: IRNames.FieldName) = new WasmFieldName(name.nameString)
    def fromIR(name: IRNames.MethodName) = new WasmFieldName(name.nameString)
    def fromFunction(name: WasmFunctionName) = new WasmFieldName(name.name)
    val vtable = new WasmFieldName("vtable")
  }

  // GC types ====
  sealed abstract class WasmTypeName(override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmTypeName {
    final case class WasmStructTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name)
    object WasmStructTypeName {
      def apply(name: IRNames.ClassName) = new WasmStructTypeName(name.nameString)
    }

    /** Array type's name */
    final case class WasmArrayTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name)
    object WasmArrayTypeName {
      def fromIR(ty: IRTypes.ArrayType) = {
        val ref = ty.arrayTypeRef
        // TODO: better naming?
        new WasmArrayTypeName(s"${ref.base.displayName}_${ref.dimensions}")
      }
    }

    final case class WasmFunctionTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name)
    object WasmFunctionTypeName {
      def fromIR(name: IRNames.MethodName) = new WasmFunctionTypeName(name.nameString)
      def fromLiteral(lit: IRTrees.StringLiteral): WasmFunctionTypeName = new WasmFunctionTypeName(
        lit.value
      )
    }

    /** Vtable type's name */
    final case class WasmVTableTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name)
    object WasmVTableTypeName {
      def fromIR(ir: IRNames.ClassName) = new WasmVTableTypeName(ir.nameString)
    }

  }

  final case class WasmExportName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmExportName {
    def fromStr(str: String) = new WasmExportName(str)
  }

}
