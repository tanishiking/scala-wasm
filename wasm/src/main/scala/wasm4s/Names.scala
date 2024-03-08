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
        case _: WasmGlobalName.WasmGlobalITableName   => "g_itable"
        case _: WasmFunctionName                      => "fun"
        case _: WasmFieldName                         => "field"
        case _: WasmExportName                        => "export"
        case _: WasmTypeName.WasmFunctionTypeName     => "ty"
        case _: WasmTypeName.WasmStructTypeName       => "struct"
        case _: WasmTypeName.WasmArrayTypeName        => "arr"
        case _: WasmTypeName.WasmVTableTypeName       => "vtable"
        case _: WasmTypeName.WasmITableTypeName       => "itable"
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
    val receiver = new WasmLocalName("___<this>")
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
      def apply(name: IRNames.ClassName): WasmGlobalVTableName = new WasmGlobalVTableName(
        name.nameString
      )
      def apply(name: WasmTypeName): WasmGlobalVTableName = new WasmGlobalVTableName(
        name.name
      )
    }

    final case class WasmGlobalITableName private (override private[wasm4s] val name: String)
        extends WasmGlobalName(name)
    object WasmGlobalITableName {
      def apply(name: IRNames.ClassName): WasmGlobalITableName = new WasmGlobalITableName(
        name.nameString
      )
    }
  }

  // final case class WasmGlobalName private (val name: String) extends WasmName(name) {
  //     def apply(n: IRNames.LocalName): WasmLocalName = new WasmLocalName(n.nameString)
  // }

  case class WasmFunctionName private (
      val className: String,
      val methodName: String
  ) extends WasmName(s"$className#$methodName")
  object WasmFunctionName {
    def apply(clazz: IRNames.ClassName, method: IRNames.MethodName): WasmFunctionName =
      new WasmFunctionName(clazz.nameString, method.nameString)
    def apply(lit: IRTrees.StringLiteral): WasmFunctionName = new WasmFunctionName(lit.value, "")

    // Adding prefix __ to avoid name clashes with user code.
    // It should be safe not to add prefix to the method name
    // since loadModule is a static method and it's not registered in the vtable.
    def loadModule(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName(s"__${clazz.nameString}", "loadModule")
    def newDefault(clazz: IRNames.ClassName): WasmFunctionName =
      new WasmFunctionName(s"__${clazz.nameString}", "newDefault")

    private def helper(name: String): WasmFunctionName =
      new WasmFunctionName("__scalaJSHelpers", name)

    val is = helper("is")

    val undef = helper("undef")
    val isUndef = helper("isUndef")

    def box(primRef: IRTypes.PrimRef): WasmFunctionName = helper("b" + primRef.charCode)
    def unbox(primRef: IRTypes.PrimRef): WasmFunctionName = helper("u" + primRef.charCode)
    def unboxOrNull(primRef: IRTypes.PrimRef): WasmFunctionName = helper("uN" + primRef.charCode)
    def typeTest(primRef: IRTypes.PrimRef): WasmFunctionName = helper("t" + primRef.charCode)

    val emptyString = helper("emptyString")
    val stringLength = helper("stringLength")
    val stringCharAt = helper("stringCharAt")
    val jsValueToString = helper("jsValueToString")
    val booleanToString = helper("booleanToString")
    val charToString = helper("charToString")
    val intToString = helper("intToString")
    val doubleToString = helper("doubleToString")
    val stringConcat = helper("stringConcat")
    val isString = helper("isString")
  }

  final case class WasmFieldName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmFieldName {
    def apply(name: IRNames.FieldName) = new WasmFieldName(name.nameString)

    /** For class itable fields, each fields point to an itable of the interfaces */
    def apply(name: WasmTypeName.WasmITableTypeName) = new WasmFieldName(name.name)
    def apply(name: IRNames.MethodName) = new WasmFieldName(name.nameString)
    def apply(name: WasmFunctionName) = new WasmFieldName(name.name)
    val vtable = new WasmFieldName("vtable")
    val itable = new WasmFieldName("itable")
    val itables = new WasmFieldName("itables")
  }

  // GC types ====
  sealed abstract class WasmTypeName(override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmTypeName {
    final case class WasmStructTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name) {
      def toIRName = IRNames.ClassName(name)
    }
    object WasmStructTypeName {
      def apply(name: IRNames.ClassName) = new WasmStructTypeName(name.nameString)
    }

    /** Array type's name */
    final case class WasmArrayTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name)
    object WasmArrayTypeName {
      def apply(ty: IRTypes.ArrayType) = {
        val ref = ty.arrayTypeRef
        // TODO: better naming?
        new WasmArrayTypeName(s"${ref.base.displayName}_${ref.dimensions}")
      }
      val itables = new WasmArrayTypeName("itable")
    }

    final case class WasmFunctionTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name)
    object WasmFunctionTypeName {
      def apply(idx: Int) = new WasmFunctionTypeName(s"fun_type___$idx")
    }

    /** Vtable type's name */
    final case class WasmVTableTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name)
    object WasmVTableTypeName {
      def apply(ir: IRNames.ClassName) = new WasmVTableTypeName(ir.nameString)
    }

    final case class WasmITableTypeName private (override private[wasm4s] val name: String)
        extends WasmTypeName(name)
    object WasmITableTypeName {
      def apply(ir: IRNames.ClassName) = new WasmITableTypeName(ir.nameString)
    }

  }

  final case class WasmExportName private (override private[wasm4s] val name: String)
      extends WasmName(name)
  object WasmExportName {
    def fromStr(str: String) = new WasmExportName(str)
  }

}
