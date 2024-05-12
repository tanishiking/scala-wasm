package org.scalajs.linker.backend.webassembly

import org.scalajs.ir.Position

import Instructions._
import Names._
import Types._

/** WebAssembly modules and their structure.
  *
  * @see
  *   [[https://webassembly.github.io/gc/core/syntax/modules.html]]
  */
object Modules {
  sealed case class WasmExpr(instr: List[WasmInstr])

  sealed abstract class WasmExport {
    val exportName: String
  }

  object WasmExport {
    final case class Function(exportName: String, funcName: WasmFunctionName) extends WasmExport
    final case class Global(exportName: String, globalName: WasmGlobalName) extends WasmExport
  }

  final case class WasmImport(module: String, name: String, desc: WasmImportDesc)

  sealed abstract class WasmImportDesc

  object WasmImportDesc {
    final case class Func(id: WasmFunctionName, typeName: WasmTypeName) extends WasmImportDesc
    final case class Global(id: WasmGlobalName, typ: WasmType, isMutable: Boolean)
        extends WasmImportDesc
    final case class Tag(id: WasmTagName, typeName: WasmTypeName) extends WasmImportDesc
  }

  /** @see
    *   https://webassembly.github.io/spec/core/syntax/modules.html#functions
    */
  final case class WasmFunction(
      val name: WasmFunctionName,
      val typeName: WasmTypeName,
      val locals: List[WasmLocal],
      val results: List[WasmType],
      val body: WasmExpr,
      val pos: Position
  )

  /** The index space for locals is only accessible inside a function and includes the parameters of
    * that function, which precede the local variables.
    */
  case class WasmLocal(
      val name: WasmLocalName,
      val typ: WasmType,
      val isParameter: Boolean // for text
  )

  final case class WasmTag(val name: WasmTagName, val typ: WasmTypeName)

  final case class WasmData(val name: WasmDataName, val bytes: Array[Byte], mode: WasmData.Mode)

  object WasmData {
    sealed abstract class Mode
    object Mode {
      case object Passive extends Mode
      // final case class Active
    }
  }

  final case class WasmGlobal(
      val name: WasmGlobalName,
      val typ: WasmType,
      val init: WasmExpr,
      val isMutable: Boolean
  )

  final case class WasmRecType(subTypes: List[WasmSubType])

  object WasmRecType {

    /** Builds a `rectype` with a single `subtype`. */
    def apply(singleSubType: WasmSubType): WasmRecType =
      WasmRecType(singleSubType :: Nil)
  }

  final case class WasmSubType(
      name: WasmTypeName,
      isFinal: Boolean,
      superType: Option[WasmTypeName],
      compositeType: WasmCompositeType
  )

  object WasmSubType {

    /** Builds a `subtype` that is `final` and without any super type. */
    def apply(name: WasmTypeName, compositeType: WasmCompositeType): WasmSubType =
      WasmSubType(name, isFinal = true, superType = None, compositeType)
  }

  sealed abstract class WasmCompositeType

  final case class WasmFunctionSignature(
      params: List[WasmType],
      results: List[WasmType]
  )
  object WasmFunctionSignature {
    val NilToNil: WasmFunctionSignature = WasmFunctionSignature(Nil, Nil)
  }

  final case class WasmFunctionType(
      params: List[WasmType],
      results: List[WasmType]
  ) extends WasmCompositeType
  object WasmFunctionType {
    def apply(sig: WasmFunctionSignature): WasmFunctionType = {
      WasmFunctionType(sig.params, sig.results)
    }
  }

  final case class WasmStructType(fields: List[WasmStructField]) extends WasmCompositeType

  final case class WasmArrayType(fieldType: WasmFieldType) extends WasmCompositeType

  final case class WasmFieldType(typ: WasmStorageType, isMutable: Boolean)

  final case class WasmStructField(name: WasmFieldName, fieldType: WasmFieldType)

  object WasmStructField {
    def apply(name: WasmFieldName, typ: WasmStorageType, isMutable: Boolean): WasmStructField =
      WasmStructField(name, WasmFieldType(typ, isMutable))
  }

  final case class WasmElement(typ: WasmType, init: List[WasmExpr], mode: WasmElement.Mode)

  object WasmElement {
    sealed abstract class Mode

    object Mode {
      case object Passive extends Mode
      // final case class Active(table: WasmImmediate.TableIdx, offset: WasmExpr) extends Mode
      case object Declarative extends Mode
    }
  }

  /** @see
    *   https://webassembly.github.io/spec/core/syntax/modules.html#modules
    */
  final class WasmModule(
      val types: List[WasmRecType],
      val imports: List[WasmImport],
      val funcs: List[WasmFunction],
      val tags: List[WasmTag],
      val globals: List[WasmGlobal],
      val exports: List[WasmExport],
      val start: Option[WasmFunctionName],
      val elems: List[WasmElement],
      val datas: List[WasmData]
  )
}
