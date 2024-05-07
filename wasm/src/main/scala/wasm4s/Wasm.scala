package wasm.wasm4s

import scala.collection.mutable

import org.scalajs.ir.Position

import wasm.ir2wasm.VarGen._

import Types._
import Names._
import Names.WasmTypeName._

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
case class WasmFunction(
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

case class WasmGlobal(
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

case class WasmFunctionSignature(
    params: List[WasmType],
    results: List[WasmType]
)
object WasmFunctionSignature {
  val NilToNil: WasmFunctionSignature = WasmFunctionSignature(Nil, Nil)
}

case class WasmFunctionType(
    params: List[WasmType],
    results: List[WasmType]
) extends WasmCompositeType
object WasmFunctionType {
  def apply(sig: WasmFunctionSignature): WasmFunctionType = {
    WasmFunctionType(sig.params, sig.results)
  }
}

case class WasmStructType(fields: List[WasmStructField]) extends WasmCompositeType
object WasmStructType {

  /** Run-time type data of a `TypeRef`. Support for `j.l.Class` methods and other reflective
    * operations.
    *
    * @see
    *   [[Names.genFieldName.typeData]], which contains documentation of what is in each field.
    */
  def typeData(implicit ctx: wasm.ir2wasm.ReadOnlyWasmContext): WasmStructType = WasmStructType(
    List(
      WasmStructField(
        genFieldName.typeData.nameOffset,
        WasmInt32,
        isMutable = false
      ),
      WasmStructField(
        genFieldName.typeData.nameSize,
        WasmInt32,
        isMutable = false
      ),
      WasmStructField(
        genFieldName.typeData.nameStringIndex,
        WasmInt32,
        isMutable = false
      ),
      WasmStructField(
        genFieldName.typeData.kind,
        WasmInt32,
        isMutable = false
      ),
      WasmStructField(
        genFieldName.typeData.specialInstanceTypes,
        WasmInt32,
        isMutable = false
      ),
      WasmStructField(
        genFieldName.typeData.strictAncestors,
        WasmRefType.nullable(genTypeName.typeDataArray),
        isMutable = false
      ),
      WasmStructField(
        genFieldName.typeData.componentType,
        WasmRefType.nullable(genTypeName.typeData),
        isMutable = false
      ),
      WasmStructField(
        genFieldName.typeData.name,
        WasmRefType.anyref,
        isMutable = true
      ),
      WasmStructField(
        genFieldName.typeData.classOfValue,
        WasmRefType.nullable(WasmHeapType.ClassType),
        isMutable = true
      ),
      WasmStructField(
        genFieldName.typeData.arrayOf,
        WasmRefType.nullable(genTypeName.ObjectVTable),
        isMutable = true
      ),
      WasmStructField(
        genFieldName.typeData.cloneFunction,
        WasmRefType.nullable(ctx.cloneFunctionTypeName),
        isMutable = false
      ),
      WasmStructField(
        genFieldName.typeData.isJSClassInstance,
        WasmRefType.nullable(ctx.isJSClassInstanceFuncTypeName),
        isMutable = false
      ),
      WasmStructField(
        genFieldName.typeData.reflectiveProxies,
        WasmRefType(WasmHeapType(genTypeName.reflectiveProxies)),
        isMutable = false
      )
    )
  )

  // The number of fields of typeData, after which we find the vtable entries
  def typeDataFieldCount(implicit ctx: wasm.ir2wasm.ReadOnlyWasmContext): Int =
    typeData.fields.size

  val reflectiveProxy: WasmStructType = WasmStructType(
    List(
      WasmStructField(
        genFieldName.reflectiveProxy.func_name,
        WasmInt32,
        isMutable = false
      ),
      WasmStructField(
        genFieldName.reflectiveProxy.func_ref,
        WasmRefType(WasmHeapType.Func),
        isMutable = false
      )
    )
  )
}

case class WasmArrayType(fieldType: WasmFieldType) extends WasmCompositeType
object WasmArrayType {

  /** array (ref typeData) */
  val typeDataArray = WasmArrayType(
    WasmFieldType(
      WasmRefType(genTypeName.typeData),
      isMutable = false
    )
  )

  /** array (ref struct) */
  val itables = WasmArrayType(
    WasmFieldType(
      WasmRefType.nullable(WasmHeapType.Struct),
      isMutable = true
    )
  )

  val reflectiveProxies = WasmArrayType(
    WasmFieldType(
      WasmRefType(genTypeName.reflectiveProxy),
      isMutable = false
    )
  )

  /** array i8 */
  val i8Array = WasmArrayType(WasmFieldType(WasmInt8, true))

  /** array i16 */
  val i16Array = WasmArrayType(WasmFieldType(WasmInt16, true))

  /** array i32 */
  val i32Array = WasmArrayType(WasmFieldType(WasmInt32, true))

  /** array i64 */
  val i64Array = WasmArrayType(WasmFieldType(WasmInt64, true))

  /** array f32 */
  val f32Array = WasmArrayType(WasmFieldType(WasmFloat32, true))

  /** array f64 */
  val f64Array = WasmArrayType(WasmFieldType(WasmFloat64, true))

  /** array anyref */
  val anyArray = WasmArrayType(WasmFieldType(WasmRefType.anyref, true))
}

final case class WasmFieldType(typ: WasmStorageType, isMutable: Boolean)

final case class WasmStructField(name: WasmFieldName, fieldType: WasmFieldType)

object WasmStructField {
  def apply(name: WasmFieldName, typ: WasmStorageType, isMutable: Boolean): WasmStructField =
    WasmStructField(name, WasmFieldType(typ, isMutable))

  val itables = WasmStructField(
    genFieldName.objStruct.itables,
    WasmRefType.nullable(genTypeName.itables),
    isMutable = false
  )
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
