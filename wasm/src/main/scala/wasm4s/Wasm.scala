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

final class WasmRecType {
  private val _subTypes = mutable.ListBuffer.empty[WasmSubType]

  def addSubType(subType: WasmSubType): Unit =
    _subTypes += subType

  def addSubType(name: WasmTypeName, compositeType: WasmCompositeType): Unit =
    addSubType(WasmSubType(name, compositeType))

  def subTypes: List[WasmSubType] = _subTypes.toList
}

object WasmRecType {

  /** Builds a `rectype` with a single `subtype`. */
  def apply(singleSubType: WasmSubType): WasmRecType = {
    val recType = new WasmRecType
    recType.addSubType(singleSubType)
    recType
  }
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
  def typeData(implicit ctx: ReadOnlyWasmContext): WasmStructType = WasmStructType(
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
  def typeDataFieldCount(implicit ctx: ReadOnlyWasmContext) = typeData.fields.size

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
class WasmModule {
  private val _recTypes: mutable.ListBuffer[WasmRecType] = new mutable.ListBuffer()
  private val _imports: mutable.ListBuffer[WasmImport] = new mutable.ListBuffer()
  private val _definedFunctions: mutable.ListBuffer[WasmFunction] = new mutable.ListBuffer()
  private val _tags: mutable.ListBuffer[WasmTag] = new mutable.ListBuffer()
  private val _data: mutable.ListBuffer[WasmData] = new mutable.ListBuffer()
  private val _globals: mutable.ListBuffer[WasmGlobal] = new mutable.ListBuffer()
  private val _exports: mutable.ListBuffer[WasmExport] = new mutable.ListBuffer()
  private var _startFunction: Option[WasmFunctionName] = None
  private val _elements: mutable.ListBuffer[WasmElement] = new mutable.ListBuffer()

  def addRecType(typ: WasmRecType): Unit = _recTypes += typ
  def addRecType(typ: WasmSubType): Unit = addRecType(WasmRecType(typ))

  def addRecType(name: WasmTypeName, compositeType: WasmCompositeType): Unit =
    addRecType(WasmSubType(name, compositeType))

  def addImport(imprt: WasmImport): Unit = _imports += imprt
  def addFunction(function: WasmFunction): Unit = _definedFunctions += function
  def addTag(tag: WasmTag): Unit = _tags += tag
  def addData(data: WasmData): Unit = _data += data
  def addGlobal(typ: WasmGlobal): Unit = _globals += typ
  def addExport(exprt: WasmExport): Unit = _exports += exprt
  def setStartFunction(startFunction: WasmFunctionName): Unit = _startFunction = Some(startFunction)
  def addElement(element: WasmElement): Unit = _elements += element

  def recTypes = _recTypes.toList
  def imports = _imports.toList
  def definedFunctions = _definedFunctions.toList
  def tags: List[WasmTag] = _tags.toList
  def data: List[WasmData] = _data.toList
  def globals = _globals.toList
  def exports = _exports.toList
  def startFunction: Option[WasmFunctionName] = _startFunction
  def elements: List[WasmElement] = _elements.toList
}
