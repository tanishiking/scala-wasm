package wasm.wasm4s

import scala.collection.mutable

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
  final case class Func(id: WasmFunctionName, typ: WasmFunctionType) extends WasmImportDesc
  final case class Global(id: WasmGlobalName, typ: WasmType, isMutable: Boolean)
      extends WasmImportDesc
  final case class Tag(id: WasmTagName, typ: WasmFunctionType) extends WasmImportDesc
}

/** @see
  *   https://webassembly.github.io/spec/core/syntax/modules.html#functions
  */
case class WasmFunction(
    val name: WasmFunctionName,
    val typ: WasmFunctionType,
    val locals: List[WasmLocal],
    val body: WasmExpr
) extends WasmNamedDefinitionField[WasmFunctionName]

/** The index space for locals is only accessible inside a function and includes the parameters of
  * that function, which precede the local variables.
  */
case class WasmLocal(
    val name: WasmLocalName,
    val typ: WasmType,
    val isParameter: Boolean // for text
) extends WasmNamedDefinitionField[WasmLocalName]

final case class WasmTag(val name: WasmTagName, val typ: WasmFunctionTypeName)
    extends WasmNamedDefinitionField[WasmTagName]

final case class WasmData(val name: WasmDataName, val bytes: Array[Byte], mode: WasmData.Mode)
    extends WasmNamedDefinitionField[WasmDataName]

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
) extends WasmNamedDefinitionField[WasmGlobalName]

trait WasmTypeDefinition[T <: WasmName] extends WasmNamedDefinitionField[T]

case class WasmFunctionSignature(
    params: List[WasmType],
    results: List[WasmType]
)
object WasmFunctionSignature {
  val NilToNil: WasmFunctionSignature = WasmFunctionSignature(Nil, Nil)
}

case class WasmFunctionType(
    name: WasmFunctionTypeName,
    params: List[WasmType],
    results: List[WasmType]
) extends WasmTypeDefinition[WasmFunctionTypeName]
object WasmFunctionType {
  def apply(name: WasmFunctionTypeName, sig: WasmFunctionSignature): WasmFunctionType = {
    WasmFunctionType(name, sig.params, sig.results)
  }
}

sealed trait WasmGCTypeDefinition extends WasmTypeDefinition[WasmTypeName]
case class WasmStructType(
    name: WasmTypeName,
    fields: List[WasmStructField],
    superType: Option[WasmTypeName]
) extends WasmGCTypeDefinition
object WasmStructType {

  /** Run-time type data of a `TypeRef`. Support for `j.l.Class` methods and other reflective
    * operations.
    *
    * @see
    *   [[Names.WasmFieldName.typeData]], which contains documentation of what is in each field.
    */
  def typeData(implicit ctx: ReadOnlyWasmContext): WasmStructType = WasmStructType(
    WasmTypeName.WasmStructTypeName.typeData,
    List(
      WasmStructField(
        WasmFieldName.typeData.nameData,
        WasmRefType.nullable(WasmArrayTypeName.i16Array),
        isMutable = false
      ),
      WasmStructField(
        WasmFieldName.typeData.kind,
        WasmInt32,
        isMutable = false
      ),
      WasmStructField(
        WasmFieldName.typeData.specialInstanceTypes,
        WasmInt32,
        isMutable = false
      ),
      WasmStructField(
        WasmFieldName.typeData.strictAncestors,
        WasmRefType.nullable(WasmTypeName.WasmArrayTypeName.typeDataArray),
        isMutable = false
      ),
      WasmStructField(
        WasmFieldName.typeData.componentType,
        WasmRefType.nullable(WasmTypeName.WasmStructTypeName.typeData),
        isMutable = false
      ),
      WasmStructField(
        WasmFieldName.typeData.name,
        WasmRefType.anyref,
        isMutable = true
      ),
      WasmStructField(
        WasmFieldName.typeData.classOfValue,
        WasmRefType.nullable(WasmHeapType.ClassType),
        isMutable = true
      ),
      WasmStructField(
        WasmFieldName.typeData.arrayOf,
        WasmRefType.nullable(WasmTypeName.WasmStructTypeName.ObjectVTable),
        isMutable = true
      ),
      WasmStructField(
        WasmFieldName.typeData.cloneFunction,
        WasmRefType.nullable(ctx.cloneFunctionTypeName),
        isMutable = false
      )
    ),
    None
  )

  // The number of fields of typeData, after which we find the vtable entries
  def typeDataFieldCount(implicit ctx: ReadOnlyWasmContext) = typeData.fields.size
}

case class WasmArrayType(
    name: WasmTypeName.WasmArrayTypeName,
    field: WasmStructField
) extends WasmGCTypeDefinition
object WasmArrayType {

  /** array (ref typeData) */
  val typeDataArray = WasmArrayType(
    WasmArrayTypeName.typeDataArray,
    WasmStructField(
      WasmFieldName.arrayItem,
      WasmRefType(WasmStructTypeName.typeData),
      isMutable = false
    )
  )

  /** array (ref struct) */
  val itables = WasmArrayType(
    WasmArrayTypeName.itables,
    WasmStructField(
      WasmFieldName.itable,
      WasmRefType.nullable(WasmHeapType.Struct),
      isMutable = true
    )
  )

  /** array i8 */
  val i8Array = WasmArrayType(
    WasmArrayTypeName.i8Array,
    WasmStructField(WasmFieldName.arrayItem, WasmInt8, true)
  )

  /** array i16 */
  val i16Array = WasmArrayType(
    WasmArrayTypeName.i16Array,
    WasmStructField(WasmFieldName.arrayItem, WasmInt16, true)
  )

  /** array i32 */
  val i32Array = WasmArrayType(
    WasmArrayTypeName.i32Array,
    WasmStructField(WasmFieldName.arrayItem, WasmInt32, true)
  )

  /** array i64 */
  val i64Array = WasmArrayType(
    WasmArrayTypeName.i64Array,
    WasmStructField(WasmFieldName.arrayItem, WasmInt64, true)
  )

  /** array f32 */
  val f32Array = WasmArrayType(
    WasmArrayTypeName.f32Array,
    WasmStructField(WasmFieldName.arrayItem, WasmFloat32, true)
  )

  /** array f64 */
  val f64Array = WasmArrayType(
    WasmArrayTypeName.f64Array,
    WasmStructField(WasmFieldName.arrayItem, WasmFloat64, true)
  )

  /** array anyref */
  val anyArray = WasmArrayType(
    WasmArrayTypeName.anyArray,
    WasmStructField(WasmFieldName.arrayItem, WasmRefType.anyref, true)
  )
}

case class WasmStructField(
    name: WasmFieldName,
    typ: WasmStorageType,
    isMutable: Boolean
)
object WasmStructField {
  val itables = WasmStructField(
    WasmFieldName.itables,
    WasmRefType.nullable(WasmArrayType.itables.name),
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
class WasmModule(
    private val _functionTypes: mutable.ListBuffer[WasmFunctionType] = new mutable.ListBuffer(),
    private val _arrayTypes: mutable.Set[WasmArrayType] = new mutable.HashSet(),
    private val _recGroupTypes: mutable.ListBuffer[WasmStructType] = new mutable.ListBuffer(),
    // val importsInOrder: List[WasmNamedModuleField] = Nil,
    private val _imports: mutable.ListBuffer[WasmImport] = new mutable.ListBuffer(),
    // val importedMemories: List[WasmMemory] = Nil,
    // val importedTables: List[WasmTable] = Nil,
    // val importedGlobals: List[WasmGlobal] = Nil,
    // val importedTags: List[WasmTag] = Nil,
    private val _definedFunctions: mutable.ListBuffer[WasmFunction] = new mutable.ListBuffer(),
    // val tables: List[WasmTable] = Nil,
    // val memories: List[WasmMemory] = Nil,
    private val _tags: mutable.ListBuffer[WasmTag] = new mutable.ListBuffer(),
    private val _data: mutable.ListBuffer[WasmData] = new mutable.ListBuffer(),
    private val _globals: mutable.ListBuffer[WasmGlobal] = new mutable.ListBuffer(),
    private val _exports: mutable.ListBuffer[WasmExport] = new mutable.ListBuffer(),
    private var _startFunction: Option[WasmFunctionName] = None,
    private val _elements: mutable.ListBuffer[WasmElement] = new mutable.ListBuffer()
    // val tags: List[WasmTag] = Nil,
    // val startFunction: WasmFunction = null,
    // val data: List[WasmData] = Nil,
    // val dataCount: Boolean = true
) {
  def addImport(imprt: WasmImport): Unit = _imports += imprt
  def addFunction(function: WasmFunction): Unit = _definedFunctions += function
  def addArrayType(typ: WasmArrayType): Unit = _arrayTypes += typ
  def addFunctionType(typ: WasmFunctionType): Unit = _functionTypes += typ
  def addRecGroupType(typ: WasmStructType): Unit = _recGroupTypes += typ
  def addTag(tag: WasmTag): Unit = _tags += tag
  def addData(data: WasmData): Unit = _data += data
  def addGlobal(typ: WasmGlobal): Unit = _globals += typ
  def addExport(exprt: WasmExport): Unit = _exports += exprt
  def setStartFunction(startFunction: WasmFunctionName): Unit = _startFunction = Some(startFunction)
  def addElement(element: WasmElement): Unit = _elements += element

  locally {
    addArrayType(WasmArrayType.typeDataArray)
    addArrayType(WasmArrayType.itables)

    addArrayType(WasmArrayType.i8Array)
    addArrayType(WasmArrayType.i16Array)
    addArrayType(WasmArrayType.i32Array)
    addArrayType(WasmArrayType.i64Array)
    addArrayType(WasmArrayType.f32Array)
    addArrayType(WasmArrayType.f64Array)
    addArrayType(WasmArrayType.anyArray)
  }

  def functionTypes = _functionTypes.toList
  def recGroupTypes = WasmModule.tsort(_recGroupTypes.toList)
  def arrayTypes = _arrayTypes.toList
  def imports = _imports.toList
  def definedFunctions = _definedFunctions.toList
  def tags: List[WasmTag] = _tags.toList
  def data: List[WasmData] = _data.toList
  def globals = _globals.toList
  def exports = _exports.toList
  def startFunction: Option[WasmFunctionName] = _startFunction
  def elements: List[WasmElement] = _elements.toList
}

object WasmModule {

  private def tsort(types: List[WasmStructType]): List[WasmStructType] = {
    def tsort(
        toPreds: Map[WasmTypeName, Option[WasmTypeName]],
        done: List[WasmTypeName]
    ): List[WasmTypeName] = {
      val (noPreds, hasPreds) = toPreds.partition { _._2.isEmpty }
      if (noPreds.isEmpty) {
        if (hasPreds.isEmpty) done else sys.error(hasPreds.toString)
      } else {
        val found = noPreds.map { _._1 }.toSet
        val updated = hasPreds.map { case (k, v) =>
          (k, v.filter(!found.contains(_)))
        }
        tsort(updated, done ++ found)
      }
    }
    val predecessors: Map[WasmTypeName, Option[WasmTypeName]] =
      types.map(t => t.name -> t.superType).toMap
    val typeMap = types.map(t => t.name -> t).toMap

    val sortedNames = tsort(predecessors, Nil)
    sortedNames.map(name => typeMap(name))
  }

}
