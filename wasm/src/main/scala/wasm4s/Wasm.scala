package wasm.wasm4s

import scala.collection.mutable

import Types._
import Names._
import Names.WasmTypeName._

sealed case class WasmExpr(instr: List[WasmInstr])

sealed abstract class WasmExport[T <: WasmNamedDefinitionField[_]](
    val name: String,
    val field: T,
    val kind: Byte,
    val keyword: String
)

object WasmExport {
  class Function(name: String, field: WasmFunction)
      extends WasmExport[WasmFunction](name, field, 0x0, "func")
  class Global(name: String, field: WasmGlobal)
      extends WasmExport[WasmGlobal](name, field, 0x3, "global")

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

case class WasmGlobal(
    val name: WasmGlobalName,
    val typ: WasmType,
    val init: Option[WasmExpr]
) extends WasmNamedDefinitionField[WasmGlobalName]

trait WasmTypeDefinition[T <: WasmName] extends WasmNamedDefinitionField[T]

case class WasmFunctionSignature(
    params: List[WasmType],
    results: List[WasmType]
)
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

case class WasmArrayType(
    name: WasmTypeName,
    field: WasmStructField
) extends WasmGCTypeDefinition

case class WasmStructField(
    name: WasmFieldName,
    typ: WasmType,
    isMutable: Boolean
)

/** @see
  *   https://webassembly.github.io/spec/core/syntax/modules.html#modules
  */
class WasmModule(
    private val _functionTypes: mutable.ListBuffer[WasmFunctionType] = new mutable.ListBuffer(),
    private val _recGroupTypes: mutable.ListBuffer[WasmGCTypeDefinition] = new mutable.ListBuffer(),
    // val importsInOrder: List[WasmNamedModuleField] = Nil,
    // val importedFunctions: List[WasmFunction.Imported] = Nil,
    // val importedMemories: List[WasmMemory] = Nil,
    // val importedTables: List[WasmTable] = Nil,
    // val importedGlobals: List[WasmGlobal] = Nil,
    // val importedTags: List[WasmTag] = Nil,
    private val _definedFunctions: mutable.ListBuffer[WasmFunction] = new mutable.ListBuffer(),
    // val tables: List[WasmTable] = Nil,
    // val memories: List[WasmMemory] = Nil,
    private val _globals: mutable.ListBuffer[WasmGlobal] = new mutable.ListBuffer(),
    private val _exports: mutable.ListBuffer[WasmExport[_]] = new mutable.ListBuffer()
    // val elements: List[WasmElement] = Nil,
    // val tags: List[WasmTag] = Nil,
    // val startFunction: WasmFunction = null,
    // val data: List[WasmData] = Nil,
    // val dataCount: Boolean = true
) {
  def addFunction(function: WasmFunction): Unit = _definedFunctions.addOne(function)
  def addFunctionType(typ: WasmFunctionType): Unit = _functionTypes.addOne(typ)
  def addRecGroupType(typ: WasmGCTypeDefinition): Unit = _recGroupTypes.addOne(typ)
  def addGlobal(typ: WasmGlobal): Unit = _globals.addOne(typ)
  def addExport(export: WasmExport[_]) = _exports.addOne(export)

  def functionTypes = _functionTypes.toList
  def recGroupTypes = _recGroupTypes.toList
  def definedFunctions = _definedFunctions.toList
  def globals = _globals.toList
  def exports = _exports.toList
}
