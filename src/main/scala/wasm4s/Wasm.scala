package wasm4s

import scala.collection.mutable

import Types._

case class Ident(name: String) {

}

sealed case class WasmExpr(instr: List[WasmInstr])

/** @see
  *   https://webassembly.github.io/spec/core/syntax/modules.html#functions
  */
case class WasmFunction(
    val ident: Ident,
    val typ: WasmSymbol[WasmFunctionType],
    val locals: List[WasmLocal],
    val body: WasmExpr
) extends WasmNamedDefinitionField

/** The index space for locals is only accessible inside a function and includes the parameters of
  * that function, which precede the local variables.
  */
case class WasmLocal(
    val ident: Ident,
    val typ: WasmType,
    val isParameter: Boolean // for text
) extends WasmNamedDefinitionField

case class WasmGlobal(
    val ident: Ident,
    val typ: WasmType
) extends WasmNamedDefinitionField

trait WasmTypeDefinition extends WasmNamedDefinitionField
trait WasmGCTypeDefinition extends WasmTypeDefinition
case class WasmFunctionType(
    ident: Ident,
    params: List[WasmType],
    results: List[WasmType]
) extends WasmTypeDefinition

case class WasmStructType(
    ident: Ident,
    fields: List[WasmStructField],
    superType: WasmSymbol[WasmTypeDefinition]
) extends WasmGCTypeDefinition

case class WasmArrayType(
    ident: Ident,
    field: WasmStructField
) extends WasmGCTypeDefinition

case class WasmStructField(
    name: String,
    typ: WasmType,
    isMutable: Boolean
)

/** @see
  *   https://webassembly.github.io/spec/core/syntax/modules.html#modules
  */
class WasmModule(
    private val _functionTypes: mutable.ListBuffer[WasmFunctionType] = new mutable.ListBuffer(),
    private val _recGroupTypes: mutable.ListBuffer[WasmTypeDefinition] = new mutable.ListBuffer(),
    // val importsInOrder: List[WasmNamedModuleField] = Nil,
    // val importedFunctions: List[WasmFunction.Imported] = Nil,
    // val importedMemories: List[WasmMemory] = Nil,
    // val importedTables: List[WasmTable] = Nil,
    // val importedGlobals: List[WasmGlobal] = Nil,
    // val importedTags: List[WasmTag] = Nil,
    private val _definedFunctions: mutable.ListBuffer[WasmFunction] = new mutable.ListBuffer()
    // val tables: List[WasmTable] = Nil,
    // val memories: List[WasmMemory] = Nil,
    // val globals: List[WasmGlobal] = Nil,
    // val exports: List[WasmExport[_]] = Nil,
    // val elements: List[WasmElement] = Nil,
    // val tags: List[WasmTag] = Nil,
    // val startFunction: WasmFunction = null,
    // val data: List[WasmData] = Nil,
    // val dataCount: Boolean = true
) {
  def addFunction(function: WasmFunction): Unit = _definedFunctions.addOne(function)
  def addFunctionType(typ: WasmFunctionType): Unit = _functionTypes.addOne(typ)
  def addRecGroupType(typ: WasmTypeDefinition): Unit = _recGroupTypes.addOne(typ)

  def functionTypes = _functionTypes.toList
  def recGroupTypes = _recGroupTypes.toList
  def definedFunctions = _definedFunctions.toList
}
