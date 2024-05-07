package org.scalajs.linker.backend.webassembly

import scala.collection.mutable

import Names._

final class ModuleBuilder(functionSignatureProvider: ModuleBuilder.FunctionSignatureProvider) {
  import ModuleBuilder._

  /** Items are `WasmRecType | RecTypeBuilder`. */
  private val types: mutable.ListBuffer[AnyRef] = new mutable.ListBuffer()

  private val imports: mutable.ListBuffer[WasmImport] = new mutable.ListBuffer()
  private val funcs: mutable.ListBuffer[WasmFunction] = new mutable.ListBuffer()
  private val tags: mutable.ListBuffer[WasmTag] = new mutable.ListBuffer()
  private val globals: mutable.ListBuffer[WasmGlobal] = new mutable.ListBuffer()
  private val exports: mutable.ListBuffer[WasmExport] = new mutable.ListBuffer()
  private var start: Option[WasmFunctionName] = None
  private val elems: mutable.ListBuffer[WasmElement] = new mutable.ListBuffer()
  private val datas: mutable.ListBuffer[WasmData] = new mutable.ListBuffer()

  def signatureToTypeName(sig: WasmFunctionSignature): WasmTypeName =
    functionSignatureProvider.signatureToTypeName(sig)

  def addRecType(typ: WasmRecType): Unit = types += typ
  def addRecType(typ: WasmSubType): Unit = addRecType(WasmRecType(typ))

  def addRecType(name: WasmTypeName, compositeType: WasmCompositeType): Unit =
    addRecType(WasmSubType(name, compositeType))

  def addRecTypeBuilder(recTypeBuilder: RecTypeBuilder): Unit =
    types += recTypeBuilder

  def addImport(imprt: WasmImport): Unit = imports += imprt
  def addFunction(function: WasmFunction): Unit = funcs += function
  def addTag(tag: WasmTag): Unit = tags += tag
  def addGlobal(typ: WasmGlobal): Unit = globals += typ
  def addExport(exprt: WasmExport): Unit = exports += exprt
  def setStart(startFunction: WasmFunctionName): Unit = start = Some(startFunction)
  def addElement(element: WasmElement): Unit = elems += element
  def addData(data: WasmData): Unit = datas += data

  def build(): WasmModule = {
    val builtTypes: List[WasmRecType] = types.toList.map { tpe =>
      tpe match {
        case tpe: WasmRecType        => tpe
        case builder: RecTypeBuilder => builder.build()
      }
    }

    new WasmModule(
      builtTypes,
      imports.toList,
      funcs.toList,
      tags.toList,
      globals.toList,
      exports.toList,
      start,
      elems.toList,
      datas.toList
    )
  }
}

object ModuleBuilder {
  trait FunctionSignatureProvider {
    def signatureToTypeName(sig: WasmFunctionSignature): WasmTypeName
  }

  final class RecTypeBuilder {
    private val subTypes = mutable.ListBuffer.empty[WasmSubType]

    def addSubType(subType: WasmSubType): Unit =
      subTypes += subType

    def addSubType(name: WasmTypeName, compositeType: WasmCompositeType): Unit =
      addSubType(WasmSubType(name, compositeType))

    def build(): WasmRecType = WasmRecType(subTypes.toList)
  }
}
