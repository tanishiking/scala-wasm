package org.scalajs.linker.backend.webassembly

import scala.collection.mutable

import Names._
import Modules._

final class ModuleBuilder(functionSignatureProvider: ModuleBuilder.FunctionTypeProvider) {
  import ModuleBuilder._

  /** Items are `RecType | RecTypeBuilder`. */
  private val types: mutable.ListBuffer[AnyRef] = new mutable.ListBuffer()

  private val imports: mutable.ListBuffer[Import] = new mutable.ListBuffer()
  private val funcs: mutable.ListBuffer[Function] = new mutable.ListBuffer()
  private val tags: mutable.ListBuffer[Tag] = new mutable.ListBuffer()
  private val globals: mutable.ListBuffer[Global] = new mutable.ListBuffer()
  private val exports: mutable.ListBuffer[Export] = new mutable.ListBuffer()
  private var start: Option[FunctionName] = None
  private val elems: mutable.ListBuffer[Element] = new mutable.ListBuffer()
  private val datas: mutable.ListBuffer[Data] = new mutable.ListBuffer()
  private val memories: mutable.ListBuffer[Memory] = new mutable.ListBuffer()

  def functionTypeToTypeName(sig: FunctionType): TypeName =
    functionSignatureProvider.functionTypeToTypeName(sig)

  def addRecType(typ: RecType): Unit = types += typ
  def addRecType(typ: SubType): Unit = addRecType(RecType(typ))

  def addRecType(name: TypeName, compositeType: CompositeType): Unit =
    addRecType(SubType(name, compositeType))

  def addRecTypeBuilder(recTypeBuilder: RecTypeBuilder): Unit =
    types += recTypeBuilder

  def addImport(imprt: Import): Unit = imports += imprt
  def addFunction(function: Function): Unit = funcs += function
  def addTag(tag: Tag): Unit = tags += tag
  def addGlobal(typ: Global): Unit = globals += typ
  def addExport(exprt: Export): Unit = exports += exprt
  def setStart(startFunction: FunctionName): Unit = start = Some(startFunction)
  def addElement(element: Element): Unit = elems += element
  def addData(data: Data): Unit = datas += data
  def addMemory(mem: Memory): Unit = memories += mem

  def build(): Module = {
    val builtTypes: List[RecType] = types.toList.map { tpe =>
      tpe match {
        case tpe: RecType            => tpe
        case builder: RecTypeBuilder => builder.build()
      }
    }

    new Module(
      builtTypes,
      imports.toList,
      funcs.toList,
      tags.toList,
      globals.toList,
      exports.toList,
      start,
      elems.toList,
      datas.toList,
      memories.toList
    )
  }
}

object ModuleBuilder {
  trait FunctionTypeProvider {
    def functionTypeToTypeName(sig: FunctionType): TypeName
  }

  final class RecTypeBuilder {
    private val subTypes = mutable.ListBuffer.empty[SubType]

    def addSubType(subType: SubType): Unit =
      subTypes += subType

    def addSubType(name: TypeName, compositeType: CompositeType): Unit =
      addSubType(SubType(name, compositeType))

    def build(): RecType = RecType(subTypes.toList)
  }
}
