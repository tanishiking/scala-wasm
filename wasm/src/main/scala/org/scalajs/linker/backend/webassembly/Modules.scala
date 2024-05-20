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
  sealed case class Expr(instr: List[Instr])

  sealed abstract class Export {
    val exportName: String
  }

  object Export {
    final case class Function(exportName: String, funcName: FunctionName) extends Export
    final case class Global(exportName: String, globalName: GlobalName) extends Export
    final case class Memory(exportName: String, memoryName: MemoryName) extends Export
  }

  final case class Import(module: String, name: String, desc: ImportDesc)

  sealed abstract class ImportDesc

  object ImportDesc {
    final case class Func(id: FunctionName, typeName: TypeName) extends ImportDesc
    final case class Global(id: GlobalName, typ: Type, isMutable: Boolean) extends ImportDesc
    final case class Tag(id: TagName, typeName: TypeName) extends ImportDesc
  }

  /** @see
    *   https://webassembly.github.io/spec/core/syntax/modules.html#functions
    */
  final case class Function(
      val name: FunctionName,
      val typeName: TypeName,
      val locals: List[Local],
      val results: List[Type],
      val body: Expr,
      val pos: Position
  )

  /** The index space for locals is only accessible inside a function and includes the parameters of
    * that function, which precede the local variables.
    */
  case class Local(
      val name: LocalName,
      val typ: Type,
      val isParameter: Boolean // for text
  )

  final case class Tag(val name: TagName, val typ: TypeName)

  final case class Memory(val name: MemoryName, val limits: Memory.Limits)
  object Memory {
    final case class Limits(min: Int, max: Option[Int])
  }

  final case class Data(val name: DataName, val bytes: Array[Byte], mode: Data.Mode)

  object Data {
    sealed abstract class Mode
    object Mode {
      case object Passive extends Mode
      // final case class Active
    }
  }

  final case class Global(
      val name: GlobalName,
      val typ: Type,
      val init: Expr,
      val isMutable: Boolean
  )

  final case class RecType(subTypes: List[SubType])

  object RecType {

    /** Builds a `rectype` with a single `subtype`. */
    def apply(singleSubType: SubType): RecType =
      RecType(singleSubType :: Nil)
  }

  final case class SubType(
      name: TypeName,
      isFinal: Boolean,
      superType: Option[TypeName],
      compositeType: CompositeType
  )

  object SubType {

    /** Builds a `subtype` that is `final` and without any super type. */
    def apply(name: TypeName, compositeType: CompositeType): SubType =
      SubType(name, isFinal = true, superType = None, compositeType)
  }

  sealed abstract class CompositeType

  final case class FunctionType(params: List[Type], results: List[Type]) extends CompositeType

  object FunctionType {
    val NilToNil: FunctionType = FunctionType(Nil, Nil)
  }

  final case class StructType(fields: List[StructField]) extends CompositeType

  final case class ArrayType(fieldType: FieldType) extends CompositeType

  final case class FieldType(typ: StorageType, isMutable: Boolean)

  final case class StructField(name: FieldName, fieldType: FieldType)

  object StructField {
    def apply(name: FieldName, typ: StorageType, isMutable: Boolean): StructField =
      StructField(name, FieldType(typ, isMutable))
  }

  final case class Element(typ: Type, init: List[Expr], mode: Element.Mode)

  object Element {
    sealed abstract class Mode

    object Mode {
      case object Passive extends Mode
      // final case class Active(table: Immediate.TableIdx, offset: Expr) extends Mode
      case object Declarative extends Mode
    }
  }

  /** @see
    *   https://webassembly.github.io/spec/core/syntax/modules.html#modules
    */
  final class Module(
      val types: List[RecType],
      val imports: List[Import],
      val funcs: List[Function],
      val tags: List[Tag],
      val globals: List[Global],
      val exports: List[Export],
      val start: Option[FunctionName],
      val elems: List[Element],
      val datas: List[Data],
      val memories: List[Memory]
  )
}
