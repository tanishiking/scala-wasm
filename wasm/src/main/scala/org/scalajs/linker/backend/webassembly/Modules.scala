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

  /** A WebAssembly `export`. */
  final case class Export(name: String, desc: ExportDesc)

  /** A WebAssembly `exportdesc`. */
  sealed abstract class ExportDesc

  object ExportDesc {
    final case class Func(funcName: FunctionName) extends ExportDesc
    final case class Global(globalName: GlobalName) extends ExportDesc
  }

  /** A WebAssembly `import`. */
  final case class Import(module: String, name: String, desc: ImportDesc)

  /** A WebAssembly `importdesc`. */
  sealed abstract class ImportDesc

  object ImportDesc {
    final case class Func(id: FunctionName, typeName: TypeName) extends ImportDesc
    final case class Global(id: GlobalName, typ: Type, isMutable: Boolean) extends ImportDesc
    final case class Tag(id: TagName, typeName: TypeName) extends ImportDesc
  }

  /** A WebAssembly `func`, including names for parameters and locals. */
  final case class Function(
      name: FunctionName,
      typeName: TypeName,
      params: List[Local],
      results: List[Type],
      locals: List[Local],
      body: Expr,
      pos: Position
  )

  final case class Local(name: LocalName, typ: Type)

  final case class Tag(val name: TagName, val typ: TypeName)

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
      val datas: List[Data]
  )
}
