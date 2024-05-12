package org.scalajs.linker.backend.webassembly

/** Abstract representation of WebAssembly indices as names.
  *
  * See [[https://webassembly.github.io/gc/core/syntax/modules.html#indices]]
  */
object Names {
  sealed abstract class Name {
    val name: String
  }

  final case class LocalName(name: String) extends Name

  final case class LabelName(name: String) extends Name

  final case class GlobalName(name: String) extends Name

  final case class FunctionName(name: String) extends Name

  final case class FieldName(name: String) extends Name

  final case class FieldIdx(value: Int)

  final case class TypeName(name: String) extends Name

  final case class TagName(name: String) extends Name

  final case class DataName(name: String) extends Name

  final case class ExportName(name: String) extends Name

}
