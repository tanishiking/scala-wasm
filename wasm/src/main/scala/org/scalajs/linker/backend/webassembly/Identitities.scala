package org.scalajs.linker.backend.webassembly

/** Abstract representation of WebAssembly indices as IDs.
  *
  * Concrete implementations must provide meaningful `equals` and `hashCode` semantics.
  *
  * The are encouraged to have a usable `toString()` implementation for debugging purposes.
  *
  * See [[https://webassembly.github.io/gc/core/syntax/modules.html#indices]]
  */
object Identitities {
  trait LocalID

  trait LabelID

  trait GlobalID

  trait FunctionID

  trait FieldID

  trait TypeID

  trait TagID

  trait DataID

  trait ExportID
}
