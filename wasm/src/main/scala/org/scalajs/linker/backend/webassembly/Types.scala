package org.scalajs.linker.backend.webassembly

import Names._

/** WebAssembly types.
  *
  * @see
  *   [[https://webassembly.github.io/gc/core/syntax/types.html]]
  */
object Types {
  sealed trait StorageType

  sealed abstract class Type extends StorageType

  sealed abstract class SimpleType(val textName: String, val binaryCode: Byte) extends Type

  case object Int32 extends SimpleType("i32", 0x7F)
  case object Int64 extends SimpleType("i64", 0x7E)
  case object Float32 extends SimpleType("f32", 0x7D)
  case object Float64 extends SimpleType("f64", 0x7C)

  sealed abstract class PackedType(val textName: String, val binaryCode: Byte) extends StorageType
  case object Int8 extends PackedType("i8", 0x78)
  case object Int16 extends PackedType("i16", 0x77)

  final case class RefType(nullable: Boolean, heapType: HeapType) extends Type {
    def toNullable: RefType = RefType(true, heapType)
    def toNonNullable: RefType = RefType(false, heapType)
  }

  object RefType {

    /** Builds a non-nullable `(ref ht)` for the given `ht`. */
    def apply(ht: HeapType): RefType = RefType(false, ht)

    /** Builds a non-nullable `(ref typ)` for the given `typ`. */
    def apply(typ: TypeName): RefType = apply(HeapType(typ))

    /** Builds a nullable `(ref null ht)` for the given `ht`. */
    def nullable(ht: HeapType): RefType = RefType(true, ht)

    /** Builds a nullable `(ref null typ)` for the given `typ`. */
    def nullable(typ: TypeName): RefType = nullable(HeapType(typ))

    /** `(ref any)`. */
    val any: RefType = apply(HeapType.Any)

    /** `(ref null any)`, i.e., `anyref`. */
    val anyref: RefType = nullable(HeapType.Any)

    /** `(ref func)`. */
    val func: RefType = apply(HeapType.Func)

    /** `(ref null func)`, i.e., `funcref`. */
    val funcref: RefType = nullable(HeapType.Func)

    /** `(ref extern)`. */
    val extern: RefType = apply(HeapType.Extern)

    /** `(ref null extern)`, i.e., `externref`. */
    val externref: RefType = nullable(HeapType.Extern)

    /** `(ref null exn)`, i.e., `exnref`. */
    val exnref: RefType = nullable(HeapType.Exn)

    /** `(ref null none)`, i.e., `nullref`. */
    val nullref: RefType = nullable(HeapType.None)
  }

  sealed trait HeapType

  object HeapType {
    final case class Type(val typ: TypeName) extends HeapType

    sealed abstract class AbsHeapType(
        val textName: String,
        val nullableRefTextName: String,
        val binaryCode: Byte
    ) extends HeapType

    case object Func extends AbsHeapType("func", "funcref", 0x70)
    case object Extern extends AbsHeapType("extern", "externref", 0x6F)
    case object Any extends AbsHeapType("any", "anyref", 0x6E)
    case object Eq extends AbsHeapType("eq", "eqref", 0x6D)
    case object Array extends AbsHeapType("array", "arrayref", 0x6A)
    case object Exn extends AbsHeapType("exn", "exnref", 0x69)
    case object Struct extends AbsHeapType("struct", "structref", 0x6B)
    case object None extends AbsHeapType("none", "nullref", 0x71)
    case object NoExtern extends AbsHeapType("noextern", "nullexternref", 0x72)
    case object NoFunc extends AbsHeapType("nofunc", "nullfuncref", 0x73)
    case object NoExn extends AbsHeapType("noexn", "nullexnref", 0x74)

    def apply(typ: TypeName): HeapType.Type =
      HeapType.Type(typ)
  }
}
