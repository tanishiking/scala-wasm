package org.scalajs.linker.backend.webassembly

import org.scalajs.ir.OriginalName

import Identitities._

/** WebAssembly types.
  *
  * @see
  *   [[https://webassembly.github.io/gc/core/syntax/types.html]]
  */
object Types {

  /** A WebAssembly `storagetype`. */
  sealed trait StorageType

  /** A WebAssembly `valtype`. */
  sealed abstract class Type extends StorageType

  /** Convenience superclass for `Type`s that are encoded with a simple opcode. */
  sealed abstract class SimpleType(val textName: String, val binaryCode: Byte) extends Type

  case object Int32 extends SimpleType("i32", 0x7F)
  case object Int64 extends SimpleType("i64", 0x7E)
  case object Float32 extends SimpleType("f32", 0x7D)
  case object Float64 extends SimpleType("f64", 0x7C)

  /** A WebAssembly `packedtype`. */
  sealed abstract class PackedType(val textName: String, val binaryCode: Byte) extends StorageType

  case object Int8 extends PackedType("i8", 0x78)
  case object Int16 extends PackedType("i16", 0x77)

  /** A WebAssembly `reftype`. */
  final case class RefType(nullable: Boolean, heapType: HeapType) extends Type {
    def toNullable: RefType = RefType(true, heapType)
    def toNonNullable: RefType = RefType(false, heapType)
  }

  object RefType {

    /** Builds a non-nullable `(ref ht)` for the given `ht`. */
    def apply(ht: HeapType): RefType = RefType(false, ht)

    /** Builds a non-nullable `(ref typ)` for the given `typ`. */
    def apply(typ: TypeID): RefType = apply(HeapType(typ))

    /** Builds a nullable `(ref null ht)` for the given `ht`. */
    def nullable(ht: HeapType): RefType = RefType(true, ht)

    /** Builds a nullable `(ref null typ)` for the given `typ`. */
    def nullable(typ: TypeID): RefType = nullable(HeapType(typ))

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

  /** A WebAssembly `heaptype`. */
  sealed abstract class HeapType

  object HeapType {

    /** Reference to a named composite type. */
    final case class Type(val typ: TypeID) extends HeapType

    /** A WebAssembly `absheaptype`. */
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

    def apply(typ: TypeID): HeapType.Type =
      HeapType.Type(typ)
  }

  /** A WebAssembly `rectype`. */
  final case class RecType(subTypes: List[SubType])

  object RecType {

    /** Builds a `rectype` with a single `subtype`. */
    def apply(singleSubType: SubType): RecType =
      RecType(singleSubType :: Nil)
  }

  /** A WebAssembly `subtype` with an associated name. */
  final case class SubType(
      id: TypeID,
      originalName: OriginalName,
      isFinal: Boolean,
      superType: Option[TypeID],
      compositeType: CompositeType
  )

  object SubType {

    /** Builds a `subtype` that is `final` and without any super type. */
    def apply(id: TypeID, originalName: OriginalName, compositeType: CompositeType): SubType =
      SubType(id, originalName, isFinal = true, superType = None, compositeType)
  }

  /** A WebAssembly `comptype`. */
  sealed abstract class CompositeType

  /** A WebAssembly `functype`. */
  final case class FunctionType(params: List[Type], results: List[Type]) extends CompositeType

  object FunctionType {
    val NilToNil: FunctionType = FunctionType(Nil, Nil)
  }

  /** A WebAssembly `structtype` with associated field names. */
  final case class StructType(fields: List[StructField]) extends CompositeType

  /** A member of a `StructType`, with a field name and a WebAssembly `fieldtype`. */
  final case class StructField(id: FieldID, originalName: OriginalName, fieldType: FieldType)

  /** A WebAssembly `arraytype`. */
  final case class ArrayType(fieldType: FieldType) extends CompositeType

  /** A WebAssembly `fieldtype`. */
  final case class FieldType(typ: StorageType, isMutable: Boolean)

  object StructField {
    def apply(
        id: FieldID,
        originalName: OriginalName,
        typ: StorageType,
        isMutable: Boolean
    ): StructField = {
      StructField(id, originalName, FieldType(typ, isMutable))
    }
  }
}
