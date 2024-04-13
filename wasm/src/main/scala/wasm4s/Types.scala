package wasm.wasm4s

import org.scalajs.ir.{Names => IRNames}
import Names._
import Names.WasmTypeName._

import wasm.ir2wasm.SpecialNames

object Types {
  sealed trait WasmStorageType

  sealed abstract class WasmType extends WasmStorageType

  sealed abstract class WasmSimpleType(val textName: String, val binaryCode: Byte) extends WasmType

  case object WasmInt32 extends WasmSimpleType("i32", 0x7F)
  case object WasmInt64 extends WasmSimpleType("i64", 0x7E)
  case object WasmFloat32 extends WasmSimpleType("f32", 0x7D)
  case object WasmFloat64 extends WasmSimpleType("f64", 0x7C)

  sealed abstract class WasmPackedType(val textName: String, val binaryCode: Byte)
      extends WasmStorageType
  case object WasmInt8 extends WasmPackedType("i8", 0x78)
  case object WasmInt16 extends WasmPackedType("i16", 0x77)

  final case class WasmRefType(nullable: Boolean, heapType: WasmHeapType) extends WasmType

  object WasmRefType {

    /** Builds a non-nullable `(ref ht)` for the given `ht`. */
    def apply(ht: WasmHeapType): WasmRefType = WasmRefType(false, ht)

    /** Builds a non-nullable `(ref typ)` for the given `typ`. */
    def apply(typ: WasmTypeName): WasmRefType = apply(WasmHeapType(typ))

    /** Builds a nullable `(ref null ht)` for the given `ht`. */
    def nullable(ht: WasmHeapType): WasmRefType = WasmRefType(true, ht)

    /** Builds a nullable `(ref null typ)` for the given `typ`. */
    def nullable(typ: WasmTypeName): WasmRefType = nullable(WasmHeapType(typ))

    /** `(ref any)`. */
    val any: WasmRefType = apply(WasmHeapType.Any)

    /** `(ref null any)`, i.e., `anyref`. */
    val anyref: WasmRefType = nullable(WasmHeapType.Any)

    /** `(ref func)`. */
    val func: WasmRefType = apply(WasmHeapType.Func)

    /** `(ref null func)`, i.e., `funcref`. */
    val funcref: WasmRefType = nullable(WasmHeapType.Func)

    /** `(ref extern)`. */
    val extern: WasmRefType = apply(WasmHeapType.Extern)

    /** `(ref null exn)`, i.e., `exnref`. */
    val exnref: WasmRefType = nullable(WasmHeapType.Exn)

    /** `(ref null none)`, i.e., `nullref`. */
    val nullref: WasmRefType = nullable(WasmHeapType.None)
  }

  sealed trait WasmHeapType

  object WasmHeapType {
    final case class Type(val typ: WasmTypeName) extends WasmHeapType

    sealed abstract class AbsHeapType(
        val textName: String,
        val nullableRefTextName: String,
        val binaryCode: Byte
    ) extends WasmHeapType

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

    def apply(typ: WasmTypeName): WasmHeapType.Type =
      WasmHeapType.Type(typ)

    val ObjectType = Type(WasmStructTypeName.forClass(IRNames.ObjectClass))
    val ClassType = Type(WasmStructTypeName.forClass(IRNames.ClassClass))
    val ThrowableType = Type(WasmStructTypeName.forClass(IRNames.ThrowableClass))
    val JSExceptionType = Type(WasmStructTypeName.forClass(SpecialNames.JSExceptionClass))
  }
}
