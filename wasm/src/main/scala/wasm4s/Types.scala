package wasm.wasm4s

import org.scalajs.ir.{Names => IRNames}
import Names._
import Names.WasmTypeName._

object Types {
  abstract sealed class WasmStorageType(
      private val name: String,
      val code: Byte
  ) {
    def show: String = name
  }

  abstract sealed class WasmType(name: String, code: Byte) extends WasmStorageType(name, code)

  // todo
  case object WasmNoType extends WasmType("", 0x00)

  // case object WasmTypeNone extends WasmType
  case object WasmUnreachableType extends WasmType("unreachable", -0x40)
  case object WasmInt32 extends WasmType("i32", 0x7F)
  case object WasmInt64 extends WasmType("i64", 0x7E)
  case object WasmFloat32 extends WasmType("f32", 0x7D)
  case object WasmFloat64 extends WasmType("f64", 0x7C)
  // case object WasmVec128 extends WasmType("v128", -0x5)

  sealed abstract class WasmPackedType(name: String, code: Byte) extends WasmStorageType(name, code)
  case object WasmInt8 extends WasmPackedType("i8", 0x78)
  case object WasmInt16 extends WasmPackedType("i16", 0x77)

  // shorthands
  // https://github.com/WebAssembly/gc/blob/main/proposals/gc/MVP.md#reference-types-1
  case object WasmFuncRef extends WasmType("funcref", 0x70)
  case object WasmExternRef extends WasmType("externref", 0x6F)

  /** shorthand for (ref null any) */
  case object WasmAnyRef extends WasmType("anyref", 0x6E)
  case object WasmEqRef extends WasmType("eqref", 0x6D)
  case object WasmRefNullrefType extends WasmType("nullref", 0x71) // Shorthand for (ref null none)
  case object WasmRefNullExternrefType
      extends WasmType("nullexternref", 0x72) // Shorthand for (ref null noextern)
  case class WasmRefNullType(val heapType: WasmHeapType) extends WasmType("ref null", 0x63) {
    override def show: String = s"(ref null ${heapType.show})"
  }
  case class WasmRefType(val heapType: WasmHeapType) extends WasmType("ref", 0x64) {
    override def show: String = s"(ref ${heapType.show})"
  }
  object WasmRefType {

    /** Non-null `anyref`. */
    val any: WasmRefType = WasmRefType(WasmHeapType.Simple.Any)
  }

  sealed trait WasmHeapType {
    def show: String
  }
  object WasmHeapType {
    case class Type(val typ: WasmTypeName) extends WasmHeapType {
      override def show: String = typ.show
    }
    case class Func(val typ: WasmFunctionTypeName) extends WasmHeapType {
      override def show: String = typ.show
    }
    sealed class Simple(val name: String, val code: Byte) extends WasmHeapType {
      override def show: String = name
    }
    object Simple {
      object Func extends Simple("func", 0x70)
      object Extern extends Simple("extern", 0x6F)
      object Any extends Simple("any", 0x6E)
      object Eq extends Simple("eq", 0x6D)
      object Array extends Simple("array", 0x6A)
      object Struct extends Simple("struct", 0x6B)
      object None extends Simple("none", 0x71)
      object NoExtern extends Simple("noextern", 0x72)
    }

    val ObjectType = Type(WasmStructTypeName(IRNames.ObjectClass))
    val ClassType = Type(WasmStructTypeName(IRNames.ClassClass))
  }
}
