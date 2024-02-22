package wasm.wasm4s

import org.scalajs.ir.{Names => IRNames}
import Names._
import Names.WasmTypeName._

object Types {
  abstract sealed class WasmType(
      private val name: String,
      val code: Byte
  ) {
    def show: String = name
  }

  // case object WasmTypeNone extends WasmType
  case object WasmUnreachableType extends WasmType("unreachable", -0x40)
  case object WasmInt32 extends WasmType("i32", -0x1)
  case object WasmInt64 extends WasmType("i64", -0x2)
  case object WasmFloat32 extends WasmType("f32", -0x3)
  case object WasmFloat64 extends WasmType("f64", -0x4)
  // case object WasmVec128 extends WasmType("v128", -0x5)
  // case object WasmInt8 extends WasmType("i8", -0x6)
  // case object WasmInt16 extends WasmType("i16", -0x7)

  // shorthands
  // https://github.com/WebAssembly/gc/blob/main/proposals/gc/MVP.md#reference-types-1
  case object WasmFuncRef extends WasmType("funcref", -0x10)
  case object WasmExternRef extends WasmType("externref", -0x11)
  /** shorthand for (ref null any) */
  case object WasmAnyRef extends WasmType("anyref", -0x12)
  case object WasmEqRef extends WasmType("eqref", -0x13)
  case object WasmRefNullrefType extends WasmType("nullref", -0x0F) // Shorthand for (ref null none)
  case object WasmRefNullExternrefType extends WasmType("nullexternref", -0x0E) // Shorthand for (ref null noextern)
  case class WasmRefNullType(val heapType: WasmHeapType) extends WasmType("ref null", -0x14) {
    override def show: String = s"(ref null ${heapType.show})"
  }
  case class WasmRefType(val heapType: WasmHeapType) extends WasmType("ref", -0x15) {
    override def show: String = s"(ref ${heapType.show})"
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
      object Func extends Simple("func", -0x10)
      object Extern extends Simple("extern", -0x11)
      object Any extends Simple("any", -0x12)
      object Eq extends Simple("eq", -0x13)
      object Struct extends Simple("struct", -0x15)
      object None extends Simple("none", -0x0f)
      object NoExtern extends Simple("noextern", -0x0e)
    }

    val ObjectType = Type(WasmStructTypeName(IRNames.ObjectClass))
  }
}
