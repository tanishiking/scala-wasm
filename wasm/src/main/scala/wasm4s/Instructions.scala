package wasm.wasm4s
// https://webassembly.github.io/spec/core/syntax/instructions.html

import Types._
import Names._
import Names.WasmTypeName._

sealed abstract class WasmInstr(val mnemonic: String, val opcode: Int)

object WasmInstr {
  import WasmImmediate._

  // Semantic categories of instructions

  /** A stack-polymorphic instruction. */
  sealed trait StackPolymorphicInstr extends WasmInstr

  /** An instruction that opens a structured control block. */
  sealed trait StructuredLabeledInstr extends WasmInstr {
    val label: Option[WasmLabelName]
  }

  // Convenience subclasses of instructions for writing text/binary

  /** An instruction without any immediate argument. */
  sealed abstract class WasmSimpleInstr(mnemonic: String, opcode: Int)
      extends WasmInstr(mnemonic, opcode)

  /** A structured labeled instruction with a single `BlockType` argument. */
  sealed abstract class WasmBlockTypeLabeledInstr(
      mnemonic: String,
      opcode: Int,
      val blockTypeArgument: BlockType
  ) extends WasmInstr(mnemonic, opcode)
      with StructuredLabeledInstr

  /** An instruction with a single `LabelIdx` argument. */
  sealed abstract class WasmLabelInstr(
      mnemonic: String,
      opcode: Int,
      val labelArgument: WasmLabelName
  ) extends WasmInstr(mnemonic, opcode)

  /** An instruction with a single `WasmFunctionName` argument. */
  sealed abstract class WasmFuncInstr(
      mnemonic: String,
      opcode: Int,
      val funcArgument: WasmFunctionName
  ) extends WasmInstr(mnemonic, opcode)

  /** An instruction with a single `WasmTypeName` argument. */
  sealed abstract class WasmTypeInstr(mnemonic: String, opcode: Int, val typeArgument: WasmTypeName)
      extends WasmInstr(mnemonic, opcode)

  /** An instruction with a single `WasmTagName` argument. */
  sealed abstract class WasmTagInstr(mnemonic: String, opcode: Int, val tagArgument: WasmTagName)
      extends WasmInstr(mnemonic, opcode)

  /** An instruction with a single `WasmLocalName` argument. */
  sealed abstract class WasmLocalInstr(
      mnemonic: String,
      opcode: Int,
      val localArgument: WasmLocalName
  ) extends WasmInstr(mnemonic, opcode)

  /** An instruction with a single `WasmGlobalName` argument. */
  sealed abstract class WasmGlobalInstr(
      mnemonic: String,
      opcode: Int,
      val globalArgument: WasmGlobalName
  ) extends WasmInstr(mnemonic, opcode)

  /** An instruction with a single `WasmHeapType` argument. */
  sealed abstract class WasmHeapTypeInstr(
      mnemonic: String,
      opcode: Int,
      val heapTypeArgument: WasmHeapType
  ) extends WasmInstr(mnemonic, opcode)

  /** An instruction with a single `WasmRefType` argument
    *
    * In the binary format, it has split opcodes for the nullable and non-nullable variants.
    */
  sealed abstract class WasmRefTypeInstr(
      mnemonic: String,
      nonNullOpcode: Int,
      nullOpcode: Int,
      val refTypeArgument: WasmRefType
  ) extends WasmInstr(mnemonic, if (refTypeArgument.nullable) nullOpcode else nonNullOpcode)

  /** An instruction with a pair of `WasmTypeName`, `StructFieldIdx` arguments. */
  sealed abstract class WasmStructFieldInstr(
      mnemonic: String,
      opcode: Int,
      val structTypeName: WasmTypeName,
      val fieldIdx: WasmFieldIdx
  ) extends WasmInstr(mnemonic, opcode)

  // The actual instruction list

  // Unary operations
  case object I32_EQZ extends WasmSimpleInstr("i32.eqz", 0x45)
  case object I64_EQZ extends WasmSimpleInstr("i64.eqz", 0x50)
  case object I32_CLZ extends WasmSimpleInstr("i32.clz", 0x67)
  case object I32_CTZ extends WasmSimpleInstr("i32.ctz", 0x68)
  case object I32_POPCNT extends WasmSimpleInstr("i32.popcnt", 0x69)
  case object I64_CLZ extends WasmSimpleInstr("i64.clz", 0x79)
  case object I64_CTZ extends WasmSimpleInstr("i64.ctz", 0x7A)
  case object I64_POPCNT extends WasmSimpleInstr("i64.popcnt", 0x7B)
  case object F32_ABS extends WasmSimpleInstr("f32.abs", 0x8B)
  case object F32_NEG extends WasmSimpleInstr("f32.neg", 0x8C)
  case object F32_CEIL extends WasmSimpleInstr("f32.ceil", 0x8D)
  case object F32_FLOOR extends WasmSimpleInstr("f32.floor", 0x8E)
  case object F32_TRUNC extends WasmSimpleInstr("f32.trunc", 0x8F)
  case object F32_NEAREST extends WasmSimpleInstr("f32.nearest", 0x90)
  case object F32_SQRT extends WasmSimpleInstr("f32.sqrt", 0x91)
  case object F64_ABS extends WasmSimpleInstr("f64.abs", 0x99)
  case object F64_NEG extends WasmSimpleInstr("f64.neg", 0x9A)
  case object F64_CEIL extends WasmSimpleInstr("f64.ceil", 0x9B)
  case object F64_FLOOR extends WasmSimpleInstr("f64.floor", 0x9C)
  case object F64_TRUNC extends WasmSimpleInstr("f64.trunc", 0x9D)
  case object F64_NEAREST extends WasmSimpleInstr("f64.nearest", 0x9E)
  case object F64_SQRT extends WasmSimpleInstr("f64.sqrt", 0x9F)
  case object I32_WRAP_I64 extends WasmSimpleInstr("i32.wrap_i64", 0xA7)
  case object I32_TRUNC_F32_S extends WasmSimpleInstr("i32.trunc_f32_s", 0xA8)
  case object I32_TRUNC_F32_U extends WasmSimpleInstr("i32.trunc_f32_u", 0xA9)
  case object I32_TRUNC_F64_S extends WasmSimpleInstr("i32.trunc_f64_s", 0xAA)
  case object I32_TRUNC_F64_U extends WasmSimpleInstr("i32.trunc_f64_u", 0xAB)
  case object I64_EXTEND_I32_S extends WasmSimpleInstr("i64.extend_i32_s", 0xAC)
  case object I64_EXTEND_I32_U extends WasmSimpleInstr("i64.extend_i32_u", 0xAD)
  case object I64_TRUNC_F32_S extends WasmSimpleInstr("i64.trunc_f32_s", 0xAE)
  case object I64_TRUNC_F32_U extends WasmSimpleInstr("i64.trunc_f32_u", 0xAF)
  case object I64_TRUNC_F64_S extends WasmSimpleInstr("i64.trunc_f64_s", 0xB0)
  case object I64_TRUNC_F64_U extends WasmSimpleInstr("i64.trunc_f64_u", 0xB1)
  case object F32_CONVERT_I32_S extends WasmSimpleInstr("f32.convert_i32_s", 0xB2)
  case object F32_CONVERT_I32_U extends WasmSimpleInstr("f32.convert_i32_u", 0xB3)
  case object F32_CONVERT_I64_S extends WasmSimpleInstr("f32.convert_i64_s", 0xB4)
  case object F32_CONVERT_I64_U extends WasmSimpleInstr("f32.convert_i64_u", 0xB5)
  case object F32_DEMOTE_F64 extends WasmSimpleInstr("f32.demote_f64", 0xB6)
  case object F64_CONVERT_I32_S extends WasmSimpleInstr("f64.convert_i32_s", 0xB7)
  case object F64_CONVERT_I32_U extends WasmSimpleInstr("f64.convert_i32_u", 0xB8)
  case object F64_CONVERT_I64_S extends WasmSimpleInstr("f64.convert_i64_s", 0xB9)
  case object F64_CONVERT_I64_U extends WasmSimpleInstr("f64.convert_i64_u", 0xBA)
  case object F64_PROMOTE_F32 extends WasmSimpleInstr("f64.promote_f32", 0xBB)
  case object I32_REINTERPRET_F32 extends WasmSimpleInstr("i32.reinterpret_f32", 0xBC)
  case object I64_REINTERPRET_F64 extends WasmSimpleInstr("i64.reinterpret_f64", 0xBD)
  case object F32_REINTERPRET_I32 extends WasmSimpleInstr("f32.reinterpret_i32", 0xBE)
  case object F64_REINTERPRET_I64 extends WasmSimpleInstr("f64.reinterpret_i64", 0xBF)
  case object I32_EXTEND8_S extends WasmSimpleInstr("i32.extend8_s", 0xC0)
  case object I32_EXTEND16_S extends WasmSimpleInstr("i32.extend16_s", 0xC1)
  case object I64_EXTEND8_S extends WasmSimpleInstr("i64.extend8_s", 0xC2)
  case object I64_EXTEND16_S extends WasmSimpleInstr("i64.extend16_s", 0xC3)
  case object I64_EXTEND32_S extends WasmSimpleInstr("i64.extend32_s", 0xC4)
  case object I32_TRUNC_SAT_F64_S extends WasmSimpleInstr("i32.trunc_sat_f64_s", 0xFC02)
  case object I64_TRUNC_SAT_F64_S extends WasmSimpleInstr("i64.trunc_sat_f64_s", 0xFC06)

  // Binary operations
  case object I32_EQ extends WasmSimpleInstr("i32.eq", 0x46)
  case object I32_NE extends WasmSimpleInstr("i32.ne", 0x47)
  case object I32_LT_S extends WasmSimpleInstr("i32.lt_s", 0x48)
  case object I32_LT_U extends WasmSimpleInstr("i32.lt_u", 0x49)
  case object I32_GT_S extends WasmSimpleInstr("i32.gt_s", 0x4A)
  case object I32_GT_U extends WasmSimpleInstr("i32.gt_u", 0x4B)
  case object I32_LE_S extends WasmSimpleInstr("i32.le_s", 0x4C)
  case object I32_LE_U extends WasmSimpleInstr("i32.le_u", 0x4D)
  case object I32_GE_S extends WasmSimpleInstr("i32.ge_s", 0x4E)
  case object I32_GE_U extends WasmSimpleInstr("i32.ge_u", 0x4F)
  case object I64_EQ extends WasmSimpleInstr("i64.eq", 0x51)
  case object I64_NE extends WasmSimpleInstr("i64.ne", 0x52)
  case object I64_LT_S extends WasmSimpleInstr("i64.lt_s", 0x53)
  case object I64_LT_U extends WasmSimpleInstr("i64.lt_u", 0x54)
  case object I64_GT_S extends WasmSimpleInstr("i64.gt_s", 0x55)
  case object I64_GT_U extends WasmSimpleInstr("i64.gt_u", 0x56)
  case object I64_LE_S extends WasmSimpleInstr("i64.le_s", 0x57)
  case object I64_LE_U extends WasmSimpleInstr("i64.le_u", 0x58)
  case object I64_GE_S extends WasmSimpleInstr("i64.ge_s", 0x59)
  case object I64_GE_U extends WasmSimpleInstr("i64.ge_u", 0x5A)
  case object F32_EQ extends WasmSimpleInstr("f32.eq", 0x5B)
  case object F32_NE extends WasmSimpleInstr("f32.ne", 0x5C)
  case object F32_LT extends WasmSimpleInstr("f32.lt", 0x5D)
  case object F32_GT extends WasmSimpleInstr("f32.gt", 0x5E)
  case object F32_LE extends WasmSimpleInstr("f32.le", 0x5F)
  case object F32_GE extends WasmSimpleInstr("f32.ge", 0x60)
  case object F64_EQ extends WasmSimpleInstr("f64.eq", 0x61)
  case object F64_NE extends WasmSimpleInstr("f64.ne", 0x62)
  case object F64_LT extends WasmSimpleInstr("f64.lt", 0x63)
  case object F64_GT extends WasmSimpleInstr("f64.gt", 0x64)
  case object F64_LE extends WasmSimpleInstr("f64.le", 0x65)
  case object F64_GE extends WasmSimpleInstr("f64.ge", 0x66)
  case object I32_ADD extends WasmSimpleInstr("i32.add", 0x6A)
  case object I32_SUB extends WasmSimpleInstr("i32.sub", 0x6B)
  case object I32_MUL extends WasmSimpleInstr("i32.mul", 0x6C)
  case object I32_DIV_S extends WasmSimpleInstr("i32.div_s", 0x6D)
  case object I32_DIV_U extends WasmSimpleInstr("i32.div_u", 0x6E)
  case object I32_REM_S extends WasmSimpleInstr("i32.rem_s", 0x6F)
  case object I32_REM_U extends WasmSimpleInstr("i32.rem_u", 0x70)
  case object I32_AND extends WasmSimpleInstr("i32.and", 0x71)
  case object I32_OR extends WasmSimpleInstr("i32.or", 0x72)
  case object I32_XOR extends WasmSimpleInstr("i32.xor", 0x73)
  case object I32_SHL extends WasmSimpleInstr("i32.shl", 0x74)
  case object I32_SHR_S extends WasmSimpleInstr("i32.shr_s", 0x75)
  case object I32_SHR_U extends WasmSimpleInstr("i32.shr_u", 0x76)
  case object I32_ROTL extends WasmSimpleInstr("i32.rotl", 0x77)
  case object I32_ROTR extends WasmSimpleInstr("i32.rotr", 0x78)
  case object I64_ADD extends WasmSimpleInstr("i64.add", 0x7C)
  case object I64_SUB extends WasmSimpleInstr("i64.sub", 0x7D)
  case object I64_MUL extends WasmSimpleInstr("i64.mul", 0x7E)
  case object I64_DIV_S extends WasmSimpleInstr("i64.div_s", 0x7F)
  case object I64_DIV_U extends WasmSimpleInstr("i64.div_u", 0x80)
  case object I64_REM_S extends WasmSimpleInstr("i64.rem_s", 0x81)
  case object I64_REM_U extends WasmSimpleInstr("i64.rem_u", 0x82)
  case object I64_AND extends WasmSimpleInstr("i64.and", 0x83)
  case object I64_OR extends WasmSimpleInstr("i64.or", 0x84)
  case object I64_XOR extends WasmSimpleInstr("i64.xor", 0x85)
  case object I64_SHL extends WasmSimpleInstr("i64.shl", 0x86)
  case object I64_SHR_S extends WasmSimpleInstr("i64.shr_s", 0x87)
  case object I64_SHR_U extends WasmSimpleInstr("i64.shr_u", 0x88)
  case object I64_ROTL extends WasmSimpleInstr("i64.rotl", 0x89)
  case object I64_ROTR extends WasmSimpleInstr("i64.rotr", 0x8A)
  case object F32_ADD extends WasmSimpleInstr("f32.add", 0x92)
  case object F32_SUB extends WasmSimpleInstr("f32.sub", 0x93)
  case object F32_MUL extends WasmSimpleInstr("f32.mul", 0x94)
  case object F32_DIV extends WasmSimpleInstr("f32.div", 0x95)
  case object F32_MIN extends WasmSimpleInstr("f32.min", 0x96)
  case object F32_MAX extends WasmSimpleInstr("f32.max", 0x97)
  case object F32_COPYSIGN extends WasmSimpleInstr("f32.copysign", 0x98)
  case object F64_ADD extends WasmSimpleInstr("f64.add", 0xA0)
  case object F64_SUB extends WasmSimpleInstr("f64.sub", 0xA1)
  case object F64_MUL extends WasmSimpleInstr("f64.mul", 0xA2)
  case object F64_DIV extends WasmSimpleInstr("f64.div", 0xA3)
  case object F64_MIN extends WasmSimpleInstr("f64.min", 0xA4)
  case object F64_MAX extends WasmSimpleInstr("f64.max", 0xA5)

  case class I32_CONST(v: Int) extends WasmInstr("i32.const", 0x41)
  case class I64_CONST(v: Long) extends WasmInstr("i64.const", 0x42)
  case class F32_CONST(v: Float) extends WasmInstr("f32.const", 0x43)
  case class F64_CONST(v: Double) extends WasmInstr("f64.const", 0x44)

  // Control instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions
  case object UNREACHABLE extends WasmSimpleInstr("unreachable", 0x00) with StackPolymorphicInstr
  case object NOP extends WasmSimpleInstr("nop", 0x01)
  case class BLOCK(i: BlockType, label: Option[WasmLabelName])
      extends WasmBlockTypeLabeledInstr("block", 0x02, i)
  case class LOOP(i: BlockType, label: Option[WasmLabelName])
      extends WasmBlockTypeLabeledInstr("loop", 0x03, i)
  case class IF(i: BlockType, label: Option[WasmLabelName] = None)
      extends WasmBlockTypeLabeledInstr("if", 0x04, i)
  case object ELSE extends WasmSimpleInstr("else", 0x05)
  case object END extends WasmSimpleInstr("end", 0x0B)
  case class BR(i: WasmLabelName) extends WasmLabelInstr("br", 0x0C, i) with StackPolymorphicInstr
  case class BR_IF(i: WasmLabelName) extends WasmLabelInstr("br_if", 0x0D, i)
  case class BR_TABLE(table: List[WasmLabelName], default: WasmLabelName)
      extends WasmInstr("br_table", 0x0E)
      with StackPolymorphicInstr
  case object RETURN extends WasmSimpleInstr("return", 0x0F) with StackPolymorphicInstr
  case class CALL(i: WasmFunctionName) extends WasmFuncInstr("call", 0x10, i)
  case class THROW(i: WasmTagName) extends WasmTagInstr("throw", 0x08, i) with StackPolymorphicInstr
  case object THROW_REF extends WasmSimpleInstr("throw_ref", 0x0A) with StackPolymorphicInstr
  case class TRY_TABLE(i: BlockType, cs: List[CatchClause], label: Option[WasmLabelName] = None)
      extends WasmInstr("try_table", 0x1F)
      with StructuredLabeledInstr

  // Parametric instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#parametric-instructions
  case object DROP extends WasmSimpleInstr("drop", 0x1A)
  // TODO: SELECT

  // Variable instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#variable-instructions
  case class LOCAL_GET(i: WasmLocalName) extends WasmLocalInstr("local.get", 0x20, i)
  case class LOCAL_SET(i: WasmLocalName) extends WasmLocalInstr("local.set", 0x21, i)
  case class LOCAL_TEE(i: WasmLocalName) extends WasmLocalInstr("local.tee", 0x22, i)
  case class GLOBAL_GET(i: WasmGlobalName) extends WasmGlobalInstr("global.get", 0x23, i)
  case class GLOBAL_SET(i: WasmGlobalName) extends WasmGlobalInstr("global.set", 0x24, i)

  // Table instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#table-instructions
  // impolement when it's needed

  // Memory instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#memory-instructions

  // ============================================================
  // Reference types
  /** evaluates to the null reference constant.
    *
    * `ref.null rt : [] -> [rtref]` (iff rt = func or rt = extern)
    */
  case class REF_NULL(i: WasmHeapType) extends WasmHeapTypeInstr("ref.null", 0xD0, i)

  /** checks for null. ref.is_null : [rtref] -> [i32]
    */
  case object REF_IS_NULL extends WasmSimpleInstr("ref.is_null", 0xD1)

  /** creates a reference to a given function. `ref.func $x : [] -> [funcref]` (iff $x : func $t)
    */
  case class REF_FUNC(i: WasmFunctionName) extends WasmFuncInstr("ref.func", 0xD2, i)

  case object REF_I31 extends WasmSimpleInstr("ref.i31", 0xFB1C)
  case object I31_GET_S extends WasmSimpleInstr("i31.get_s", 0xFB1D)
  case object I31_GET_U extends WasmSimpleInstr("i31.get_u", 0xFB1E)

  // ============================================================
  // Typed Function References
  // https://github.com/WebAssembly/function-references
  case class CALL_REF(i: WasmTypeName) extends WasmTypeInstr("call_ref", 0x14, i)
  case class RETURN_CALL_REF(i: WasmTypeName) extends WasmTypeInstr("return_call_ref", 0x15, i)
  case object REF_AS_NOT_NULL extends WasmSimpleInstr("ref.as_non_null", 0xD4)
  case class BR_ON_NULL(i: WasmLabelName) extends WasmLabelInstr("br_on_null", 0xD5, i)
  case class BR_ON_NON_NULL(i: WasmLabelName) extends WasmLabelInstr("br_on_non_null", 0xD6, i)

  // ============================================================
  // gc
  case class STRUCT_NEW(i: WasmTypeName) extends WasmTypeInstr("struct.new", 0xFB00, i)
  case class STRUCT_NEW_DEFAULT(i: WasmTypeName)
      extends WasmTypeInstr("struct.new_default", 0xFB01, i)
  case class STRUCT_GET(tyidx: WasmTypeName, fidx: WasmFieldIdx)
      extends WasmStructFieldInstr("struct.get", 0xFB02, tyidx, fidx)
  // STRUCT_GET_S
  // STRUCT_GET_U
  case class STRUCT_SET(tyidx: WasmTypeName, fidx: WasmFieldIdx)
      extends WasmStructFieldInstr("struct.set", 0xFB05, tyidx, fidx)

  case class ARRAY_NEW(i: WasmTypeName) extends WasmTypeInstr("array.new", 0xFB06, i)
  case class ARRAY_NEW_DEFAULT(i: WasmTypeName)
      extends WasmTypeInstr("array.new_default", 0xFB07, i)
  case class ARRAY_NEW_FIXED(i: WasmTypeName, size: Int)
      extends WasmInstr("array.new_fixed", 0xFB08)
  case class ARRAY_NEW_DATA(i: WasmTypeName, d: WasmDataName)
      extends WasmInstr("array.new_data", 0xFB09)
  case class ARRAY_GET(i: WasmTypeName) extends WasmTypeInstr("array.get", 0xFB0B, i)
  case class ARRAY_GET_S(i: WasmTypeName) extends WasmTypeInstr("array.get_s", 0xFB0C, i)
  case class ARRAY_GET_U(i: WasmTypeName) extends WasmTypeInstr("array.get_u", 0xFB0D, i)
  case class ARRAY_SET(i: WasmTypeName) extends WasmTypeInstr("array.set", 0xFB0E, i)
  case object ARRAY_LEN extends WasmSimpleInstr("array.len", 0xFB0F)
  // ARRAY_FILL,
  // ARRAY_COPY
  // ARRAY_NEW_DATA
  // array_NEW_FIXED

  case object REF_EQ extends WasmSimpleInstr("ref.eq", 0xD3)
  case class REF_TEST(i: WasmRefType) extends WasmRefTypeInstr("ref.test", 0xFB14, 0xFB15, i)
  case class REF_CAST(i: WasmRefType) extends WasmRefTypeInstr("ref.cast", 0xFB16, 0xFB17, i)

  case class BR_ON_CAST(label: WasmLabelName, from: WasmRefType, to: WasmRefType)
      extends WasmInstr("br_on_cast", 0xFB18)
  case class BR_ON_CAST_FAIL(label: WasmLabelName, from: WasmRefType, to: WasmRefType)
      extends WasmInstr("br_on_cast_fail", 0xFB19)

  // Catch clauses for TRY_TABLE

  sealed abstract class CatchClause(
      val mnemonic: String,
      val opcode: Int,
      val tag: Option[WasmTagName],
      val label: WasmLabelName
  )

  object CatchClause {
    case class Catch(x: WasmTagName, l: WasmLabelName)
        extends CatchClause("catch", 0x00, Some(x), l)
    case class CatchRef(x: WasmTagName, l: WasmLabelName)
        extends CatchClause("catch_ref", 0x01, Some(x), l)
    case class CatchAll(l: WasmLabelName) extends CatchClause("catch_all", 0x02, None, l)
    case class CatchAllRef(l: WasmLabelName) extends CatchClause("catch_all_ref", 0x03, None, l)
  }
}

object WasmImmediate {

  /** A structured instruction can consume input and produce output on the operand stack according
    * to its annotated block type. It is given either as a type index that refers to a suitable
    * function type, or as an optional value type inline, which is a shorthand for the function type
    * [] -> [valtype]
    * @see
    *   https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions
    */
  abstract sealed trait BlockType
  object BlockType {
    case class FunctionType(ty: WasmFunctionTypeName) extends BlockType
    case class ValueType private (ty: Option[WasmType]) extends BlockType
    object ValueType {
      def apply(ty: WasmType): ValueType = ValueType(Some(ty))
      def apply(): ValueType = ValueType(None)
    }
  }
}
