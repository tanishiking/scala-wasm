package org.scalajs.linker.backend.webassembly

import org.scalajs.ir.Position

import Names._
import Types._

/** WebAssembly instructions.
  *
  * @see
  *   [[https://webassembly.github.io/spec/core/syntax/instructions.html]]
  */
object Instructions {
  sealed abstract class Instr(val mnemonic: String, val opcode: Int)

  // Semantic categories of instructions

  /** A stack-polymorphic instruction. */
  sealed trait StackPolymorphicInstr extends Instr

  /** An instruction that opens a structured control block. */
  sealed trait StructuredLabeledInstr extends Instr {
    val label: Option[LabelName]
  }

  // Convenience subclasses of instructions for writing text/binary

  /** An instruction without any immediate argument. */
  sealed abstract class SimpleInstr(mnemonic: String, opcode: Int) extends Instr(mnemonic, opcode)

  /** A structured labeled instruction with a single `BlockType` argument. */
  sealed abstract class BlockTypeLabeledInstr(
      mnemonic: String,
      opcode: Int,
      val blockTypeArgument: BlockType
  ) extends Instr(mnemonic, opcode)
      with StructuredLabeledInstr

  /** An instruction with a single `LabelIdx` argument. */
  sealed abstract class LabelInstr(
      mnemonic: String,
      opcode: Int,
      val labelArgument: LabelName
  ) extends Instr(mnemonic, opcode)

  /** An instruction with a single `FunctionName` argument. */
  sealed abstract class FuncInstr(
      mnemonic: String,
      opcode: Int,
      val funcArgument: FunctionName
  ) extends Instr(mnemonic, opcode)

  /** An instruction with a single `TypeName` argument. */
  sealed abstract class TypeInstr(mnemonic: String, opcode: Int, val typeArgument: TypeName)
      extends Instr(mnemonic, opcode)

  /** An instruction with a single `TagName` argument. */
  sealed abstract class TagInstr(mnemonic: String, opcode: Int, val tagArgument: TagName)
      extends Instr(mnemonic, opcode)

  /** An instruction with a single `LocalName` argument. */
  sealed abstract class LocalInstr(
      mnemonic: String,
      opcode: Int,
      val localArgument: LocalName
  ) extends Instr(mnemonic, opcode)

  /** An instruction with a single `GlobalName` argument. */
  sealed abstract class GlobalInstr(
      mnemonic: String,
      opcode: Int,
      val globalArgument: GlobalName
  ) extends Instr(mnemonic, opcode)

  /** An instruction with a single `HeapType` argument. */
  sealed abstract class HeapTypeInstr(
      mnemonic: String,
      opcode: Int,
      val heapTypeArgument: HeapType
  ) extends Instr(mnemonic, opcode)

  /** An instruction with a single `RefType` argument
    *
    * In the binary format, it has split opcodes for the nullable and non-nullable variants.
    */
  sealed abstract class RefTypeInstr(
      mnemonic: String,
      nonNullOpcode: Int,
      nullOpcode: Int,
      val refTypeArgument: RefType
  ) extends Instr(mnemonic, if (refTypeArgument.nullable) nullOpcode else nonNullOpcode)

  /** An instruction with a pair of `TypeName`, `StructFieldIdx` arguments. */
  sealed abstract class StructFieldInstr(
      mnemonic: String,
      opcode: Int,
      val structTypeName: TypeName,
      val fieldIdx: FieldIdx
  ) extends Instr(mnemonic, opcode)

  // The actual instruction list

  // Fake instruction to mark position changes
  final case class PositionMark(pos: Position) extends Instr("pos", -1)

  // Unary operations
  case object I32_EQZ extends SimpleInstr("i32.eqz", 0x45)
  case object I64_EQZ extends SimpleInstr("i64.eqz", 0x50)
  case object I32_CLZ extends SimpleInstr("i32.clz", 0x67)
  case object I32_CTZ extends SimpleInstr("i32.ctz", 0x68)
  case object I32_POPCNT extends SimpleInstr("i32.popcnt", 0x69)
  case object I64_CLZ extends SimpleInstr("i64.clz", 0x79)
  case object I64_CTZ extends SimpleInstr("i64.ctz", 0x7A)
  case object I64_POPCNT extends SimpleInstr("i64.popcnt", 0x7B)
  case object F32_ABS extends SimpleInstr("f32.abs", 0x8B)
  case object F32_NEG extends SimpleInstr("f32.neg", 0x8C)
  case object F32_CEIL extends SimpleInstr("f32.ceil", 0x8D)
  case object F32_FLOOR extends SimpleInstr("f32.floor", 0x8E)
  case object F32_TRUNC extends SimpleInstr("f32.trunc", 0x8F)
  case object F32_NEAREST extends SimpleInstr("f32.nearest", 0x90)
  case object F32_SQRT extends SimpleInstr("f32.sqrt", 0x91)
  case object F64_ABS extends SimpleInstr("f64.abs", 0x99)
  case object F64_NEG extends SimpleInstr("f64.neg", 0x9A)
  case object F64_CEIL extends SimpleInstr("f64.ceil", 0x9B)
  case object F64_FLOOR extends SimpleInstr("f64.floor", 0x9C)
  case object F64_TRUNC extends SimpleInstr("f64.trunc", 0x9D)
  case object F64_NEAREST extends SimpleInstr("f64.nearest", 0x9E)
  case object F64_SQRT extends SimpleInstr("f64.sqrt", 0x9F)
  case object I32_WRAP_I64 extends SimpleInstr("i32.wrap_i64", 0xA7)
  case object I32_TRUNC_F32_S extends SimpleInstr("i32.trunc_f32_s", 0xA8)
  case object I32_TRUNC_F32_U extends SimpleInstr("i32.trunc_f32_u", 0xA9)
  case object I32_TRUNC_F64_S extends SimpleInstr("i32.trunc_f64_s", 0xAA)
  case object I32_TRUNC_F64_U extends SimpleInstr("i32.trunc_f64_u", 0xAB)
  case object I64_EXTEND_I32_S extends SimpleInstr("i64.extend_i32_s", 0xAC)
  case object I64_EXTEND_I32_U extends SimpleInstr("i64.extend_i32_u", 0xAD)
  case object I64_TRUNC_F32_S extends SimpleInstr("i64.trunc_f32_s", 0xAE)
  case object I64_TRUNC_F32_U extends SimpleInstr("i64.trunc_f32_u", 0xAF)
  case object I64_TRUNC_F64_S extends SimpleInstr("i64.trunc_f64_s", 0xB0)
  case object I64_TRUNC_F64_U extends SimpleInstr("i64.trunc_f64_u", 0xB1)
  case object F32_CONVERT_I32_S extends SimpleInstr("f32.convert_i32_s", 0xB2)
  case object F32_CONVERT_I32_U extends SimpleInstr("f32.convert_i32_u", 0xB3)
  case object F32_CONVERT_I64_S extends SimpleInstr("f32.convert_i64_s", 0xB4)
  case object F32_CONVERT_I64_U extends SimpleInstr("f32.convert_i64_u", 0xB5)
  case object F32_DEMOTE_F64 extends SimpleInstr("f32.demote_f64", 0xB6)
  case object F64_CONVERT_I32_S extends SimpleInstr("f64.convert_i32_s", 0xB7)
  case object F64_CONVERT_I32_U extends SimpleInstr("f64.convert_i32_u", 0xB8)
  case object F64_CONVERT_I64_S extends SimpleInstr("f64.convert_i64_s", 0xB9)
  case object F64_CONVERT_I64_U extends SimpleInstr("f64.convert_i64_u", 0xBA)
  case object F64_PROMOTE_F32 extends SimpleInstr("f64.promote_f32", 0xBB)
  case object I32_REINTERPRET_F32 extends SimpleInstr("i32.reinterpret_f32", 0xBC)
  case object I64_REINTERPRET_F64 extends SimpleInstr("i64.reinterpret_f64", 0xBD)
  case object F32_REINTERPRET_I32 extends SimpleInstr("f32.reinterpret_i32", 0xBE)
  case object F64_REINTERPRET_I64 extends SimpleInstr("f64.reinterpret_i64", 0xBF)
  case object I32_EXTEND8_S extends SimpleInstr("i32.extend8_s", 0xC0)
  case object I32_EXTEND16_S extends SimpleInstr("i32.extend16_s", 0xC1)
  case object I64_EXTEND8_S extends SimpleInstr("i64.extend8_s", 0xC2)
  case object I64_EXTEND16_S extends SimpleInstr("i64.extend16_s", 0xC3)
  case object I64_EXTEND32_S extends SimpleInstr("i64.extend32_s", 0xC4)
  case object I32_TRUNC_SAT_F64_S extends SimpleInstr("i32.trunc_sat_f64_s", 0xFC02)
  case object I64_TRUNC_SAT_F64_S extends SimpleInstr("i64.trunc_sat_f64_s", 0xFC06)

  // Binary operations
  case object I32_EQ extends SimpleInstr("i32.eq", 0x46)
  case object I32_NE extends SimpleInstr("i32.ne", 0x47)
  case object I32_LT_S extends SimpleInstr("i32.lt_s", 0x48)
  case object I32_LT_U extends SimpleInstr("i32.lt_u", 0x49)
  case object I32_GT_S extends SimpleInstr("i32.gt_s", 0x4A)
  case object I32_GT_U extends SimpleInstr("i32.gt_u", 0x4B)
  case object I32_LE_S extends SimpleInstr("i32.le_s", 0x4C)
  case object I32_LE_U extends SimpleInstr("i32.le_u", 0x4D)
  case object I32_GE_S extends SimpleInstr("i32.ge_s", 0x4E)
  case object I32_GE_U extends SimpleInstr("i32.ge_u", 0x4F)
  case object I64_EQ extends SimpleInstr("i64.eq", 0x51)
  case object I64_NE extends SimpleInstr("i64.ne", 0x52)
  case object I64_LT_S extends SimpleInstr("i64.lt_s", 0x53)
  case object I64_LT_U extends SimpleInstr("i64.lt_u", 0x54)
  case object I64_GT_S extends SimpleInstr("i64.gt_s", 0x55)
  case object I64_GT_U extends SimpleInstr("i64.gt_u", 0x56)
  case object I64_LE_S extends SimpleInstr("i64.le_s", 0x57)
  case object I64_LE_U extends SimpleInstr("i64.le_u", 0x58)
  case object I64_GE_S extends SimpleInstr("i64.ge_s", 0x59)
  case object I64_GE_U extends SimpleInstr("i64.ge_u", 0x5A)
  case object F32_EQ extends SimpleInstr("f32.eq", 0x5B)
  case object F32_NE extends SimpleInstr("f32.ne", 0x5C)
  case object F32_LT extends SimpleInstr("f32.lt", 0x5D)
  case object F32_GT extends SimpleInstr("f32.gt", 0x5E)
  case object F32_LE extends SimpleInstr("f32.le", 0x5F)
  case object F32_GE extends SimpleInstr("f32.ge", 0x60)
  case object F64_EQ extends SimpleInstr("f64.eq", 0x61)
  case object F64_NE extends SimpleInstr("f64.ne", 0x62)
  case object F64_LT extends SimpleInstr("f64.lt", 0x63)
  case object F64_GT extends SimpleInstr("f64.gt", 0x64)
  case object F64_LE extends SimpleInstr("f64.le", 0x65)
  case object F64_GE extends SimpleInstr("f64.ge", 0x66)
  case object I32_ADD extends SimpleInstr("i32.add", 0x6A)
  case object I32_SUB extends SimpleInstr("i32.sub", 0x6B)
  case object I32_MUL extends SimpleInstr("i32.mul", 0x6C)
  case object I32_DIV_S extends SimpleInstr("i32.div_s", 0x6D)
  case object I32_DIV_U extends SimpleInstr("i32.div_u", 0x6E)
  case object I32_REM_S extends SimpleInstr("i32.rem_s", 0x6F)
  case object I32_REM_U extends SimpleInstr("i32.rem_u", 0x70)
  case object I32_AND extends SimpleInstr("i32.and", 0x71)
  case object I32_OR extends SimpleInstr("i32.or", 0x72)
  case object I32_XOR extends SimpleInstr("i32.xor", 0x73)
  case object I32_SHL extends SimpleInstr("i32.shl", 0x74)
  case object I32_SHR_S extends SimpleInstr("i32.shr_s", 0x75)
  case object I32_SHR_U extends SimpleInstr("i32.shr_u", 0x76)
  case object I32_ROTL extends SimpleInstr("i32.rotl", 0x77)
  case object I32_ROTR extends SimpleInstr("i32.rotr", 0x78)
  case object I64_ADD extends SimpleInstr("i64.add", 0x7C)
  case object I64_SUB extends SimpleInstr("i64.sub", 0x7D)
  case object I64_MUL extends SimpleInstr("i64.mul", 0x7E)
  case object I64_DIV_S extends SimpleInstr("i64.div_s", 0x7F)
  case object I64_DIV_U extends SimpleInstr("i64.div_u", 0x80)
  case object I64_REM_S extends SimpleInstr("i64.rem_s", 0x81)
  case object I64_REM_U extends SimpleInstr("i64.rem_u", 0x82)
  case object I64_AND extends SimpleInstr("i64.and", 0x83)
  case object I64_OR extends SimpleInstr("i64.or", 0x84)
  case object I64_XOR extends SimpleInstr("i64.xor", 0x85)
  case object I64_SHL extends SimpleInstr("i64.shl", 0x86)
  case object I64_SHR_S extends SimpleInstr("i64.shr_s", 0x87)
  case object I64_SHR_U extends SimpleInstr("i64.shr_u", 0x88)
  case object I64_ROTL extends SimpleInstr("i64.rotl", 0x89)
  case object I64_ROTR extends SimpleInstr("i64.rotr", 0x8A)
  case object F32_ADD extends SimpleInstr("f32.add", 0x92)
  case object F32_SUB extends SimpleInstr("f32.sub", 0x93)
  case object F32_MUL extends SimpleInstr("f32.mul", 0x94)
  case object F32_DIV extends SimpleInstr("f32.div", 0x95)
  case object F32_MIN extends SimpleInstr("f32.min", 0x96)
  case object F32_MAX extends SimpleInstr("f32.max", 0x97)
  case object F32_COPYSIGN extends SimpleInstr("f32.copysign", 0x98)
  case object F64_ADD extends SimpleInstr("f64.add", 0xA0)
  case object F64_SUB extends SimpleInstr("f64.sub", 0xA1)
  case object F64_MUL extends SimpleInstr("f64.mul", 0xA2)
  case object F64_DIV extends SimpleInstr("f64.div", 0xA3)
  case object F64_MIN extends SimpleInstr("f64.min", 0xA4)
  case object F64_MAX extends SimpleInstr("f64.max", 0xA5)

  case class I32_CONST(v: Int) extends Instr("i32.const", 0x41)
  case class I64_CONST(v: Long) extends Instr("i64.const", 0x42)
  case class F32_CONST(v: Float) extends Instr("f32.const", 0x43)
  case class F64_CONST(v: Double) extends Instr("f64.const", 0x44)

  // Control instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions
  case object UNREACHABLE extends SimpleInstr("unreachable", 0x00) with StackPolymorphicInstr
  case object NOP extends SimpleInstr("nop", 0x01)
  case class BLOCK(i: BlockType, label: Option[LabelName])
      extends BlockTypeLabeledInstr("block", 0x02, i)
  case class LOOP(i: BlockType, label: Option[LabelName])
      extends BlockTypeLabeledInstr("loop", 0x03, i)
  case class IF(i: BlockType, label: Option[LabelName] = None)
      extends BlockTypeLabeledInstr("if", 0x04, i)
  case object ELSE extends SimpleInstr("else", 0x05)
  case object END extends SimpleInstr("end", 0x0B)
  case class BR(i: LabelName) extends LabelInstr("br", 0x0C, i) with StackPolymorphicInstr
  case class BR_IF(i: LabelName) extends LabelInstr("br_if", 0x0D, i)
  case class BR_TABLE(table: List[LabelName], default: LabelName)
      extends Instr("br_table", 0x0E)
      with StackPolymorphicInstr
  case object RETURN extends SimpleInstr("return", 0x0F) with StackPolymorphicInstr
  case class CALL(i: FunctionName) extends FuncInstr("call", 0x10, i)
  case class RETURN_CALL(i: FunctionName) extends FuncInstr("return_call", 0x12, i)
  case class THROW(i: TagName) extends TagInstr("throw", 0x08, i) with StackPolymorphicInstr
  case object THROW_REF extends SimpleInstr("throw_ref", 0x0A) with StackPolymorphicInstr
  case class TRY_TABLE(i: BlockType, cs: List[CatchClause], label: Option[LabelName] = None)
      extends Instr("try_table", 0x1F)
      with StructuredLabeledInstr

  // Legacy exception system
  case class TRY(i: BlockType, label: Option[LabelName] = None)
      extends BlockTypeLabeledInstr("try", 0x06, i)
  case class CATCH(i: TagName) extends TagInstr("catch", 0x07, i)
  case object CATCH_ALL extends SimpleInstr("catch_all", 0x19)
  // case class DELEGATE(i: LabelName) extends LabelInstr("delegate", 0x18, i)
  case class RETHROW(i: LabelName) extends LabelInstr("rethrow", 0x09, i) with StackPolymorphicInstr

  // Parametric instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#parametric-instructions
  case object DROP extends SimpleInstr("drop", 0x1A)
  // TODO: SELECT

  // Variable instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#variable-instructions
  case class LOCAL_GET(i: LocalName) extends LocalInstr("local.get", 0x20, i)
  case class LOCAL_SET(i: LocalName) extends LocalInstr("local.set", 0x21, i)
  case class LOCAL_TEE(i: LocalName) extends LocalInstr("local.tee", 0x22, i)
  case class GLOBAL_GET(i: GlobalName) extends GlobalInstr("global.get", 0x23, i)
  case class GLOBAL_SET(i: GlobalName) extends GlobalInstr("global.set", 0x24, i)

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
  case class REF_NULL(i: HeapType) extends HeapTypeInstr("ref.null", 0xD0, i)

  /** checks for null. ref.is_null : [rtref] -> [i32]
    */
  case object REF_IS_NULL extends SimpleInstr("ref.is_null", 0xD1)

  /** creates a reference to a given function.
    *
    * `ref.func $$x : [] -> [funcref]` (iff $$x : func $$t)
    */
  case class REF_FUNC(i: FunctionName) extends FuncInstr("ref.func", 0xD2, i)

  case object ANY_CONVERT_EXTERN extends SimpleInstr("any.convert_extern", 0xFB1A)
  case object EXTERN_CONVERT_ANY extends SimpleInstr("extern.convert_any", 0xFB1B)

  case object REF_I31 extends SimpleInstr("ref.i31", 0xFB1C)
  case object I31_GET_S extends SimpleInstr("i31.get_s", 0xFB1D)
  case object I31_GET_U extends SimpleInstr("i31.get_u", 0xFB1E)

  // ============================================================
  // Typed Function References
  // https://github.com/WebAssembly/function-references
  case class CALL_REF(i: TypeName) extends TypeInstr("call_ref", 0x14, i)
  case class RETURN_CALL_REF(i: TypeName) extends TypeInstr("return_call_ref", 0x15, i)
  case object REF_AS_NOT_NULL extends SimpleInstr("ref.as_non_null", 0xD4)
  case class BR_ON_NULL(i: LabelName) extends LabelInstr("br_on_null", 0xD5, i)
  case class BR_ON_NON_NULL(i: LabelName) extends LabelInstr("br_on_non_null", 0xD6, i)

  // ============================================================
  // gc
  case class STRUCT_NEW(i: TypeName) extends TypeInstr("struct.new", 0xFB00, i)
  case class STRUCT_NEW_DEFAULT(i: TypeName) extends TypeInstr("struct.new_default", 0xFB01, i)
  case class STRUCT_GET(tyidx: TypeName, fidx: FieldIdx)
      extends StructFieldInstr("struct.get", 0xFB02, tyidx, fidx)
  // STRUCT_GET_S
  // STRUCT_GET_U
  case class STRUCT_SET(tyidx: TypeName, fidx: FieldIdx)
      extends StructFieldInstr("struct.set", 0xFB05, tyidx, fidx)

  case class ARRAY_NEW(i: TypeName) extends TypeInstr("array.new", 0xFB06, i)
  case class ARRAY_NEW_DEFAULT(i: TypeName) extends TypeInstr("array.new_default", 0xFB07, i)
  case class ARRAY_NEW_FIXED(i: TypeName, size: Int) extends Instr("array.new_fixed", 0xFB08)
  case class ARRAY_NEW_DATA(i: TypeName, d: DataName) extends Instr("array.new_data", 0xFB09)
  case class ARRAY_GET(i: TypeName) extends TypeInstr("array.get", 0xFB0B, i)
  case class ARRAY_GET_S(i: TypeName) extends TypeInstr("array.get_s", 0xFB0C, i)
  case class ARRAY_GET_U(i: TypeName) extends TypeInstr("array.get_u", 0xFB0D, i)
  case class ARRAY_SET(i: TypeName) extends TypeInstr("array.set", 0xFB0E, i)
  case object ARRAY_LEN extends SimpleInstr("array.len", 0xFB0F)
  // ARRAY_FILL,
  case class ARRAY_COPY(destType: TypeName, srcType: TypeName) extends Instr("array.copy", 0xFB11)
  // ARRAY_NEW_DATA
  // array_NEW_FIXED

  case object REF_EQ extends SimpleInstr("ref.eq", 0xD3)
  case class REF_TEST(i: RefType) extends RefTypeInstr("ref.test", 0xFB14, 0xFB15, i)
  case class REF_CAST(i: RefType) extends RefTypeInstr("ref.cast", 0xFB16, 0xFB17, i)

  case class BR_ON_CAST(label: LabelName, from: RefType, to: RefType)
      extends Instr("br_on_cast", 0xFB18)
  case class BR_ON_CAST_FAIL(label: LabelName, from: RefType, to: RefType)
      extends Instr("br_on_cast_fail", 0xFB19)

  // Catch clauses for TRY_TABLE

  sealed abstract class CatchClause(
      val mnemonic: String,
      val opcode: Int,
      val tag: Option[TagName],
      val label: LabelName
  )

  object CatchClause {
    case class Catch(x: TagName, l: LabelName) extends CatchClause("catch", 0x00, Some(x), l)
    case class CatchRef(x: TagName, l: LabelName) extends CatchClause("catch_ref", 0x01, Some(x), l)
    case class CatchAll(l: LabelName) extends CatchClause("catch_all", 0x02, None, l)
    case class CatchAllRef(l: LabelName) extends CatchClause("catch_all_ref", 0x03, None, l)
  }

  // Block types

  /** A structured instruction can consume input and produce output on the operand stack according
    * to its annotated block type. It is given either as a type index that refers to a suitable
    * function type, or as an optional value type inline, which is a shorthand for the function type
    * [] -> [valtype]
    * @see
    *   https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions
    */
  sealed abstract class BlockType
  object BlockType {
    case class FunctionType(ty: TypeName) extends BlockType
    case class ValueType(ty: Option[Type]) extends BlockType
    object ValueType {
      def apply(ty: Type): ValueType = ValueType(Some(ty))
      def apply(): ValueType = ValueType(None)
    }
  }
}
