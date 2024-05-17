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

  /** A WebAssembly `expr`. */
  sealed case class Expr(instr: List[Instr])

  /** A WebAssembly `instr`. */
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
  case object I32Eqz extends SimpleInstr("i32.eqz", 0x45)
  case object I64Eqz extends SimpleInstr("i64.eqz", 0x50)
  case object I32Clz extends SimpleInstr("i32.clz", 0x67)
  case object I32Ctz extends SimpleInstr("i32.ctz", 0x68)
  case object I32Popcnt extends SimpleInstr("i32.popcnt", 0x69)
  case object I64Clz extends SimpleInstr("i64.clz", 0x79)
  case object I64Ctz extends SimpleInstr("i64.ctz", 0x7A)
  case object I64Popcnt extends SimpleInstr("i64.popcnt", 0x7B)
  case object F32Abs extends SimpleInstr("f32.abs", 0x8B)
  case object F32Neg extends SimpleInstr("f32.neg", 0x8C)
  case object F32Ceil extends SimpleInstr("f32.ceil", 0x8D)
  case object F32Floor extends SimpleInstr("f32.floor", 0x8E)
  case object F32Trunc extends SimpleInstr("f32.trunc", 0x8F)
  case object F32Nearest extends SimpleInstr("f32.nearest", 0x90)
  case object F32Sqrt extends SimpleInstr("f32.sqrt", 0x91)
  case object F64Abs extends SimpleInstr("f64.abs", 0x99)
  case object F64Neg extends SimpleInstr("f64.neg", 0x9A)
  case object F64Ceil extends SimpleInstr("f64.ceil", 0x9B)
  case object F64Floor extends SimpleInstr("f64.floor", 0x9C)
  case object F64Trunc extends SimpleInstr("f64.trunc", 0x9D)
  case object F64Nearest extends SimpleInstr("f64.nearest", 0x9E)
  case object F64Sqrt extends SimpleInstr("f64.sqrt", 0x9F)
  case object I32WrapI64 extends SimpleInstr("i32.wrap_i64", 0xA7)
  case object I32TruncF32S extends SimpleInstr("i32.trunc_f32_s", 0xA8)
  case object I32TruncF32U extends SimpleInstr("i32.trunc_f32_u", 0xA9)
  case object I32TruncF64S extends SimpleInstr("i32.trunc_f64_s", 0xAA)
  case object I32TruncF64U extends SimpleInstr("i32.trunc_f64_u", 0xAB)
  case object I64ExtendI32S extends SimpleInstr("i64.extend_i32_s", 0xAC)
  case object I64ExtendI32U extends SimpleInstr("i64.extend_i32_u", 0xAD)
  case object I64TruncF32S extends SimpleInstr("i64.trunc_f32_s", 0xAE)
  case object I64TruncF32U extends SimpleInstr("i64.trunc_f32_u", 0xAF)
  case object I64TruncF64S extends SimpleInstr("i64.trunc_f64_s", 0xB0)
  case object I64TruncF64U extends SimpleInstr("i64.trunc_f64_u", 0xB1)
  case object F32ConvertI32S extends SimpleInstr("f32.convert_i32_s", 0xB2)
  case object F32ConvertI32U extends SimpleInstr("f32.convert_i32_u", 0xB3)
  case object F32ConvertI64S extends SimpleInstr("f32.convert_i64_s", 0xB4)
  case object F32ConvertI64U extends SimpleInstr("f32.convert_i64_u", 0xB5)
  case object F32DemoteF64 extends SimpleInstr("f32.demote_f64", 0xB6)
  case object F64ConvertI32S extends SimpleInstr("f64.convert_i32_s", 0xB7)
  case object F64ConvertI32U extends SimpleInstr("f64.convert_i32_u", 0xB8)
  case object F64ConvertI64S extends SimpleInstr("f64.convert_i64_s", 0xB9)
  case object F64ConvertI64U extends SimpleInstr("f64.convert_i64_u", 0xBA)
  case object F64PromoteF32 extends SimpleInstr("f64.promote_f32", 0xBB)
  case object I32ReinterpretF32 extends SimpleInstr("i32.reinterpret_f32", 0xBC)
  case object I64ReinterpretF64 extends SimpleInstr("i64.reinterpret_f64", 0xBD)
  case object F32ReinterpretI32 extends SimpleInstr("f32.reinterpret_i32", 0xBE)
  case object F64ReinterpretI64 extends SimpleInstr("f64.reinterpret_i64", 0xBF)
  case object I32Extend8S extends SimpleInstr("i32.extend8_s", 0xC0)
  case object I32Extend16S extends SimpleInstr("i32.extend16_s", 0xC1)
  case object I64Extend8S extends SimpleInstr("i64.extend8_s", 0xC2)
  case object I64Extend16S extends SimpleInstr("i64.extend16_s", 0xC3)
  case object I64Extend32S extends SimpleInstr("i64.extend32_s", 0xC4)
  case object I32TruncSatF64S extends SimpleInstr("i32.trunc_sat_f64_s", 0xFC02)
  case object I64TruncSatF64S extends SimpleInstr("i64.trunc_sat_f64_s", 0xFC06)

  // Binary operations
  case object I32Eq extends SimpleInstr("i32.eq", 0x46)
  case object I32Ne extends SimpleInstr("i32.ne", 0x47)
  case object I32LtS extends SimpleInstr("i32.lt_s", 0x48)
  case object I32LtU extends SimpleInstr("i32.lt_u", 0x49)
  case object I32GtS extends SimpleInstr("i32.gt_s", 0x4A)
  case object I32GtU extends SimpleInstr("i32.gt_u", 0x4B)
  case object I32LeS extends SimpleInstr("i32.le_s", 0x4C)
  case object I32LeU extends SimpleInstr("i32.le_u", 0x4D)
  case object I32GeS extends SimpleInstr("i32.ge_s", 0x4E)
  case object I32GeU extends SimpleInstr("i32.ge_u", 0x4F)
  case object I64Eq extends SimpleInstr("i64.eq", 0x51)
  case object I64Ne extends SimpleInstr("i64.ne", 0x52)
  case object I64LtS extends SimpleInstr("i64.lt_s", 0x53)
  case object I64LtU extends SimpleInstr("i64.lt_u", 0x54)
  case object I64GtS extends SimpleInstr("i64.gt_s", 0x55)
  case object I64GtU extends SimpleInstr("i64.gt_u", 0x56)
  case object I64LeS extends SimpleInstr("i64.le_s", 0x57)
  case object I64LeU extends SimpleInstr("i64.le_u", 0x58)
  case object I64GeS extends SimpleInstr("i64.ge_s", 0x59)
  case object I64GeU extends SimpleInstr("i64.ge_u", 0x5A)
  case object F32Eq extends SimpleInstr("f32.eq", 0x5B)
  case object F32Ne extends SimpleInstr("f32.ne", 0x5C)
  case object F32Lt extends SimpleInstr("f32.lt", 0x5D)
  case object F32Gt extends SimpleInstr("f32.gt", 0x5E)
  case object F32Le extends SimpleInstr("f32.le", 0x5F)
  case object F32Ge extends SimpleInstr("f32.ge", 0x60)
  case object F64Eq extends SimpleInstr("f64.eq", 0x61)
  case object F64Ne extends SimpleInstr("f64.ne", 0x62)
  case object F64Lt extends SimpleInstr("f64.lt", 0x63)
  case object F64Gt extends SimpleInstr("f64.gt", 0x64)
  case object F64Le extends SimpleInstr("f64.le", 0x65)
  case object F64Ge extends SimpleInstr("f64.ge", 0x66)
  case object I32Add extends SimpleInstr("i32.add", 0x6A)
  case object I32Sub extends SimpleInstr("i32.sub", 0x6B)
  case object I32Mul extends SimpleInstr("i32.mul", 0x6C)
  case object I32DivS extends SimpleInstr("i32.div_s", 0x6D)
  case object I32DivU extends SimpleInstr("i32.div_u", 0x6E)
  case object I32RemS extends SimpleInstr("i32.rem_s", 0x6F)
  case object I32RemU extends SimpleInstr("i32.rem_u", 0x70)
  case object I32And extends SimpleInstr("i32.and", 0x71)
  case object I32Or extends SimpleInstr("i32.or", 0x72)
  case object I32Xor extends SimpleInstr("i32.xor", 0x73)
  case object I32Shl extends SimpleInstr("i32.shl", 0x74)
  case object I32ShrS extends SimpleInstr("i32.shr_s", 0x75)
  case object I32ShrU extends SimpleInstr("i32.shr_u", 0x76)
  case object I32Rotl extends SimpleInstr("i32.rotl", 0x77)
  case object I32Rotr extends SimpleInstr("i32.rotr", 0x78)
  case object I64Add extends SimpleInstr("i64.add", 0x7C)
  case object I64Sub extends SimpleInstr("i64.sub", 0x7D)
  case object I64Mul extends SimpleInstr("i64.mul", 0x7E)
  case object I64DivS extends SimpleInstr("i64.div_s", 0x7F)
  case object I64DivU extends SimpleInstr("i64.div_u", 0x80)
  case object I64RemS extends SimpleInstr("i64.rem_s", 0x81)
  case object I64RemU extends SimpleInstr("i64.rem_u", 0x82)
  case object I64And extends SimpleInstr("i64.and", 0x83)
  case object I64Or extends SimpleInstr("i64.or", 0x84)
  case object I64Xor extends SimpleInstr("i64.xor", 0x85)
  case object I64Shl extends SimpleInstr("i64.shl", 0x86)
  case object I64ShrS extends SimpleInstr("i64.shr_s", 0x87)
  case object I64ShrU extends SimpleInstr("i64.shr_u", 0x88)
  case object I64Rotl extends SimpleInstr("i64.rotl", 0x89)
  case object I64Rotr extends SimpleInstr("i64.rotr", 0x8A)
  case object F32Add extends SimpleInstr("f32.add", 0x92)
  case object F32Sub extends SimpleInstr("f32.sub", 0x93)
  case object F32Mul extends SimpleInstr("f32.mul", 0x94)
  case object F32Div extends SimpleInstr("f32.div", 0x95)
  case object F32Min extends SimpleInstr("f32.min", 0x96)
  case object F32Max extends SimpleInstr("f32.max", 0x97)
  case object F32Copysign extends SimpleInstr("f32.copysign", 0x98)
  case object F64Add extends SimpleInstr("f64.add", 0xA0)
  case object F64Sub extends SimpleInstr("f64.sub", 0xA1)
  case object F64Mul extends SimpleInstr("f64.mul", 0xA2)
  case object F64Div extends SimpleInstr("f64.div", 0xA3)
  case object F64Min extends SimpleInstr("f64.min", 0xA4)
  case object F64Max extends SimpleInstr("f64.max", 0xA5)

  case class I32Const(v: Int) extends Instr("i32.const", 0x41)
  case class I64Const(v: Long) extends Instr("i64.const", 0x42)
  case class F32Const(v: Float) extends Instr("f32.const", 0x43)
  case class F64Const(v: Double) extends Instr("f64.const", 0x44)

  // Control instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions
  case object Unreachable extends SimpleInstr("unreachable", 0x00) with StackPolymorphicInstr
  case object Nop extends SimpleInstr("nop", 0x01)
  case class Block(i: BlockType, label: Option[LabelName])
      extends BlockTypeLabeledInstr("block", 0x02, i)
  case class Loop(i: BlockType, label: Option[LabelName])
      extends BlockTypeLabeledInstr("loop", 0x03, i)
  case class If(i: BlockType, label: Option[LabelName] = None)
      extends BlockTypeLabeledInstr("if", 0x04, i)
  case object Else extends SimpleInstr("else", 0x05)
  case object End extends SimpleInstr("end", 0x0B)
  case class Br(i: LabelName) extends LabelInstr("br", 0x0C, i) with StackPolymorphicInstr
  case class BrIf(i: LabelName) extends LabelInstr("br_if", 0x0D, i)
  case class BrTable(table: List[LabelName], default: LabelName)
      extends Instr("br_table", 0x0E)
      with StackPolymorphicInstr
  case object Return extends SimpleInstr("return", 0x0F) with StackPolymorphicInstr
  case class Call(i: FunctionName) extends FuncInstr("call", 0x10, i)
  case class ReturnCall(i: FunctionName) extends FuncInstr("return_call", 0x12, i)
  case class Throw(i: TagName) extends TagInstr("throw", 0x08, i) with StackPolymorphicInstr
  case object ThrowRef extends SimpleInstr("throw_ref", 0x0A) with StackPolymorphicInstr
  case class TryTable(i: BlockType, cs: List[CatchClause], label: Option[LabelName] = None)
      extends Instr("try_table", 0x1F)
      with StructuredLabeledInstr

  // Legacy exception system
  case class Try(i: BlockType, label: Option[LabelName] = None)
      extends BlockTypeLabeledInstr("try", 0x06, i)
  case class Catch(i: TagName) extends TagInstr("catch", 0x07, i)
  case object CatchAll extends SimpleInstr("catch_all", 0x19)
  // case class Delegate(i: LabelName) extends LabelInstr("delegate", 0x18, i)
  case class Rethrow(i: LabelName) extends LabelInstr("rethrow", 0x09, i) with StackPolymorphicInstr

  // Parametric instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#parametric-instructions
  case object Drop extends SimpleInstr("drop", 0x1A)
  // TODO: Select

  // Variable instructions
  // https://webassembly.github.io/spec/core/syntax/instructions.html#variable-instructions
  case class LocalGet(i: LocalName) extends LocalInstr("local.get", 0x20, i)
  case class LocalSet(i: LocalName) extends LocalInstr("local.set", 0x21, i)
  case class LocalTee(i: LocalName) extends LocalInstr("local.tee", 0x22, i)
  case class GlobalGet(i: GlobalName) extends GlobalInstr("global.get", 0x23, i)
  case class GlobalSet(i: GlobalName) extends GlobalInstr("global.set", 0x24, i)

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
  case class RefNull(i: HeapType) extends HeapTypeInstr("ref.null", 0xD0, i)

  /** checks for null. ref.is_null : [rtref] -> [i32]
    */
  case object RefIsNull extends SimpleInstr("ref.is_null", 0xD1)

  /** creates a reference to a given function.
    *
    * `ref.func $$x : [] -> [funcref]` (iff $$x : func $$t)
    */
  case class RefFunc(i: FunctionName) extends FuncInstr("ref.func", 0xD2, i)

  case object AnyConvertExtern extends SimpleInstr("any.convert_extern", 0xFB1A)
  case object ExternConvertAny extends SimpleInstr("extern.convert_any", 0xFB1B)

  case object RefI31 extends SimpleInstr("ref.i31", 0xFB1C)
  case object I31GetS extends SimpleInstr("i31.get_s", 0xFB1D)
  case object I31GetU extends SimpleInstr("i31.get_u", 0xFB1E)

  // ============================================================
  // Typed Function References
  // https://github.com/WebAssembly/function-references
  case class CallRef(i: TypeName) extends TypeInstr("call_ref", 0x14, i)
  case class ReturnCallRef(i: TypeName) extends TypeInstr("return_call_ref", 0x15, i)
  case object RefAsNotNull extends SimpleInstr("ref.as_non_null", 0xD4)
  case class BrOnNull(i: LabelName) extends LabelInstr("br_on_null", 0xD5, i)
  case class BrOnNonNull(i: LabelName) extends LabelInstr("br_on_non_null", 0xD6, i)

  // ============================================================
  // gc
  case class StructNew(i: TypeName) extends TypeInstr("struct.new", 0xFB00, i)
  case class StructNewDefault(i: TypeName) extends TypeInstr("struct.new_default", 0xFB01, i)
  case class StructGet(tyidx: TypeName, fidx: FieldIdx)
      extends StructFieldInstr("struct.get", 0xFB02, tyidx, fidx)
  // StructGetS
  // StructGetU
  case class StructSet(tyidx: TypeName, fidx: FieldIdx)
      extends StructFieldInstr("struct.set", 0xFB05, tyidx, fidx)

  case class ArrayNew(i: TypeName) extends TypeInstr("array.new", 0xFB06, i)
  case class ArrayNewDefault(i: TypeName) extends TypeInstr("array.new_default", 0xFB07, i)
  case class ArrayNewFixed(i: TypeName, size: Int) extends Instr("array.new_fixed", 0xFB08)
  case class ArrayNewData(i: TypeName, d: DataName) extends Instr("array.new_data", 0xFB09)
  case class ArrayGet(i: TypeName) extends TypeInstr("array.get", 0xFB0B, i)
  case class ArrayGetS(i: TypeName) extends TypeInstr("array.get_s", 0xFB0C, i)
  case class ArrayGetU(i: TypeName) extends TypeInstr("array.get_u", 0xFB0D, i)
  case class ArraySet(i: TypeName) extends TypeInstr("array.set", 0xFB0E, i)
  case object ArrayLen extends SimpleInstr("array.len", 0xFB0F)
  // ArrayFill
  case class ArrayCopy(destType: TypeName, srcType: TypeName) extends Instr("array.copy", 0xFB11)
  // ArrayInitData
  // ArrayInitElem

  case object RefEq extends SimpleInstr("ref.eq", 0xD3)
  case class RefTest(i: RefType) extends RefTypeInstr("ref.test", 0xFB14, 0xFB15, i)
  case class RefCast(i: RefType) extends RefTypeInstr("ref.cast", 0xFB16, 0xFB17, i)

  case class BrOnCast(label: LabelName, from: RefType, to: RefType)
      extends Instr("br_on_cast", 0xFB18)
  case class BrOnCastFail(label: LabelName, from: RefType, to: RefType)
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
