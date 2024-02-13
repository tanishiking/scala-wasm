package wasm
package ir2wasm

import org.scalajs.ir.{Types => IRTypes}
import wasm4s._

object TypeTransformer {

  /** This transformation should be used only for the result types of functions.
    * @see
    *   https://webassembly.github.io/spec/core/syntax/types.html#result-types
    */
  def transformResultType(t: IRTypes.Type)(implicit context: WasmContext): List[Types.WasmType] =
    t match {
      case IRTypes.NoType => Nil
      case _              => List(transform(t))
    }
  def transform(t: IRTypes.Type)(implicit context: WasmContext): Types.WasmType =
    t match {
      case IRTypes.AnyType => Types.WasmAnyRef

      case tpe @ IRTypes.ArrayType(IRTypes.ArrayTypeRef(elemType, size)) =>
        // TODO
        // val wasmElemTy =
        //   elemType match {
        //     case IRTypes.ClassRef(className) =>
        //       // val gcTypeSym = context.gcTypes.reference(Ident(className.nameString))
        //       Types.WasmRefType(Types.WasmHeapType.Type(Names.WasmGCTypeName.fromIR(className)))
        //     case IRTypes.PrimRef(tpe) =>
        //       transform(tpe)
        //   }
        // val field = WasmStructField("TODO", wasmElemTy, isMutable = false)
        // val arrayTySym =
        //   context.gcTypes.define(WasmArrayType(Names.WasmGCTypeName.fromIR(tpe), field))
        // Types.WasmRefType(Types.WasmHeapType.Type(arrayTySym))
        ???
      case IRTypes.ClassType(className) => ???
      case IRTypes.RecordType(fields)   => ???
      case IRTypes.StringType           => ??? // TODO
      case IRTypes.UndefType            => ???
      case p: IRTypes.PrimTypeWithRef   => transformPrimType(p)
    }

  def transformPrimType(
      t: IRTypes.PrimTypeWithRef
  )(implicit context: WasmContext): Types.WasmType =
    t match {
      case IRTypes.BooleanType => Types.WasmInt32
      case IRTypes.ByteType    => Types.WasmInt32
      case IRTypes.ShortType   => Types.WasmInt32
      case IRTypes.IntType     => Types.WasmInt32
      case IRTypes.CharType    => Types.WasmInt32
      case IRTypes.LongType    => Types.WasmInt64
      case IRTypes.FloatType   => Types.WasmFloat32
      case IRTypes.DoubleType  => Types.WasmFloat64
      // ???
      case IRTypes.NothingType => Types.WasmRefNullrefType
      case IRTypes.NullType    => Types.WasmRefNullrefType
      case IRTypes.NoType      => throw new Exception("NoType")
    }
}
