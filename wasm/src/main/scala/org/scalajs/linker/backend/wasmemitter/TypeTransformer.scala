package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.ClassKind
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Names => IRNames}

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.Names._

import VarGen._

object TypeTransformer {

  /** This transformation should be used only for the result types of functions or blocks.
    *
    * `nothing` translates to an empty result type list, because Wasm does not have a bottom type
    * (at least not one that can expressed at the user level). A block or function call that returns
    * `nothing` should typically be followed by an extra `unreachable` statement to recover a
    * stack-polymorphic context.
    *
    * @see
    *   https://webassembly.github.io/spec/core/syntax/types.html#result-types
    */
  def transformResultType(
      t: IRTypes.Type
  )(implicit ctx: WasmContext): List[Types.WasmType] =
    t match {
      case IRTypes.NoType      => Nil
      case IRTypes.NothingType => Nil
      case _                   => List(transformType(t))
    }

  /** Transforms a value type to a unique Wasm type.
    *
    * This method cannot be used for `void` and `nothing`, since they have no corresponding Wasm
    * value type.
    */
  def transformType(t: IRTypes.Type)(implicit ctx: WasmContext): Types.WasmType =
    t match {
      case IRTypes.AnyType => Types.WasmRefType.anyref

      case tpe: IRTypes.ArrayType =>
        Types.WasmRefType.nullable(genTypeName.forArrayClass(tpe.arrayTypeRef))
      case IRTypes.ClassType(className) => transformClassType(className)
      case IRTypes.RecordType(fields)   => ???
      case IRTypes.StringType | IRTypes.UndefType =>
        Types.WasmRefType.any
      case p: IRTypes.PrimTypeWithRef => transformPrimType(p)
    }

  def transformClassType(
      className: IRNames.ClassName
  )(implicit ctx: WasmContext): Types.WasmRefType = {
    val info = ctx.getClassInfo(className)
    if (info.isAncestorOfHijackedClass)
      Types.WasmRefType.anyref
    else if (info.isInterface)
      Types.WasmRefType.nullable(genTypeName.ObjectStruct)
    else
      Types.WasmRefType.nullable(genTypeName.forClass(className))
  }

  private def transformPrimType(
      t: IRTypes.PrimTypeWithRef
  ): Types.WasmType =
    t match {
      case IRTypes.BooleanType => Types.WasmInt32
      case IRTypes.ByteType    => Types.WasmInt32
      case IRTypes.ShortType   => Types.WasmInt32
      case IRTypes.IntType     => Types.WasmInt32
      case IRTypes.CharType    => Types.WasmInt32
      case IRTypes.LongType    => Types.WasmInt64
      case IRTypes.FloatType   => Types.WasmFloat32
      case IRTypes.DoubleType  => Types.WasmFloat64
      case IRTypes.NullType    => Types.WasmRefType.nullref

      case IRTypes.NoType | IRTypes.NothingType =>
        throw new IllegalArgumentException(s"${t.show()} does not have a corresponding Wasm type")
    }
}
