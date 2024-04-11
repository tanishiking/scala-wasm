package wasm
package ir2wasm

import org.scalajs.ir.ClassKind
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Names => IRNames}
import wasm4s._

object TypeTransformer {

  val makeReceiverType: Types.WasmType =
    Types.WasmRefType.any

  def transformFunctionType(
      // clazz: WasmContext.WasmClassInfo,
      method: WasmContext.WasmFunctionInfo
  )(implicit ctx: TypeDefinableWasmContext): WasmFunctionType = {
    // val className = clazz.name
    val name = method.name
    val receiverType = makeReceiverType
    //   if (clazz.kind.isClass) List(makeReceiverType) else Nil
    val sig = WasmFunctionSignature(
      receiverType +: method.argTypes.map(transformType),
      transformResultType(method.resultType)
    )
    val typeName = ctx.addFunctionType(sig)
    WasmFunctionType(typeName, sig)
  }

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
  )(implicit ctx: ReadOnlyWasmContext): List[Types.WasmType] =
    t match {
      case IRTypes.NoType      => Nil
      case IRTypes.NothingType => Nil
      case _                   => List(transformType(t))
    }

  def transformTypeRef(t: IRTypes.TypeRef)(implicit ctx: ReadOnlyWasmContext): Types.WasmType =
    t match {
      case arrayTypeRef: IRTypes.ArrayTypeRef =>
        Types.WasmRefType.nullable(
          Names.WasmTypeName.WasmStructTypeName.forArrayClass(arrayTypeRef)
        )
      case IRTypes.ClassRef(className) =>
        transformClassByName(className)
      case IRTypes.PrimRef(tpe) => transformPrimType(tpe)
    }

  /** Transforms a value type to a unique Wasm type.
    *
    * This method cannot be used for `void` and `nothing`, since they have no corresponding Wasm
    * value type.
    */
  def transformType(t: IRTypes.Type)(implicit ctx: ReadOnlyWasmContext): Types.WasmType =
    t match {
      case IRTypes.AnyType => Types.WasmRefType.anyref

      case tpe: IRTypes.ArrayType =>
        Types.WasmRefType.nullable(
          Names.WasmTypeName.WasmStructTypeName.forArrayClass(tpe.arrayTypeRef)
        )
      case IRTypes.ClassType(className) => transformClassByName(className)
      case IRTypes.RecordType(fields)   => ???
      case IRTypes.StringType | IRTypes.UndefType =>
        Types.WasmRefType.any
      case p: IRTypes.PrimTypeWithRef => transformPrimType(p)
    }

  private def transformClassByName(
      className: IRNames.ClassName
  )(implicit ctx: ReadOnlyWasmContext): Types.WasmType = {
    className match {
      case _ =>
        val info = ctx.getClassInfo(className)
        if (info.isAncestorOfHijackedClass)
          Types.WasmRefType.anyref
        else if (info.isInterface)
          Types.WasmRefType.nullable(Types.WasmHeapType.ObjectType)
        else
          Types.WasmRefType.nullable(
            Names.WasmTypeName.WasmStructTypeName.forClass(className)
          )
    }
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
