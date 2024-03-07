package wasm
package ir2wasm

import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Names => IRNames}
import wasm4s._

object TypeTransformer {

  val makeReceiverType: Types.WasmType =
    Types.WasmRefNullType(Types.WasmHeapType.ObjectType)

  def transformFunctionType(
      // clazz: WasmContext.WasmClassInfo,
      method: WasmContext.WasmFunctionInfo
  )(implicit ctx: FunctionTypeWriterWasmContext): WasmFunctionType = {
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

  /** This transformation should be used only for the result types of functions.
    * @see
    *   https://webassembly.github.io/spec/core/syntax/types.html#result-types
    */
  def transformResultType(
      t: IRTypes.Type
  )(implicit ctx: ReadOnlyWasmContext): List[Types.WasmType] =
    t match {
      case IRTypes.NoType => Nil
      case _              => List(transformType(t))
    }
  def transformType(t: IRTypes.Type)(implicit ctx: ReadOnlyWasmContext): Types.WasmType =
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
      case clazz @ IRTypes.ClassType(className) =>
        className match {
          case IRNames.BoxedStringClass =>
            Types.WasmRefNullType(
              Types.WasmHeapType.Type(Names.WasmTypeName.WasmStructTypeName.string)
            )
          case IRNames.BoxedUnitClass =>
            Types.WasmRefNullType(
              Types.WasmHeapType.Type(Names.WasmTypeName.WasmStructTypeName.undef)
            )
          case _ =>
            if (ctx.getClassInfo(clazz.className).isInterface)
              Types.WasmRefNullType(Types.WasmHeapType.ObjectType)
            else
              Types.WasmRefType(
                Types.WasmHeapType.Type(Names.WasmTypeName.WasmStructTypeName(className))
              )
        }
      case IRTypes.RecordType(fields) => ???
      case IRTypes.StringType =>
        Types.WasmRefType(
          Types.WasmHeapType.Type(Names.WasmTypeName.WasmStructTypeName.string)
        )
      case IRTypes.UndefType =>
        Types.WasmRefType(
          Types.WasmHeapType.Type(Names.WasmTypeName.WasmStructTypeName.undef)
        )
      case p: IRTypes.PrimTypeWithRef => transformPrimType(p)
    }

  def transformPrimType(
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
      // ???
      case IRTypes.NothingType => Types.WasmRefNullrefType
      case IRTypes.NullType    => Types.WasmRefNullrefType
      case IRTypes.NoType      => Types.WasmNoType
    }
}
