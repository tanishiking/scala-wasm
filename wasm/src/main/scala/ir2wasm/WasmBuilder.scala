package wasm
package ir2wasm

import wasm4s._
import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.ClassKind
import org.scalajs.ir.Trees.FieldDef
import org.scalajs.ir.Trees.JSFieldDef

class WasmBuilder {
  val module = new WasmModule()

  def transformClassDef(clazz: IRTrees.ClassDef)(implicit ctx: WasmContext) = {
    clazz.kind match {
      case ClassKind.ModuleClass => transformModuleClass(clazz)
      case _                     =>
    }
  }

  def transformModuleClass(clazz: IRTrees.ClassDef)(implicit ctx: WasmContext) = {
    assert(clazz.kind == ClassKind.ModuleClass)

    // type definition
    val fields = clazz.fields.map(transformField)
    val structType = WasmStructType(
      Names.WasmGCTypeName.fromIR(clazz.name.name),
      fields,
      None // TODO
    )
    ctx.gcTypes.define(structType)
    module.addRecGroupType(structType)

    // global instance
    // (global name (ref null type))
    val global = WasmGlobal(
      Names.WasmGlobalName.forModuleClassInstance(clazz.name.name),
      Types.WasmRefNullType(Types.WasmHeapType.Type(structType.name))
    )
    ctx.globals.define(global)
    module.addGlobal(global)

    val receiver = WasmLocal(
      Names.WasmLocalName.fromStr("<this>"),
      global.typ,
      isParameter = true
    )
    clazz.methods.foreach { method =>
      val paramTys = receiver.typ +: method.args.map(arg => TypeTransformer.transform(arg.ptpe))
      val resultTy = TypeTransformer.transformResultType(method.resultType)
      val funcType = WasmFunctionType(
        Names.WasmFunctionTypeName.fromIR(method.name.name),
        paramTys,
        resultTy
      )
      ctx.functionTypes.define(funcType)
      module.addFunctionType(funcType) // TODO: normalize

      implicit val fctx = new WasmFunctionContext(receiver)
      val expressionBuilder = new WasmExpressionBuilder(fctx)

      val params = receiver +: method.args.map { arg =>
        WasmLocal(
          Names.WasmLocalName.fromIR(arg.name.name),
          TypeTransformer.transform(arg.ptpe),
          isParameter = true
        )
      }
      params.foreach(fctx.locals.define)

      val body = expressionBuilder.transformMethod(method)
      val func = WasmFunction(
        Names.WasmFunctionName.fromIR(method.name.name),
        funcType.name,
        params,
        WasmExpr(body)
      )
      ctx.functions.define(func)
      module.addFunction(func)
    }
  }

  /*
  def addFunction(method: IRTrees.MethodDef)(implicit ctx: WasmContext) = {
    val paramTys = method.args.map(arg => TypeTransformer.transform(arg.ptpe))
    val resultTy = TypeTransformer.transformResultType(method.resultType)
    val funcType =
      WasmFunctionType(Names.WasmFunctionTypeName.fromIR(method.name.name), paramTys, resultTy)
    ctx.functionTypes.define(funcType)
    module.addFunctionType(funcType) // TODO: normalize

    implicit val fctx = new WasmFunctionContext()
    val expressionBuilder = new WasmExpressionBuilder(fctx)

    val localNames = method.args.map(addParam)
    val locals = localNames.map { sym => fctx.locals.resolve(sym) }

    val body = expressionBuilder.transformMethod(method)
    val func = WasmFunction(
      Names.WasmFunctionName.fromIR(method.name.name),
      funcType.name,
      locals,
      WasmExpr(body)
    )
    val funcSym = ctx.functions.define(func)
    module.addFunction(func)
  }
   */

  private def transformField(
      field: IRTrees.AnyFieldDef
  )(implicit ctx: WasmContext): WasmStructField = {
    val fieldName =
      field match {
        case f: FieldDef =>
          Names.WasmFieldName.fromIR(f.name.name)
        // TODO
        case js: JSFieldDef => ???
      }
    WasmStructField(
      fieldName,
      TypeTransformer.transform(field.ftpe),
      field.flags.isMutable
    )
  }

  /*
  private def addParam(
      param: IRTrees.ParamDef
  )(implicit ctx: WasmContext, fctx: WasmFunctionContext): Names.WasmLocalName = {
    val local = WasmLocal(
      Names.WasmLocalName.fromIR(param.name.name),
      TypeTransformer.transform(param.ptpe),
      isParameter = true
    )
    fctx.locals.define(local)
    local.name
  }
   */

}
