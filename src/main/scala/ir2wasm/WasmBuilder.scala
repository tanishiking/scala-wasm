package ir2wasm

import wasm4s._
import org.scalajs.ir.{Trees => IRTrees}

class WasmBuilder {
  val module = new WasmModule()

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

}
