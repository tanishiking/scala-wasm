package ir2wasm

import wasm4s._
import org.scalajs.ir.{Trees => IRTrees}

class WasmBuilder {
  val module = new WasmModule()

  private def buildFunctionTypeName(method: IRTrees.MethodDef): Ident =
    Ident(s"${method.name.name.nameString}___ty")

  private def buildFunctionName(method: IRTrees.MethodDef): Ident =
    Ident(s"${method.name.name.nameString}___method")

  def addFunction(method: IRTrees.MethodDef)(implicit ctx: WasmContext) = {
    val paramTys = method.args.map(arg => TypeTransformer.transform(arg.ptpe))
    val resultTy = TypeTransformer.transformResultType(method.resultType)
    val funcType = WasmFunctionType(buildFunctionTypeName(method), paramTys, resultTy)
    val tySym = ctx.functionTypes.define(funcType)
    module.addFunctionType(funcType) // TODO: normalize

    implicit val fctx = new WasmFunctionContext()
    val expressionBuilder = new WasmExpressionBuilder(fctx)

    val localSyms = method.args.map(addParam)
    val locals = localSyms.map {sym => fctx.locals.resolve(sym)}
    
    val body = expressionBuilder.transformMethod(method)
    val func = WasmFunction(
        buildFunctionName(method),
        tySym,
        locals,
        WasmExpr(body)
    )
    val funcSym = ctx.functions.define(func)
    module.addFunction(func)
  }

  private def addParam(param: IRTrees.ParamDef)(implicit ctx: WasmContext, fctx: WasmFunctionContext): WasmSymbol[WasmLocal] = {
    val local = WasmLocal(
        Ident(param.name.name.nameString),
        TypeTransformer.transform(param.ptpe),
        isParameter = true,
    )
    fctx.locals.define(local)
  }


}
