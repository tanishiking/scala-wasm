package wasm.ir2wasm

import scala.collection.mutable

import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.Position

import wasm.wasm4s._
import wasm.wasm4s.Names._
import wasm.wasm4s.Types.WasmType
import wasm.wasm4s.WasmInstr._

class WasmFunctionContext private (
    ctx: TypeDefinableWasmContext,
    val enclosingClassName: Option[IRNames.ClassName],
    val functionName: WasmFunctionName,
    _params: List[WasmLocal],
    _newTargetStorage: Option[WasmFunctionContext.VarStorage.Local],
    _receiverStorage: Option[WasmFunctionContext.VarStorage.Local],
    _paramsEnv: WasmFunctionContext.Env,
    _resultTypes: List[WasmType],
    functionPos: Position
) {
  import WasmFunctionContext._

  val fb: FunctionBuilder = new FunctionBuilder(ctx.moduleBuilder, functionName, functionPos)
  for (param <- _params)
    fb.addParam(param.name, param.typ)
  fb.setResultTypes(_resultTypes)

  /** Alias for `fb` because we have been using `fctx.instrs` everywhere. */
  val instrs: fb.type = fb

  private var innerFuncIdx = 0
  private var currentEnv: Env = _paramsEnv

  def newTargetStorage: VarStorage.Local =
    _newTargetStorage.getOrElse(throw new Error("Cannot access new.target in this context."))

  def receiverStorage: VarStorage.Local =
    _receiverStorage.getOrElse(throw new Error("Cannot access to the receiver in this context."))

  def paramIndices: List[WasmLocalName] = _params.map(_.name)

  def addLocal(name: WasmLocalName, typ: WasmType): WasmLocalName =
    fb.addLocal(name, typ)

  def addLocal(name: String, typ: WasmType): WasmLocalName =
    fb.addLocal(name, typ)

  private def addLocal(name: IRNames.LocalName, typ: WasmType): WasmLocalName =
    addLocal(localNameFromIR(name), typ)

  def withNewLocal[A](name: IRNames.LocalName, typ: WasmType)(body: WasmLocalName => A): A = {
    val savedEnv = currentEnv
    val local = addLocal(name, typ)
    currentEnv = currentEnv.updated(name, VarStorage.Local(local))
    try body(local)
    finally currentEnv = savedEnv
  }

  def lookupLocal(name: IRNames.LocalName): VarStorage = {
    currentEnv.getOrElse(
      name, {
        throw new AssertionError(s"Cannot find binding for '${name.nameString}'")
      }
    )
  }

  def lookupLocalAssertLocalStorage(name: IRNames.LocalName): WasmLocalName = {
    (lookupLocal(name): @unchecked) match {
      case VarStorage.Local(local) => local
    }
  }

  def addSyntheticLocal(typ: WasmType): WasmLocalName =
    fb.addSyntheticLocal(typ)

  def genInnerFuncName(): WasmFunctionName = {
    val innerName = WasmFunctionName(functionName.name + "__c" + innerFuncIdx)
    innerFuncIdx += 1
    innerName
  }

  // Position handling

  def markPosition(tree: IRTrees.Tree): Unit =
    fb.markPosition(tree)

  // Final result

  def buildAndAddToContext(): WasmFunction =
    buildAndAddToContext(useFunctionTypeInMainRecType = false)

  def buildAndAddToContext(useFunctionTypeInMainRecType: Boolean): WasmFunction = {
    if (useFunctionTypeInMainRecType) {
      val sig = WasmFunctionSignature(_params.map(_.typ), _resultTypes)
      fb.setFunctionType(ctx.addFunctionTypeInMainRecType(sig))
    }

    fb.buildAndAddToModule()
  }

  def buildAndAddToContext(functionTypeName: WasmTypeName): WasmFunction = {
    fb.setFunctionType(functionTypeName)
    fb.buildAndAddToModule()
  }
}

object WasmFunctionContext {
  private val newTargetLocalName = WasmLocalName("new.target")
  private val receiverLocalName = WasmLocalName("___<this>")

  private def localNameFromIR(name: IRNames.LocalName): WasmLocalName =
    WasmLocalName(name.nameString)

  sealed abstract class VarStorage

  object VarStorage {
    final case class Local(idx: WasmLocalName) extends VarStorage

    final case class StructField(
        structIdx: WasmLocalName,
        structTypeName: WasmTypeName,
        fieldIdx: WasmFieldIdx
    ) extends VarStorage
  }

  private type Env = Map[IRNames.LocalName, VarStorage]

  def apply(
      enclosingClassName: Option[IRNames.ClassName],
      name: WasmFunctionName,
      captureParamDefs: Option[List[IRTrees.ParamDef]],
      preSuperVarDefs: Option[List[IRTrees.VarDef]],
      hasNewTarget: Boolean,
      receiverTyp: Option[WasmType],
      paramDefs: List[IRTrees.ParamDef],
      resultTypes: List[WasmType]
  )(implicit ctx: TypeDefinableWasmContext, pos: Position): WasmFunctionContext = {
    def makeCaptureLikeParamListAndEnv(
        captureParamName: String,
        captureLikes: Option[List[(IRNames.LocalName, IRTypes.Type)]]
    ): (List[WasmLocal], Env) = {
      captureLikes match {
        case None =>
          (Nil, Map.empty)

        case Some(captureLikes) =>
          val dataStructTypeName = ctx.getClosureDataStructType(captureLikes.map(_._2))
          val local = WasmLocal(
            WasmLocalName(captureParamName),
            Types.WasmRefType(dataStructTypeName),
            isParameter = true
          )
          val env: Env = captureLikes.zipWithIndex.map { case (captureLike, idx) =>
            val storage = VarStorage.StructField(
              local.name,
              dataStructTypeName,
              WasmFieldIdx(idx)
            )
            captureLike._1 -> storage
          }.toMap
          (local :: Nil, env)
      }
    }

    val (captureDataParamList, captureParamsEnv) = {
      makeCaptureLikeParamListAndEnv(
        "__captureData",
        captureParamDefs.map(_.map(p => p.name.name -> p.ptpe))
      )
    }

    val (preSuperEnvParamList, preSuperEnvEnv) = {
      makeCaptureLikeParamListAndEnv(
        "__preSuperEnv",
        preSuperVarDefs.map(_.map(p => p.name.name -> p.vtpe))
      )
    }

    val newTarget =
      if (!hasNewTarget) None
      else Some(WasmLocal(newTargetLocalName, Types.WasmRefType.anyref, isParameter = true))
    val newTargetStorage = newTarget.map(local => VarStorage.Local(local.name))

    val receiver = receiverTyp.map { typ =>
      WasmLocal(receiverLocalName, typ, isParameter = true)
    }
    val receiverStorage = receiver.map(local => VarStorage.Local(local.name))

    val normalParams = paramDefsToWasmParams(paramDefs)
    val normalParamsEnv = paramDefs.zip(normalParams).map { case (paramDef, param) =>
      paramDef.name.name -> VarStorage.Local(param.name)
    }

    val allParams =
      captureDataParamList ::: preSuperEnvParamList ::: newTarget.toList ::: receiver.toList ::: normalParams
    val fullEnv = captureParamsEnv ++ preSuperEnvEnv ++ normalParamsEnv

    new WasmFunctionContext(
      ctx,
      enclosingClassName,
      name,
      allParams,
      newTargetStorage,
      receiverStorage,
      fullEnv,
      resultTypes,
      pos
    )
  }

  def apply(
      enclosingClassName: Option[IRNames.ClassName],
      name: WasmFunctionName,
      captureParamDefs: Option[List[IRTrees.ParamDef]],
      receiverTyp: Option[WasmType],
      paramDefs: List[IRTrees.ParamDef],
      resultTypes: List[WasmType]
  )(implicit ctx: TypeDefinableWasmContext, pos: Position): WasmFunctionContext = {
    apply(
      enclosingClassName,
      name,
      captureParamDefs,
      None,
      hasNewTarget = false,
      receiverTyp,
      paramDefs,
      resultTypes
    )
  }

  def apply(
      enclosingClassName: Option[IRNames.ClassName],
      name: WasmFunctionName,
      receiverTyp: Option[WasmType],
      paramDefs: List[IRTrees.ParamDef],
      resultTypes: List[WasmType]
  )(implicit ctx: TypeDefinableWasmContext, pos: Position): WasmFunctionContext = {
    apply(
      enclosingClassName,
      name,
      captureParamDefs = None,
      receiverTyp,
      paramDefs,
      resultTypes
    )
  }

  def apply(
      enclosingClassName: Option[IRNames.ClassName],
      name: WasmFunctionName,
      receiverTyp: Option[WasmType],
      paramDefs: List[IRTrees.ParamDef],
      resultType: IRTypes.Type
  )(implicit ctx: TypeDefinableWasmContext, pos: Position): WasmFunctionContext = {
    apply(
      enclosingClassName,
      name,
      receiverTyp,
      paramDefs,
      TypeTransformer.transformResultType(resultType)
    )
  }

  def apply(
      name: WasmFunctionName,
      params: List[(String, WasmType)],
      resultTypes: List[WasmType]
  )(implicit ctx: TypeDefinableWasmContext, pos: Position): WasmFunctionContext = {
    val paramLocals = params.map { param =>
      WasmLocal(WasmLocalName(param._1), param._2, isParameter = true)
    }
    new WasmFunctionContext(ctx, None, name, paramLocals, None, None, Map.empty, resultTypes, pos)
  }

  def paramDefsToWasmParams(
      paramDefs: List[IRTrees.ParamDef]
  )(implicit ctx: TypeDefinableWasmContext, pos: Position): List[WasmLocal] = {
    paramDefs.map { paramDef =>
      WasmLocal(
        localNameFromIR(paramDef.name.name),
        TypeTransformer.transformType(paramDef.ptpe),
        isParameter = true
      )
    }
  }
}
