package wasm.wasm4s

import scala.collection.mutable

import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Trees => IRTrees}

import wasm.wasm4s.Names._
import wasm.wasm4s.Types.WasmType
import wasm.wasm4s.WasmImmediate.{BlockType, LabelIdx, LocalIdx}
import wasm.wasm4s.WasmInstr._

import wasm.ir2wasm.TypeTransformer

class WasmFunctionContext private (
    ctx: TypeDefinableWasmContext,
    val enclosingClassName: Option[IRNames.ClassName],
    val functionName: WasmFunctionName,
    _params: List[WasmLocal],
    _newTargetStorage: Option[WasmFunctionContext.VarStorage.Local],
    _receiverStorage: Option[WasmFunctionContext.VarStorage.Local],
    _paramsEnv: WasmFunctionContext.Env,
    _resultTypes: List[WasmType]
) {
  import WasmFunctionContext._

  private var cnt = 0
  private var labelIdx = 0
  private var innerFuncIdx = 0
  private var currentEnv: Env = _paramsEnv

  private val locals = new WasmSymbolTable[WasmLocalName, WasmLocal]()

  _params.foreach(locals.define(_))

  def newTargetStorage: VarStorage.Local =
    _newTargetStorage.getOrElse(throw new Error("Cannot access new.target in this context."))

  def receiverStorage: VarStorage.Local =
    _receiverStorage.getOrElse(throw new Error("Cannot access to the receiver in this context."))

  def paramIndices: List[LocalIdx] = _params.map(p => LocalIdx(p.name))

  private val registeredLabels =
    mutable.AnyRefMap.empty[IRNames.LabelName, (LabelIdx, IRTypes.Type)]

  /** The instructions buffer; publicly mutable on purpose. */
  val instrs: mutable.ListBuffer[WasmInstr] = mutable.ListBuffer.empty

  def genLabel(): LabelIdx = {
    val label = LabelIdx(labelIdx)
    labelIdx += 1
    label
  }

  def registerLabel(irLabelName: IRNames.LabelName, expectedType: IRTypes.Type): LabelIdx = {
    val label = genLabel()
    registeredLabels(irLabelName) = (label, expectedType)
    label
  }

  def getLabelFor(irLabelName: IRNames.LabelName): (LabelIdx, IRTypes.Type) = {
    registeredLabels.getOrElse(
      irLabelName, {
        throw new IllegalArgumentException(s"Unknown label ${irLabelName.nameString}")
      }
    )
  }

  private def addLocal(name: WasmLocalName, typ: WasmType): LocalIdx = {
    val local = WasmLocal(name, typ, isParameter = false)
    locals.define(local)
    LocalIdx(name)
  }

  def addLocal(name: String, typ: WasmType): LocalIdx =
    addLocal(WasmLocalName(name), typ)

  private def addLocal(name: IRNames.LocalName, typ: WasmType): LocalIdx =
    addLocal(WasmLocalName.fromIR(name), typ)

  def withNewLocal[A](name: IRNames.LocalName, typ: WasmType)(body: LocalIdx => A): A = {
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

  def lookupLocalAssertLocalStorage(name: IRNames.LocalName): LocalIdx = {
    (lookupLocal(name): @unchecked) match {
      case VarStorage.Local(local) => local
    }
  }

  private def genSyntheticLocalName(): WasmLocalName = {
    val name = WasmLocalName.synthetic(cnt)
    cnt += 1
    name
  }

  def addSyntheticLocal(typ: WasmType): LocalIdx =
    addLocal(genSyntheticLocalName(), typ)

  def genInnerFuncName(): WasmFunctionName = {
    val innerName = WasmFunctionName(
      functionName.namespace,
      functionName.simpleName + "__c" + innerFuncIdx
    )
    innerFuncIdx += 1
    innerName
  }

  // Helpers to build structured control flow

  def sigToBlockType(sig: WasmFunctionSignature): BlockType = sig match {
    case WasmFunctionSignature(Nil, Nil) =>
      BlockType.ValueType()
    case WasmFunctionSignature(Nil, resultType :: Nil) =>
      BlockType.ValueType(resultType)
    case _ =>
      BlockType.FunctionType(ctx.addFunctionType(sig))
  }

  def ifThenElse(blockType: BlockType)(thenp: => Unit)(elsep: => Unit): Unit = {
    instrs += IF(blockType)
    thenp
    instrs += ELSE
    elsep
    instrs += END
  }

  def ifThenElse(resultType: WasmType)(thenp: => Unit)(elsep: => Unit): Unit =
    ifThenElse(BlockType.ValueType(resultType))(thenp)(elsep)

  def ifThenElse()(thenp: => Unit)(elsep: => Unit): Unit =
    ifThenElse(BlockType.ValueType())(thenp)(elsep)

  def ifThen(blockType: BlockType)(thenp: => Unit): Unit = {
    instrs += IF(blockType)
    thenp
    instrs += END
  }

  def ifThen(sig: WasmFunctionSignature)(thenp: => Unit): Unit =
    ifThen(BlockType.FunctionType(ctx.addFunctionType(sig)))(thenp)

  def ifThen()(thenp: => Unit): Unit =
    ifThen(BlockType.ValueType())(thenp)

  def block[A](blockType: BlockType)(body: LabelIdx => A): A = {
    val label = genLabel()
    instrs += BLOCK(blockType, Some(label))
    val result = body(label)
    instrs += END
    result
  }

  def block[A](resultType: WasmType)(body: LabelIdx => A): A =
    block(BlockType.ValueType(resultType))(body)

  def block[A]()(body: LabelIdx => A): A =
    block(BlockType.ValueType())(body)

  def block[A](sig: WasmFunctionSignature)(body: LabelIdx => A): A =
    block(BlockType.FunctionType(ctx.addFunctionType(sig)))(body)

  def block[A](resultTypes: List[WasmType])(body: LabelIdx => A): A = {
    resultTypes match {
      case Nil           => block()(body)
      case single :: Nil => block(single)(body)
      case _             => block(WasmFunctionSignature(Nil, resultTypes))(body)
    }
  }

  def loop[A](blockType: BlockType)(body: LabelIdx => A): A = {
    val label = genLabel()
    instrs += LOOP(blockType, Some(label))
    val result = body(label)
    instrs += END
    result
  }

  def loop[A](resultType: WasmType)(body: LabelIdx => A): A =
    loop(BlockType.ValueType(resultType))(body)

  def loop[A]()(body: LabelIdx => A): A =
    loop(BlockType.ValueType())(body)

  def loop[A](sig: WasmFunctionSignature)(body: LabelIdx => A): A =
    loop(BlockType.FunctionType(ctx.addFunctionType(sig)))(body)

  def loop[A](resultTypes: List[WasmType])(body: LabelIdx => A): A = {
    resultTypes match {
      case Nil           => loop()(body)
      case single :: Nil => loop(single)(body)
      case _             => loop(WasmFunctionSignature(Nil, resultTypes))(body)
    }
  }

  def tryTable[A](blockType: BlockType)(clauses: List[WasmImmediate.CatchClause])(body: => A): A = {
    instrs += TRY_TABLE(blockType, WasmImmediate.CatchClauseVector(clauses))
    val result = body
    instrs += END
    result
  }

  def tryTable[A](resultType: WasmType)(clauses: List[WasmImmediate.CatchClause])(body: => A): A =
    tryTable(BlockType.ValueType(resultType))(clauses)(body)

  def tryTable[A]()(clauses: List[WasmImmediate.CatchClause])(body: => A): A =
    tryTable(BlockType.ValueType())(clauses)(body)

  def tryTable[A](sig: WasmFunctionSignature)(clauses: List[WasmImmediate.CatchClause])(
      body: => A
  ): A =
    tryTable(BlockType.FunctionType(ctx.addFunctionType(sig)))(clauses)(body)

  def tryTable[A](
      resultTypes: List[WasmType]
  )(clauses: List[WasmImmediate.CatchClause])(body: => A): A = {
    resultTypes match {
      case Nil           => tryTable()(clauses)(body)
      case single :: Nil => tryTable(single)(clauses)(body)
      case _             => tryTable(WasmFunctionSignature(Nil, resultTypes))(clauses)(body)
    }
  }

  // Final result

  def buildAndAddToContext(): WasmFunction = {
    val sig = WasmFunctionSignature(_params.map(_.typ), _resultTypes)
    val typeName = ctx.addFunctionType(sig)
    val functionType = WasmFunctionType(typeName, sig)

    val expr = WasmExpr(instrs.toList)
    val func = WasmFunction(functionName, functionType, locals.all, expr)
    ctx.addFunction(func)
    func
  }
}

object WasmFunctionContext {
  sealed abstract class VarStorage

  object VarStorage {
    final case class Local(idx: LocalIdx) extends VarStorage

    final case class StructField(
        structIdx: LocalIdx,
        structTypeName: WasmTypeName,
        fieldIdx: WasmImmediate.StructFieldIdx
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
  )(implicit ctx: TypeDefinableWasmContext): WasmFunctionContext = {
    def makeCaptureLikeParamListAndEnv(
        captureParamName: String,
        captureLikes: Option[List[(IRNames.LocalName, IRTypes.Type)]]
    ): (List[WasmLocal], Env) = {
      captureLikes match {
        case None =>
          (Nil, Map.empty)

        case Some(captureLikes) =>
          val dataStructType = ctx.getClosureDataStructType(captureLikes.map(_._2))
          val local = WasmLocal(
            WasmLocalName(captureParamName),
            Types.WasmRefType(Types.WasmHeapType.Type(dataStructType.name)),
            isParameter = true
          )
          val localIdx = LocalIdx(local.name)
          val env: Env = captureLikes.zipWithIndex.map { case (captureLike, idx) =>
            val storage = VarStorage.StructField(
              localIdx,
              dataStructType.name,
              WasmImmediate.StructFieldIdx(idx)
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
      else Some(WasmLocal(WasmLocalName.newTarget, Types.WasmAnyRef, isParameter = true))
    val newTargetStorage = newTarget.map(local => VarStorage.Local(LocalIdx(local.name)))

    val receiver = receiverTyp.map { typ =>
      WasmLocal(WasmLocalName.receiver, typ, isParameter = true)
    }
    val receiverStorage = receiver.map(local => VarStorage.Local(LocalIdx(local.name)))

    val normalParams = paramDefsToWasmParams(paramDefs)
    val normalParamsEnv = paramDefs.zip(normalParams).map { case (paramDef, param) =>
      paramDef.name.name -> VarStorage.Local(LocalIdx(param.name))
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
      resultTypes
    )
  }

  def apply(
      enclosingClassName: Option[IRNames.ClassName],
      name: WasmFunctionName,
      captureParamDefs: Option[List[IRTrees.ParamDef]],
      receiverTyp: Option[WasmType],
      paramDefs: List[IRTrees.ParamDef],
      resultTypes: List[WasmType]
  )(implicit ctx: TypeDefinableWasmContext): WasmFunctionContext = {
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
  )(implicit ctx: TypeDefinableWasmContext): WasmFunctionContext = {
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
  )(implicit ctx: TypeDefinableWasmContext): WasmFunctionContext = {
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
  )(implicit ctx: TypeDefinableWasmContext): WasmFunctionContext = {
    val paramLocals = params.map { param =>
      WasmLocal(WasmLocalName.fromStr(param._1), param._2, isParameter = true)
    }
    new WasmFunctionContext(ctx, None, name, paramLocals, None, None, Map.empty, resultTypes)
  }

  def paramDefsToWasmParams(
      paramDefs: List[IRTrees.ParamDef]
  )(implicit ctx: TypeDefinableWasmContext): List[WasmLocal] = {
    paramDefs.map { paramDef =>
      WasmLocal(
        WasmLocalName.fromIR(paramDef.name.name),
        TypeTransformer.transformType(paramDef.ptpe),
        isParameter = true
      )
    }
  }
}
