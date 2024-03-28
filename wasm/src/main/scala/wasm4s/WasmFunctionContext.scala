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
    _receiver: Option[WasmLocal],
    _params: List[WasmLocal],
    _resultTypes: List[WasmType]
) {
  private var cnt = 0
  private var labelIdx = 0
  private var innerFuncIdx = 0

  val locals = new WasmSymbolTable[WasmLocalName, WasmLocal]()

  private val _receiverAndParams = _receiver.toList ::: _params

  _receiverAndParams.foreach(locals.define(_))

  def receiver: WasmLocal =
    _receiver.getOrElse(throw new Error("Cannot access to the receiver in this context."))

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

  def addLocal(name: WasmLocalName, typ: WasmType): LocalIdx = {
    val local = WasmLocal(name, typ, isParameter = false)
    locals.define(local)
    LocalIdx(name)
  }

  def addLocal(name: String, typ: WasmType): LocalIdx =
    addLocal(WasmLocalName(name), typ)

  def addLocal(name: IRNames.LocalName, typ: WasmType): LocalIdx =
    addLocal(WasmLocalName.fromIR(name), typ)

  def genSyntheticLocalName(): WasmLocalName = {
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
    val sig = WasmFunctionSignature(_receiverAndParams.map(_.typ), _resultTypes)
    val typeName = ctx.addFunctionType(sig)
    val functionType = WasmFunctionType(typeName, sig)

    val expr = WasmExpr(instrs.toList)
    val func = WasmFunction(functionName, functionType, locals.all, expr)
    ctx.addFunction(func)
    func
  }
}

object WasmFunctionContext {
  def apply(
      enclosingClassName: Option[IRNames.ClassName],
      name: WasmFunctionName,
      receiver: Option[WasmLocal],
      params: List[WasmLocal],
      resultTypes: List[WasmType]
  )(implicit ctx: TypeDefinableWasmContext): WasmFunctionContext = {
    new WasmFunctionContext(ctx, enclosingClassName, name, receiver, params, resultTypes)
  }

  def apply(
      enclosingClassName: Option[IRNames.ClassName],
      name: WasmFunctionName,
      receiverTyp: Option[WasmType],
      paramDefs: List[IRTrees.ParamDef],
      resultType: IRTypes.Type
  )(implicit ctx: TypeDefinableWasmContext): WasmFunctionContext = {
    val receiver = receiverTyp.map { typ =>
      WasmLocal(WasmLocalName.receiver, typ, isParameter = true)
    }
    val params = paramDefs.map { paramDef =>
      WasmLocal(
        WasmLocalName.fromIR(paramDef.name.name),
        TypeTransformer.transformType(paramDef.ptpe),
        isParameter = true
      )
    }
    apply(
      enclosingClassName,
      name,
      receiver,
      params,
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
    apply(enclosingClassName = None, name, receiver = None, paramLocals, resultTypes)
  }
}
