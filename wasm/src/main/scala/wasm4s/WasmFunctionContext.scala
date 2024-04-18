package wasm.wasm4s

import scala.collection.mutable

import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Trees => IRTrees}

import wasm.wasm4s.Names._
import wasm.wasm4s.Types.WasmType
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

  private val locals = mutable.ListBuffer.empty[WasmLocal]

  locals ++= _params

  def newTargetStorage: VarStorage.Local =
    _newTargetStorage.getOrElse(throw new Error("Cannot access new.target in this context."))

  def receiverStorage: VarStorage.Local =
    _receiverStorage.getOrElse(throw new Error("Cannot access to the receiver in this context."))

  def paramIndices: List[WasmLocalName] = _params.map(_.name)

  /** The instructions buffer; publicly mutable on purpose. */
  val instrs: mutable.ListBuffer[WasmInstr] = mutable.ListBuffer.empty

  def genLabel(): WasmLabelName = {
    val label = WasmLabelName.synthetic(labelIdx)
    labelIdx += 1
    label
  }

  private def addLocal(name: WasmLocalName, typ: WasmType): WasmLocalName = {
    locals += WasmLocal(name, typ, isParameter = false)
    name
  }

  def addLocal(name: String, typ: WasmType): WasmLocalName =
    addLocal(WasmLocalName(name), typ)

  private def addLocal(name: IRNames.LocalName, typ: WasmType): WasmLocalName =
    addLocal(WasmLocalName.fromIR(name), typ)

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

  private def genSyntheticLocalName(): WasmLocalName = {
    val name = WasmLocalName.synthetic(cnt)
    cnt += 1
    name
  }

  def addSyntheticLocal(typ: WasmType): WasmLocalName =
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

  def ifThenElse(sig: WasmFunctionSignature)(thenp: => Unit)(elsep: => Unit): Unit =
    ifThenElse(sigToBlockType(sig))(thenp)(elsep)

  def ifThenElse(resultTypes: List[WasmType])(thenp: => Unit)(elsep: => Unit): Unit =
    ifThenElse(WasmFunctionSignature(Nil, resultTypes))(thenp)(elsep)

  def ifThenElse()(thenp: => Unit)(elsep: => Unit): Unit =
    ifThenElse(BlockType.ValueType())(thenp)(elsep)

  def ifThen(blockType: BlockType)(thenp: => Unit): Unit = {
    instrs += IF(blockType)
    thenp
    instrs += END
  }

  def ifThen(sig: WasmFunctionSignature)(thenp: => Unit): Unit =
    ifThen(sigToBlockType(sig))(thenp)

  def ifThen(resultTypes: List[WasmType])(thenp: => Unit): Unit =
    ifThen(WasmFunctionSignature(Nil, resultTypes))(thenp)

  def ifThen()(thenp: => Unit): Unit =
    ifThen(BlockType.ValueType())(thenp)

  def block[A](blockType: BlockType)(body: WasmLabelName => A): A = {
    val label = genLabel()
    instrs += BLOCK(blockType, Some(label))
    val result = body(label)
    instrs += END
    result
  }

  def block[A](resultType: WasmType)(body: WasmLabelName => A): A =
    block(BlockType.ValueType(resultType))(body)

  def block[A]()(body: WasmLabelName => A): A =
    block(BlockType.ValueType())(body)

  def block[A](sig: WasmFunctionSignature)(body: WasmLabelName => A): A =
    block(sigToBlockType(sig))(body)

  def block[A](resultTypes: List[WasmType])(body: WasmLabelName => A): A =
    block(WasmFunctionSignature(Nil, resultTypes))(body)

  def loop[A](blockType: BlockType)(body: WasmLabelName => A): A = {
    val label = genLabel()
    instrs += LOOP(blockType, Some(label))
    val result = body(label)
    instrs += END
    result
  }

  def loop[A](resultType: WasmType)(body: WasmLabelName => A): A =
    loop(BlockType.ValueType(resultType))(body)

  def loop[A]()(body: WasmLabelName => A): A =
    loop(BlockType.ValueType())(body)

  def loop[A](sig: WasmFunctionSignature)(body: WasmLabelName => A): A =
    loop(sigToBlockType(sig))(body)

  def loop[A](resultTypes: List[WasmType])(body: WasmLabelName => A): A =
    loop(WasmFunctionSignature(Nil, resultTypes))(body)

  def whileLoop()(cond: => Unit)(body: => Unit): Unit = {
    loop() { loopLabel =>
      cond
      ifThen() {
        body
        instrs += BR(loopLabel)
      }
    }
  }

  def tryTable[A](blockType: BlockType)(clauses: List[CatchClause])(body: => A): A = {
    instrs += TRY_TABLE(blockType, clauses)
    val result = body
    instrs += END
    result
  }

  def tryTable[A](resultType: WasmType)(clauses: List[CatchClause])(body: => A): A =
    tryTable(BlockType.ValueType(resultType))(clauses)(body)

  def tryTable[A]()(clauses: List[CatchClause])(body: => A): A =
    tryTable(BlockType.ValueType())(clauses)(body)

  def tryTable[A](sig: WasmFunctionSignature)(clauses: List[CatchClause])(body: => A): A =
    tryTable(sigToBlockType(sig))(clauses)(body)

  def tryTable[A](resultTypes: List[WasmType])(clauses: List[CatchClause])(body: => A): A =
    tryTable(WasmFunctionSignature(Nil, resultTypes))(clauses)(body)

  /** Builds a `switch` over a scrutinee using a `br_table` instruction.
    *
    * This function produces code that encodes the following control-flow:
    *
    * {{{
    * switch (scrutinee) {
    *   case clause0_alt0 | ... | clause0_altN => clause0_body
    *   ...
    *   case clauseM_alt0 | ... | clauseM_altN => clauseM_body
    *   case _ => default
    * }
    * }}}
    *
    * All the alternative values must be non-negative and distinct, but they need not be
    * consecutive. The highest one must be strictly smaller than 128, as a safety precaution against
    * generating unexpectedly large tables.
    *
    * @param scrutineeSig
    *   The signature of the `scrutinee` block, *excluding* the i32 result that will be switched
    *   over.
    * @param clauseSig
    *   The signature of every `clauseI_body` block and of the `default` block. The clauses' params
    *   must consume at least all the results of the scrutinee.
    */
  def switch(
      scrutineeSig: WasmFunctionSignature,
      clauseSig: WasmFunctionSignature
  )(scrutinee: () => Unit)(clauses: (List[Int], () => Unit)*)(default: () => Unit): Unit = {
    val clauseLabels = clauses.map(_ => genLabel())

    // Build the dispatch vector, i.e., the array of caseValue -> target clauseLabel
    val numCases = clauses.map(_._1.max).max + 1
    if (numCases >= 128)
      throw new IllegalArgumentException(s"Too many cases for switch: $numCases")
    val dispatchVector = new Array[WasmLabelName](numCases)
    for {
      (clause, clauseLabel) <- clauses.zip(clauseLabels)
      caseValue <- clause._1
    } {
      if (dispatchVector(caseValue) != null)
        throw new IllegalArgumentException(s"Duplicate case value for switch: $caseValue")
      dispatchVector(caseValue) = clauseLabel
    }

    // Compute the BlockType's we will need
    require(
      clauseSig.params.size >= scrutineeSig.results.size,
      "The clauses of a switch must consume all the results of the scrutinee " +
        s"(scrutinee results: ${scrutineeSig.results}; clause params: ${clauseSig.params})"
    )
    val (doneBlockType, clauseBlockType) = {
      val clauseParamsComingFromAbove = clauseSig.params.drop(scrutineeSig.results.size)
      val doneBlockSig = WasmFunctionSignature(
        clauseParamsComingFromAbove ::: scrutineeSig.params,
        clauseSig.results
      )
      val clauseBlockSig = WasmFunctionSignature(
        clauseParamsComingFromAbove ::: scrutineeSig.params,
        clauseSig.params
      )
      (sigToBlockType(doneBlockSig), sigToBlockType(clauseBlockSig))
    }

    block(doneBlockType) { doneLabel =>
      block(clauseBlockType) { defaultLabel =>
        // Fill up empty entries of the dispatch vector with the default label
        for (i <- 0 until numCases if dispatchVector(i) == null)
          dispatchVector(i) = defaultLabel

        // Enter all the case labels
        for (clauseLabel <- clauseLabels.reverse)
          instrs += BLOCK(clauseBlockType, Some(clauseLabel))

        // Load the scrutinee and dispatch
        scrutinee()
        instrs += BR_TABLE(dispatchVector.toList, defaultLabel)

        // Close all the case labels and emit their respective bodies
        for (clause <- clauses) {
          instrs += END // close the block whose label is the corresponding label for this clause
          clause._2() // emit the body of that clause
          instrs += BR(doneLabel) // jump to done
        }
      }

      default()
    }
  }

  def switch(
      clauseSig: WasmFunctionSignature
  )(scrutinee: () => Unit)(clauses: (List[Int], () => Unit)*)(default: () => Unit): Unit = {
    switch(WasmFunctionSignature.NilToNil, clauseSig)(scrutinee)(clauses: _*)(default)
  }

  def switch(
      resultType: WasmType
  )(scrutinee: () => Unit)(clauses: (List[Int], () => Unit)*)(default: () => Unit): Unit = {
    switch(WasmFunctionSignature(Nil, List(resultType)))(scrutinee)(clauses: _*)(default)
  }

  def switch()(
      scrutinee: () => Unit
  )(clauses: (List[Int], () => Unit)*)(default: () => Unit): Unit = {
    switch(WasmFunctionSignature.NilToNil)(scrutinee)(clauses: _*)(default)
  }

  // Final result

  def buildAndAddToContext(): WasmFunction = {
    val sig = WasmFunctionSignature(_params.map(_.typ), _resultTypes)
    buildAndAddToContext(ctx.addFunctionType(sig))
  }

  def buildAndAddToContext(useFunctionTypeInMainRecType: Boolean): WasmFunction = {
    val sig = WasmFunctionSignature(_params.map(_.typ), _resultTypes)
    val functionTypeName =
      if (useFunctionTypeInMainRecType) ctx.addFunctionTypeInMainRecType(sig)
      else ctx.addFunctionType(sig)
    buildAndAddToContext(functionTypeName)
  }

  def buildAndAddToContext(functionTypeName: WasmTypeName): WasmFunction = {
    val dcedInstrs = localDeadCodeEliminationOfInstrs()

    val expr = WasmExpr(dcedInstrs)
    val func = WasmFunction(functionName, functionTypeName, locals.toList, _resultTypes, expr)
    ctx.addFunction(func)
    func
  }

  /** Performs local dead code elimination and produces the final list of instructions.
    *
    * After a stack-polymorphic instruction, the rest of the block is unreachable. In theory,
    * WebAssembly specifies that the rest of the block should be type-checkeable no matter the
    * contents of the stack. In practice, however, it seems V8 cannot handle `throw_ref` in such a
    * context. It reports a validation error of the form "invalid type for throw_ref: expected
    * exnref, found <bot>".
    *
    * We work around this issue by forcing a pass of local dead-code elimination. This is in fact
    * straightforwrd: after every stack-polymorphic instruction, ignore all instructions until the
    * next `ELSE` or `END`. The only tricky bit is that if we encounter nested
    * `StructuredLabeledInstr`s during that process, must jump over them. That means we need to
    * track the level of nesting at which we are.
    */
  private def localDeadCodeEliminationOfInstrs(): List[WasmInstr] = {
    val resultBuilder = List.newBuilder[WasmInstr]

    val iter = instrs.iterator
    while (iter.hasNext) {
      // Emit the current instruction
      val instr = iter.next()
      resultBuilder += instr

      /* If it is a stack-polymorphic instruction, dead-code eliminate until the
       * end of the current block.
       */
      if (instr.isInstanceOf[WasmInstr.StackPolymorphicInstr]) {
        var nestingLevel = 0

        while (nestingLevel >= 0 && iter.hasNext) {
          val deadCodeInstr = iter.next()
          deadCodeInstr match {
            case END | ELSE | _: CATCH | CATCH_ALL if nestingLevel == 0 =>
              /* We have reached the end of the original block of dead code.
               * Actually emit this END or ELSE and then drop `nestingLevel`
               * below 0 to end the dead code processing loop.
               */
              resultBuilder += deadCodeInstr
              nestingLevel = -1 // acts as a `break` instruction

            case END =>
              nestingLevel -= 1

            case _: StructuredLabeledInstr =>
              nestingLevel += 1

            case _ =>
              ()
          }
        }
      }
    }

    resultBuilder.result()
  }
}

object WasmFunctionContext {
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
  )(implicit ctx: TypeDefinableWasmContext): WasmFunctionContext = {
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
      else Some(WasmLocal(WasmLocalName.newTarget, Types.WasmRefType.anyref, isParameter = true))
    val newTargetStorage = newTarget.map(local => VarStorage.Local(local.name))

    val receiver = receiverTyp.map { typ =>
      WasmLocal(WasmLocalName.receiver, typ, isParameter = true)
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
