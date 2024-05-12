package org.scalajs.linker.backend.webassembly

import scala.collection.mutable

import org.scalajs.ir.Position

import Instructions._
import Names._
import Modules._
import Types.WasmType

final class FunctionBuilder(
    moduleBuilder: ModuleBuilder,
    val functionName: WasmFunctionName,
    functionPos: Position
) {
  import FunctionBuilder._

  private var labelIdx = 0

  private val params = mutable.ListBuffer.empty[WasmLocal]
  private val locals = mutable.ListBuffer.empty[WasmLocal]
  private var resultTypes: List[WasmType] = Nil

  private var specialFunctionType: Option[WasmTypeName] = None

  /** The instructions buffer. */
  private val instrs: mutable.ListBuffer[WasmInstr] = mutable.ListBuffer.empty

  def setFunctionType(typ: WasmTypeName): Unit =
    specialFunctionType = Some(typ)

  def setResultTypes(typs: List[WasmType]): Unit =
    resultTypes = typs

  def setResultType(typ: WasmType): Unit =
    setResultTypes(typ :: Nil)

  def addParam(name: WasmLocalName, typ: WasmType): WasmLocalName = {
    params += WasmLocal(name, typ, isParameter = true)
    name
  }

  def addParam(name: String, typ: WasmType): WasmLocalName =
    addParam(WasmLocalName(name), typ)

  def genLabel(): WasmLabelName = {
    val label = WasmLabelName(labelIdx.toString())
    labelIdx += 1
    label
  }

  def addLocal(name: WasmLocalName, typ: WasmType): WasmLocalName = {
    locals += WasmLocal(name, typ, isParameter = false)
    name
  }

  def addLocal(name: String, typ: WasmType): WasmLocalName =
    addLocal(WasmLocalName(name), typ)

  // Instructions

  def +=(instr: WasmInstr): Unit =
    instrs += instr

  def ++=(instrs: Iterable[WasmInstr]): Unit =
    this.instrs ++= instrs

  def markCurrentInstructionIndex(): InstructionIndex =
    new InstructionIndex(instrs.size)

  def insert(index: InstructionIndex, instr: WasmInstr): Unit =
    instrs.insert(index.value, instr)

  // Helpers to build structured control flow

  def sigToBlockType(sig: WasmFunctionSignature): BlockType = sig match {
    case WasmFunctionSignature(Nil, Nil) =>
      BlockType.ValueType()
    case WasmFunctionSignature(Nil, resultType :: Nil) =>
      BlockType.ValueType(resultType)
    case _ =>
      BlockType.FunctionType(moduleBuilder.signatureToTypeName(sig))
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

  def buildAndAddToModule(): WasmFunction = {
    val functionTypeName = specialFunctionType.getOrElse {
      val sig = WasmFunctionSignature(params.toList.map(_.typ), resultTypes)
      moduleBuilder.signatureToTypeName(sig)
    }

    val dcedInstrs = localDeadCodeEliminationOfInstrs()

    val expr = WasmExpr(dcedInstrs)
    val allLocals = params.prependToList(locals.toList)
    val func =
      WasmFunction(functionName, functionTypeName, allLocals, resultTypes, expr, functionPos)
    moduleBuilder.addFunction(func)
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
      if (instr.isInstanceOf[StackPolymorphicInstr]) {
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

object FunctionBuilder {
  final class InstructionIndex(private[FunctionBuilder] val value: Int) extends AnyVal
}
