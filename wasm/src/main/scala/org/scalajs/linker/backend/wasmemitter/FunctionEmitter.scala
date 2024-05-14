package org.scalajs.linker.backend.wasmemitter

import scala.annotation.switch

import scala.collection.mutable

import org.scalajs.ir.{ClassKind, Position}
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Names => wanme}
import org.scalajs.linker.backend.webassembly.{Types => watpe}
import org.scalajs.linker.backend.webassembly.Modules.{FunctionSignature => Sig}

import EmbeddedConstants._
import SWasmGen._
import VarGen._

object FunctionEmitter {

  /** Whether to use the legacy `try` instruction to implement `TryCatch`.
    *
    * Support for catching JS exceptions was only added to `try_table` in V8 12.5 from April 2024.
    * While waiting for Node.js to catch up with V8, we use `try` to implement our `TryCatch`.
    *
    * We use this "fixed configuration option" to keep the code that implements `TryCatch` using
    * `try_table` in the codebase, as code that is actually compiled, so that refactorings apply to
    * it as well. It also makes it easier to manually experiment with the new `try_table` encoding,
    * which will become available in Chrome v125.
    *
    * Note that we use `try_table` regardless to implement `TryFinally`. Its `catch_all_ref` handler
    * is perfectly happy to catch and rethrow JavaScript exception in Node.js 22. Duplicating that
    * implementation for `try` would be a nightmare, given how complex it is already.
    */
  private final val UseLegacyExceptionsForTryCatch = true

  def emitFunction(
      functionName: wanme.FunctionName,
      enclosingClassName: Option[ClassName],
      captureParamDefs: Option[List[ParamDef]],
      receiverTyp: Option[watpe.Type],
      paramDefs: List[ParamDef],
      restParam: Option[ParamDef],
      body: Tree,
      resultType: Type
  )(implicit ctx: WasmContext, pos: Position): Unit = {
    val emitter = prepareEmitter(
      functionName,
      enclosingClassName,
      captureParamDefs,
      preSuperVarDefs = None,
      hasNewTarget = false,
      receiverTyp,
      paramDefs ::: restParam.toList,
      TypeTransformer.transformResultType(resultType)
    )
    emitter.genBody(body, resultType)
    emitter.fb.buildAndAddToModule()
  }

  def emitJSConstructorFunctions(
      preSuperStatsFunctionName: wanme.FunctionName,
      superArgsFunctionName: wanme.FunctionName,
      postSuperStatsFunctionName: wanme.FunctionName,
      enclosingClassName: ClassName,
      jsClassCaptures: List[ParamDef],
      ctor: JSConstructorDef
  )(implicit ctx: WasmContext): Unit = {
    implicit val pos = ctor.pos

    val allCtorParams = ctor.args ::: ctor.restParam.toList
    val ctorBody = ctor.body

    // Compute the pre-super environment
    val preSuperDecls = ctorBody.beforeSuper.collect { case varDef: VarDef =>
      varDef
    }

    // Build the `preSuperStats` function
    locally {
      val preSuperEnvStructTypeName = ctx.getClosureDataStructType(preSuperDecls.map(_.vtpe))
      val preSuperEnvTyp = watpe.RefType(preSuperEnvStructTypeName)

      val emitter = prepareEmitter(
        preSuperStatsFunctionName,
        Some(enclosingClassName),
        Some(jsClassCaptures),
        preSuperVarDefs = None,
        hasNewTarget = true,
        receiverTyp = None,
        allCtorParams,
        List(preSuperEnvTyp)
      )

      emitter.genBlockStats(ctorBody.beforeSuper) {
        // Build and return the preSuperEnv struct
        for (varDef <- preSuperDecls)
          emitter.fb += wa.LOCAL_GET(emitter.lookupLocalAssertLocalStorage(varDef.name.name))
        emitter.fb += wa.STRUCT_NEW(preSuperEnvStructTypeName)
      }

      emitter.fb.buildAndAddToModule()
    }

    // Build the `superArgs` function
    locally {
      val emitter = prepareEmitter(
        superArgsFunctionName,
        Some(enclosingClassName),
        Some(jsClassCaptures),
        Some(preSuperDecls),
        hasNewTarget = true,
        receiverTyp = None,
        allCtorParams,
        List(watpe.RefType.anyref) // a js.Array
      )
      emitter.genBody(JSArrayConstr(ctorBody.superCall.args), AnyType)
      emitter.fb.buildAndAddToModule()
    }

    // Build the `postSuperStats` function
    locally {
      val emitter = prepareEmitter(
        postSuperStatsFunctionName,
        Some(enclosingClassName),
        Some(jsClassCaptures),
        Some(preSuperDecls),
        hasNewTarget = true,
        receiverTyp = Some(watpe.RefType.anyref),
        allCtorParams,
        List(watpe.RefType.anyref)
      )
      emitter.genBody(Block(ctorBody.afterSuper), AnyType)
      emitter.fb.buildAndAddToModule()
    }
  }

  private def prepareEmitter(
      functionName: wanme.FunctionName,
      enclosingClassName: Option[ClassName],
      captureParamDefs: Option[List[ParamDef]],
      preSuperVarDefs: Option[List[VarDef]],
      hasNewTarget: Boolean,
      receiverTyp: Option[watpe.Type],
      paramDefs: List[ParamDef],
      resultTypes: List[watpe.Type]
  )(implicit ctx: WasmContext, pos: Position): FunctionEmitter = {
    val fb = new FunctionBuilder(ctx.moduleBuilder, functionName, pos)

    def addCaptureLikeParamListAndMakeEnv(
        captureParamName: String,
        captureLikes: Option[List[(LocalName, Type)]]
    ): Env = {
      captureLikes match {
        case None =>
          Map.empty

        case Some(captureLikes) =>
          val dataStructTypeName = ctx.getClosureDataStructType(captureLikes.map(_._2))
          val param = fb.addParam(captureParamName, watpe.RefType(dataStructTypeName))
          val env: Env = captureLikes.zipWithIndex.map { case (captureLike, idx) =>
            val storage = VarStorage.StructField(
              param,
              dataStructTypeName,
              wanme.FieldIdx(idx)
            )
            captureLike._1 -> storage
          }.toMap
          env
      }
    }

    val captureParamsEnv = addCaptureLikeParamListAndMakeEnv(
      "__captureData",
      captureParamDefs.map(_.map(p => p.name.name -> p.ptpe))
    )

    val preSuperEnvEnv = addCaptureLikeParamListAndMakeEnv(
      "__preSuperEnv",
      preSuperVarDefs.map(_.map(p => p.name.name -> p.vtpe))
    )

    val newTargetStorage = if (!hasNewTarget) {
      None
    } else {
      val newTargetParam = fb.addParam(newTargetLocalName, watpe.RefType.anyref)
      Some(VarStorage.Local(newTargetParam))
    }

    val receiverStorage = receiverTyp.map { typ =>
      val receiverParam = fb.addParam(receiverLocalName, typ)
      VarStorage.Local(receiverParam)
    }

    val normalParamsEnv = paramDefs.map { paramDef =>
      val param = fb.addParam(
        localNameFromIR(paramDef.name.name),
        TypeTransformer.transformType(paramDef.ptpe)
      )
      paramDef.name.name -> VarStorage.Local(param)
    }

    val fullEnv = captureParamsEnv ++ preSuperEnvEnv ++ normalParamsEnv

    fb.setResultTypes(resultTypes)

    new FunctionEmitter(
      ctx,
      fb,
      enclosingClassName,
      newTargetStorage,
      receiverStorage,
      fullEnv
    )
  }

  private val ObjectRef = ClassRef(ObjectClass)
  private val BoxedStringRef = ClassRef(BoxedStringClass)
  private val toStringMethodName = MethodName("toString", Nil, BoxedStringRef)
  private val equalsMethodName = MethodName("equals", List(ObjectRef), BooleanRef)
  private val compareToMethodName = MethodName("compareTo", List(ObjectRef), IntRef)

  private val CharSequenceClass = ClassName("java.lang.CharSequence")
  private val ComparableClass = ClassName("java.lang.Comparable")
  private val JLNumberClass = ClassName("java.lang.Number")

  private val newTargetLocalName = wanme.LocalName("new.target")
  private val receiverLocalName = wanme.LocalName("___<this>")

  private def localNameFromIR(name: LocalName): wanme.LocalName =
    wanme.LocalName(name.nameString)

  private sealed abstract class VarStorage

  private object VarStorage {
    final case class Local(idx: wanme.LocalName) extends VarStorage

    final case class StructField(
        structIdx: wanme.LocalName,
        structTypeName: wanme.TypeName,
        fieldIdx: wanme.FieldIdx
    ) extends VarStorage
  }

  private type Env = Map[LocalName, VarStorage]
}

private class FunctionEmitter private (
    ctx: WasmContext,
    val fb: FunctionBuilder,
    enclosingClassName: Option[ClassName],
    _newTargetStorage: Option[FunctionEmitter.VarStorage.Local],
    _receiverStorage: Option[FunctionEmitter.VarStorage.Local],
    paramsEnv: FunctionEmitter.Env
) {
  import FunctionEmitter._

  private val instrs = fb

  private var nextSyntheticLocalIndex = 0
  private var innerFuncIdx = 0
  private var currentEnv: Env = paramsEnv

  private def newTargetStorage: VarStorage.Local =
    _newTargetStorage.getOrElse(throw new Error("Cannot access new.target in this context."))

  private def receiverStorage: VarStorage.Local =
    _receiverStorage.getOrElse(throw new Error("Cannot access to the receiver in this context."))

  private def withNewLocal[A](name: LocalName, typ: watpe.Type)(
      body: wanme.LocalName => A
  ): A = {
    val savedEnv = currentEnv
    val local = fb.addLocal(localNameFromIR(name), typ)
    currentEnv = currentEnv.updated(name, VarStorage.Local(local))
    try body(local)
    finally currentEnv = savedEnv
  }

  private def lookupLocal(name: LocalName): VarStorage = {
    currentEnv.getOrElse(
      name, {
        throw new AssertionError(s"Cannot find binding for '${name.nameString}'")
      }
    )
  }

  private def lookupLocalAssertLocalStorage(name: LocalName): wanme.LocalName = {
    (lookupLocal(name): @unchecked) match {
      case VarStorage.Local(local) => local
    }
  }

  private def addSyntheticLocal(typ: watpe.Type): wanme.LocalName = {
    val name = wanme.LocalName(s"local___$nextSyntheticLocalIndex")
    nextSyntheticLocalIndex += 1
    fb.addLocal(name, typ)
  }

  private def genInnerFuncName(): wanme.FunctionName = {
    val innerName = wanme.FunctionName(fb.functionName.name + "__c" + innerFuncIdx)
    innerFuncIdx += 1
    innerName
  }

  private def markPosition(tree: Tree): Unit =
    instrs += wa.PositionMark(tree.pos)

  def genBody(tree: Tree, expectedType: Type): Unit =
    genTree(tree, expectedType)

  def genTrees(trees: List[Tree], expectedTypes: List[Type]): Unit = {
    for ((tree, expectedType) <- trees.zip(expectedTypes))
      genTree(tree, expectedType)
  }

  def genTreeAuto(tree: Tree): Unit =
    genTree(tree, tree.tpe)

  def genTree(tree: Tree, expectedType: Type): Unit = {
    val generatedType: Type = tree match {
      case t: Literal             => genLiteral(t, expectedType)
      case t: UnaryOp             => genUnaryOp(t)
      case t: BinaryOp            => genBinaryOp(t)
      case t: VarRef              => genVarRef(t)
      case t: LoadModule          => genLoadModule(t)
      case t: StoreModule         => genStoreModule(t)
      case t: This                => genThis(t)
      case t: ApplyStatically     => genApplyStatically(t)
      case t: Apply               => genApply(t)
      case t: ApplyStatic         => genApplyStatic(t)
      case t: ApplyDynamicImport  => genApplyDynamicImport(t)
      case t: IsInstanceOf        => genIsInstanceOf(t)
      case t: AsInstanceOf        => genAsInstanceOf(t)
      case t: GetClass            => genGetClass(t)
      case t: Block               => genBlock(t, expectedType)
      case t: Labeled             => unwinding.genLabeled(t, expectedType)
      case t: Return              => unwinding.genReturn(t)
      case t: Select              => genSelect(t)
      case t: SelectStatic        => genSelectStatic(t)
      case t: Assign              => genAssign(t)
      case t: VarDef              => genVarDef(t)
      case t: New                 => genNew(t)
      case t: If                  => genIf(t, expectedType)
      case t: While               => genWhile(t)
      case t: ForIn               => genForIn(t)
      case t: TryCatch            => genTryCatch(t, expectedType)
      case t: TryFinally          => unwinding.genTryFinally(t, expectedType)
      case t: Throw               => genThrow(t)
      case t: Match               => genMatch(t, expectedType)
      case t: Debugger            => NoType // ignore
      case t: Skip                => NoType
      case t: Clone               => genClone(t)
      case t: IdentityHashCode    => genIdentityHashCode(t)
      case t: WrapAsThrowable     => genWrapAsThrowable(t)
      case t: UnwrapFromThrowable => genUnwrapFromThrowable(t)

      // JavaScript expressions
      case t: JSNew                => genJSNew(t)
      case t: JSSelect             => genJSSelect(t)
      case t: JSFunctionApply      => genJSFunctionApply(t)
      case t: JSMethodApply        => genJSMethodApply(t)
      case t: JSImportCall         => genJSImportCall(t)
      case t: JSImportMeta         => genJSImportMeta(t)
      case t: LoadJSConstructor    => genLoadJSConstructor(t)
      case t: LoadJSModule         => genLoadJSModule(t)
      case t: SelectJSNativeMember => genSelectJSNativeMember(t)
      case t: JSDelete             => genJSDelete(t)
      case t: JSUnaryOp            => genJSUnaryOp(t)
      case t: JSBinaryOp           => genJSBinaryOp(t)
      case t: JSArrayConstr        => genJSArrayConstr(t)
      case t: JSObjectConstr       => genJSObjectConstr(t)
      case t: JSGlobalRef          => genJSGlobalRef(t)
      case t: JSTypeOfGlobalRef    => genJSTypeOfGlobalRef(t)
      case t: JSLinkingInfo        => genJSLinkingInfo(t)
      case t: Closure              => genClosure(t)

      // array
      case t: ArrayLength => genArrayLength(t)
      case t: NewArray    => genNewArray(t)
      case t: ArraySelect => genArraySelect(t)
      case t: ArrayValue  => genArrayValue(t)

      // Non-native JS classes
      case t: CreateJSClass     => genCreateJSClass(t)
      case t: JSPrivateSelect   => genJSPrivateSelect(t)
      case t: JSSuperSelect     => genJSSuperSelect(t)
      case t: JSSuperMethodCall => genJSSuperMethodCall(t)
      case t: JSNewTarget       => genJSNewTarget(t)

      case _: RecordSelect | _: RecordValue | _: Transient | _: JSSuperConstructorCall =>
        throw new AssertionError(s"Invalid tree: $tree")
    }

    genAdapt(generatedType, expectedType)
  }

  private def genAdapt(generatedType: Type, expectedType: Type): Unit = {
    (generatedType, expectedType) match {
      case _ if generatedType == expectedType =>
        ()
      case (NothingType, _) =>
        ()
      case (_, NoType) =>
        instrs += wa.DROP
      case (primType: PrimTypeWithRef, _) =>
        // box
        primType match {
          case NullType =>
            ()
          case CharType =>
            /* `char` and `long` are opaque to JS in the Scala.js semantics.
             * We implement them with real Wasm classes following the correct
             * vtable. Upcasting wraps a primitive into the corresponding class.
             */
            genBox(CharType, SpecialNames.CharBoxClass)
          case LongType =>
            genBox(LongType, SpecialNames.LongBoxClass)
          case NoType | NothingType =>
            throw new AssertionError(s"Unexpected adaptation from $primType to $expectedType")
          case _ =>
            /* Calls a `bX` helper. Most of them are of the form
             *   bX: (x) => x
             * at the JavaScript level, but with a primType->anyref Wasm type.
             * For example, for `IntType`, `bI` has type `i32 -> anyref`. This
             * asks the JS host to turn a primitive `i32` into its generic
             * representation, which we can store in an `anyref`.
             */
            instrs += wa.CALL(genFunctionName.box(primType.primRef))
        }
      case _ =>
        ()
    }
  }

  private def genAssign(t: Assign): Type = {
    t.lhs match {
      case sel: Select =>
        val className = sel.field.name.className
        val classInfo = ctx.getClassInfo(className)

        // For Select, the receiver can never be a hijacked class, so we can use genTreeAuto
        genTreeAuto(sel.qualifier)

        if (!classInfo.hasInstances) {
          /* The field may not exist in that case, and we cannot look it up.
           * However we necessarily have a `null` receiver if we reach this
           * point, so we can trap as NPE.
           */
          instrs += wa.UNREACHABLE
        } else {
          val fieldName = genFieldName.forClassInstanceField(sel.field.name)
          val idx = ctx.getClassInfo(className).getFieldIdx(sel.field.name)

          genTree(t.rhs, t.lhs.tpe)
          instrs += wa.STRUCT_SET(genTypeName.forClass(className), idx)
        }

      case sel: SelectStatic =>
        val fieldName = sel.field.name
        val globalName = genGlobalName.forStaticField(fieldName)

        genTree(t.rhs, sel.tpe)
        instrs += wa.GLOBAL_SET(globalName)

        // Update top-level export mirrors
        val classInfo = ctx.getClassInfo(fieldName.className)
        val mirrors = classInfo.staticFieldMirrors.getOrElse(fieldName, Nil)
        for (exportedName <- mirrors) {
          instrs += wa.GLOBAL_GET(globalName)
          instrs += wa.CALL(genFunctionName.forTopLevelExportSetter(exportedName))
        }

      case sel: ArraySelect =>
        genTreeAuto(sel.array)
        sel.array.tpe match {
          case ArrayType(arrayTypeRef) =>
            // Get the underlying array; implicit trap on null
            instrs += wa.STRUCT_GET(
              genTypeName.forArrayClass(arrayTypeRef),
              genFieldIdx.objStruct.uniqueRegularField
            )
            genTree(sel.index, IntType)
            genTree(t.rhs, sel.tpe)
            instrs += wa.ARRAY_SET(genTypeName.underlyingOf(arrayTypeRef))
          case NothingType =>
            // unreachable
            ()
          case NullType =>
            instrs += wa.UNREACHABLE
          case _ =>
            throw new IllegalArgumentException(
              s"ArraySelect.array must be an array type, but has type ${sel.array.tpe}"
            )
        }

      case sel: JSPrivateSelect =>
        genTree(sel.qualifier, AnyType)
        instrs += wa.GLOBAL_GET(genGlobalName.forJSPrivateField(sel.field.name))
        genTree(t.rhs, AnyType)
        instrs += wa.CALL(genFunctionName.jsSelectSet)

      case assign: JSSelect =>
        genTree(assign.qualifier, AnyType)
        genTree(assign.item, AnyType)
        genTree(t.rhs, AnyType)
        instrs += wa.CALL(genFunctionName.jsSelectSet)

      case assign: JSSuperSelect =>
        genTree(assign.superClass, AnyType)
        genTree(assign.receiver, AnyType)
        genTree(assign.item, AnyType)
        genTree(t.rhs, AnyType)
        instrs += wa.CALL(genFunctionName.jsSuperSet)

      case assign: JSGlobalRef =>
        instrs ++= ctx.getConstantStringInstr(assign.name)
        genTree(t.rhs, AnyType)
        instrs += wa.CALL(genFunctionName.jsGlobalRefSet)

      case ref: VarRef =>
        lookupLocal(ref.ident.name) match {
          case VarStorage.Local(local) =>
            genTree(t.rhs, t.lhs.tpe)
            instrs += wa.LOCAL_SET(local)
          case VarStorage.StructField(structLocal, structTypeName, fieldIdx) =>
            instrs += wa.LOCAL_GET(structLocal)
            genTree(t.rhs, t.lhs.tpe)
            instrs += wa.STRUCT_SET(structTypeName, fieldIdx)
        }

      case assign: RecordSelect =>
        throw new AssertionError(s"Invalid tree: $t")
    }

    NoType
  }

  private def genApply(t: Apply): Type = {
    t.receiver.tpe match {
      case NothingType =>
        genTree(t.receiver, NothingType)
        // nothing else to do; this is unreachable
        NothingType

      case NullType =>
        genTree(t.receiver, NullType)
        instrs += wa.UNREACHABLE // trap
        NothingType

      case _ if t.method.name.isReflectiveProxy =>
        genReflectiveCall(t)

      case _ =>
        val receiverClassName = t.receiver.tpe match {
          case prim: PrimType  => PrimTypeToBoxedClass(prim)
          case ClassType(cls)  => cls
          case AnyType         => ObjectClass
          case ArrayType(_)    => ObjectClass
          case tpe: RecordType => throw new AssertionError(s"Invalid receiver type $tpe")
        }
        val receiverClassInfo = ctx.getClassInfo(receiverClassName)

        val canUseStaticallyResolved = {
          receiverClassInfo.kind == ClassKind.HijackedClass ||
          t.receiver.tpe.isInstanceOf[ArrayType] ||
          receiverClassInfo.resolvedMethodInfos.get(t.method.name).exists(_.isEffectivelyFinal)
        }
        if (canUseStaticallyResolved) {
          genApplyStatically(
            ApplyStatically(t.flags, t.receiver, receiverClassName, t.method, t.args)(
              t.tpe
            )(
              t.pos
            )
          )
        } else {
          genApplyWithDispatch(t, receiverClassInfo)
        }
    }
  }

  private def genReflectiveCall(t: Apply): Type = {
    assert(t.method.name.isReflectiveProxy)
    val receiverLocalForDispatch =
      addSyntheticLocal(watpe.RefType.any)

    val proxyId = ctx.getReflectiveProxyId(t.method.name)
    val funcTypeName = ctx.tableFunctionType(t.method.name)

    instrs.block(watpe.RefType.anyref) { done =>
      instrs.block(watpe.RefType.any) { labelNotOurObject =>
        // arguments
        genTree(t.receiver, AnyType)
        instrs += wa.REF_AS_NOT_NULL
        instrs += wa.LOCAL_TEE(receiverLocalForDispatch)
        genArgs(t.args, t.method.name)

        // Looks up the method to be (reflectively) called
        instrs += wa.LOCAL_GET(receiverLocalForDispatch)
        instrs += wa.BR_ON_CAST_FAIL(
          labelNotOurObject,
          watpe.RefType.any,
          watpe.RefType(genTypeName.ObjectStruct)
        )
        instrs += wa.STRUCT_GET(
          genTypeName.forClass(ObjectClass),
          genFieldIdx.objStruct.vtable
        )
        instrs += wa.I32_CONST(proxyId)
        // `searchReflectiveProxy`: [typeData, i32] -> [(ref func)]
        instrs += wa.CALL(genFunctionName.searchReflectiveProxy)

        instrs += wa.REF_CAST(watpe.RefType(watpe.HeapType(funcTypeName)))
        instrs += wa.CALL_REF(funcTypeName)
        instrs += wa.BR(done)
      } // labelNotFound
      instrs += wa.UNREACHABLE
      // TODO? reflective call on primitive types
      t.tpe
    }
    // done
  }

  /** Generates the code an `Apply` call that requires dynamic dispatch.
    *
    * In that case, there is always at least a vtable/itable-based dispatch. It may also contain
    * primitive-based dispatch if the receiver's type is an ancestor of a hijacked class.
    */
  private def genApplyWithDispatch(
      t: Apply,
      receiverClassInfo: WasmContext.ClassInfo
  ): Type = {
    implicit val pos: Position = t.pos

    val receiverClassName = receiverClassInfo.name

    /* Similar to transformType(t.receiver.tpe), but:
     * - it is non-null,
     * - ancestors of hijacked classes are not treated specially,
     * - array types are treated as j.l.Object.
     *
     * This is used in the code paths where we have already ruled out `null`
     * values and primitive values (that implement hijacked classes).
     */
    val refTypeForDispatch: watpe.RefType = {
      if (receiverClassInfo.isInterface)
        watpe.RefType(genTypeName.ObjectStruct)
      else
        watpe.RefType(genTypeName.forClass(receiverClassName))
    }

    // A local for a copy of the receiver that we will use to resolve dispatch
    val receiverLocalForDispatch = addSyntheticLocal(refTypeForDispatch)

    /* Gen loading of the receiver and check that it is non-null.
     * After this codegen, the non-null receiver is on the stack.
     */
    def genReceiverNotNull(): Unit = {
      genTreeAuto(t.receiver)
      instrs += wa.REF_AS_NOT_NULL
    }

    /* Generates a resolved call to a method of a hijacked class.
     * Before this code gen, the stack must contain the receiver and the args.
     * After this code gen, the stack contains the result.
     */
    def genHijackedClassCall(hijackedClass: ClassName): Unit = {
      val funcName = genFunctionName.forMethod(
        MemberNamespace.Public,
        hijackedClass,
        t.method.name
      )
      instrs += wa.CALL(funcName)
    }

    if (!receiverClassInfo.hasInstances) {
      /* If the target class info does not have any instance, the only possible
       * for the receiver is `null`. We can therefore immediately trap for an
       * NPE. It is important to short-cut this path because the reachability
       * analysis may have dead-code eliminated the target method method
       * entirely, which means we do not know its signature and therefore
       * cannot emit the corresponding vtable/itable calls.
       */
      genTreeAuto(t.receiver)
      instrs += wa.UNREACHABLE // NPE
    } else if (!receiverClassInfo.isAncestorOfHijackedClass) {
      // Standard dispatch codegen
      genReceiverNotNull()
      instrs += wa.LOCAL_TEE(receiverLocalForDispatch)
      genArgs(t.args, t.method.name)
      genTableDispatch(receiverClassInfo, t.method.name, receiverLocalForDispatch)
    } else {
      /* Here the receiver's type is an ancestor of a hijacked class (or `any`,
       * which is treated as `jl.Object`).
       *
       * We must emit additional dispatch for the possible primitive values.
       *
       * The overall structure of the generated code is as follows:
       *
       * block resultType $done
       *   block (ref any) $notOurObject
       *     load non-null receiver and args and store into locals
       *     reload copy of receiver
       *     br_on_cast_fail (ref any) (ref $targetRealClass) $notOurObject
       *     reload args
       *     generate standard table-based dispatch
       *     br $done
       *   end $notOurObject
       *   choose an implementation of a single hijacked class, or a JS helper
       *   reload args
       *   call the chosen implementation
       * end $done
       */

      assert(receiverClassInfo.kind != ClassKind.HijackedClass, receiverClassName)

      val resultTyp = TypeTransformer.transformResultType(t.tpe)(ctx)

      instrs.block(resultTyp) { labelDone =>
        def pushArgs(argsLocals: List[wanme.LocalName]): Unit =
          argsLocals.foreach(argLocal => instrs += wa.LOCAL_GET(argLocal))

        // First try the case where the value is one of our objects
        val argsLocals = instrs.block(watpe.RefType.any) { labelNotOurObject =>
          // Load receiver and arguments and store them in temporary variables
          genReceiverNotNull()
          val argsLocals = if (t.args.isEmpty) {
            /* When there are no arguments, we can leave the receiver directly on
             * the stack instead of going through a local. We will still need a
             * local for the table-based dispatch, though.
             */
            Nil
          } else {
            val receiverLocal = addSyntheticLocal(watpe.RefType.any)

            instrs += wa.LOCAL_SET(receiverLocal)
            val argsLocals: List[wanme.LocalName] =
              for ((arg, typeRef) <- t.args.zip(t.method.name.paramTypeRefs)) yield {
                val typ = ctx.inferTypeFromTypeRef(typeRef)
                genTree(arg, typ)
                val localName = addSyntheticLocal(TypeTransformer.transformType(typ)(ctx))
                instrs += wa.LOCAL_SET(localName)
                localName
              }
            instrs += wa.LOCAL_GET(receiverLocal)
            argsLocals
          }

          instrs += wa.BR_ON_CAST_FAIL(labelNotOurObject, watpe.RefType.any, refTypeForDispatch)
          instrs += wa.LOCAL_TEE(receiverLocalForDispatch)
          pushArgs(argsLocals)
          genTableDispatch(receiverClassInfo, t.method.name, receiverLocalForDispatch)
          instrs += wa.BR(labelDone)

          argsLocals
        } // end block labelNotOurObject

        /* Now we have a value that is not one of our objects, so it must be
         * a JavaScript value whose representative class extends/implements the
         * receiver class. It may be a primitive instance of a hijacked class, or
         * any other value (whose representative class is therefore `jl.Object`).
         *
         * It is also *not* `char` or `long`, since those would reach
         * `genApplyNonPrim` in their boxed form, and therefore they are
         * "ourObject".
         *
         * The (ref any) is still on the stack.
         */

        if (t.method.name == toStringMethodName) {
          // By spec, toString() is special
          assert(argsLocals.isEmpty)
          instrs += wa.CALL(genFunctionName.jsValueToString)
        } else if (receiverClassName == JLNumberClass) {
          // the value must be a `number`, hence we can unbox to `double`
          genUnbox(DoubleType)
          pushArgs(argsLocals)
          genHijackedClassCall(BoxedDoubleClass)
        } else if (receiverClassName == CharSequenceClass) {
          // the value must be a `string`; it already has the right type
          pushArgs(argsLocals)
          genHijackedClassCall(BoxedStringClass)
        } else if (t.method.name == compareToMethodName) {
          /* The only method of jl.Comparable. Here the value can be a boolean,
           * a number or a string. We use `jsValueType` to dispatch to Wasm-side
           * implementations because they have to perform casts on their arguments.
           */
          assert(argsLocals.size == 1)

          val receiverLocal = addSyntheticLocal(watpe.RefType.any)
          instrs += wa.LOCAL_TEE(receiverLocal)

          val jsValueTypeLocal = addSyntheticLocal(watpe.Int32)
          instrs += wa.CALL(genFunctionName.jsValueType)
          instrs += wa.LOCAL_TEE(jsValueTypeLocal)

          instrs.switch(Sig(List(watpe.Int32), Nil), Sig(Nil, List(watpe.Int32))) { () =>
            // scrutinee is already on the stack
          }(
            // case JSValueTypeFalse | JSValueTypeTrue =>
            List(JSValueTypeFalse, JSValueTypeTrue) -> { () =>
              // the jsValueTypeLocal is the boolean value, thanks to the chosen encoding
              instrs += wa.LOCAL_GET(jsValueTypeLocal)
              pushArgs(argsLocals)
              genHijackedClassCall(BoxedBooleanClass)
            },
            // case JSValueTypeString =>
            List(JSValueTypeString) -> { () =>
              instrs += wa.LOCAL_GET(receiverLocal)
              // no need to unbox for string
              pushArgs(argsLocals)
              genHijackedClassCall(BoxedStringClass)
            }
          ) { () =>
            // case _ (JSValueTypeNumber) =>
            instrs += wa.LOCAL_GET(receiverLocal)
            genUnbox(DoubleType)
            pushArgs(argsLocals)
            genHijackedClassCall(BoxedDoubleClass)
          }
        } else {
          /* It must be a method of j.l.Object and it can be any value.
           * hashCode() and equals() are overridden in all hijacked classes.
           * We use `identityHashCode` for `hashCode` and `Object.is` for `equals`,
           * as they coincide with the respective specifications (on purpose).
           * The other methods are never overridden and can be statically
           * resolved to j.l.Object.
           */
          pushArgs(argsLocals)
          t.method.name match {
            case SpecialNames.hashCodeMethodName =>
              instrs += wa.CALL(genFunctionName.identityHashCode)
            case `equalsMethodName` =>
              instrs += wa.CALL(genFunctionName.is)
            case _ =>
              genHijackedClassCall(ObjectClass)
          }
        }
      } // end block labelDone
    }

    if (t.tpe == NothingType)
      instrs += wa.UNREACHABLE

    t.tpe
  }

  /** Generates a vtable- or itable-based dispatch.
    *
    * Before this code gen, the stack must contain the receiver and the args of the target method.
    * In addition, the receiver must be available in the local `receiverLocalForDispatch`. The two
    * occurrences of the receiver must have the type for dispatch.
    *
    * After this code gen, the stack contains the result. If the result type is `NothingType`,
    * `genTableDispatch` leaves the stack in an arbitrary state. It is up to the caller to insert an
    * `unreachable` instruction when appropriate.
    */
  def genTableDispatch(
      receiverClassInfo: WasmContext.ClassInfo,
      methodName: MethodName,
      receiverLocalForDispatch: wanme.LocalName
  ): Unit = {
    // Generates an itable-based dispatch.
    def genITableDispatch(): Unit = {
      val itableIdx = ctx.getItableIdx(receiverClassInfo)
      val methodIdx = receiverClassInfo.tableMethodInfos(methodName).tableIndex

      instrs += wa.LOCAL_GET(receiverLocalForDispatch)
      instrs += wa.STRUCT_GET(
        // receiver type should be upcasted into `Object` if it's interface
        // by TypeTransformer#transformType
        genTypeName.forClass(ObjectClass),
        genFieldIdx.objStruct.itables
      )
      instrs += wa.I32_CONST(itableIdx)
      instrs += wa.ARRAY_GET(genTypeName.itables)
      instrs += wa.REF_CAST(watpe.RefType(genTypeName.forITable(receiverClassInfo.name)))
      instrs += wa.STRUCT_GET(
        genTypeName.forITable(receiverClassInfo.name),
        wanme.FieldIdx(methodIdx)
      )
      instrs += wa.CALL_REF(ctx.tableFunctionType(methodName))
    }

    // Generates a vtable-based dispatch.
    def genVTableDispatch(): Unit = {
      val receiverClassName = receiverClassInfo.name
      val methodIdx = receiverClassInfo.tableMethodInfos(methodName).tableIndex

      // // push args to the stacks
      // local.get $this ;; for accessing funcref
      // local.get $this ;; for accessing vtable
      // struct.get $classType 0 ;; get vtable
      // struct.get $vtableType $methodIdx ;; get funcref
      // call.ref (type $funcType) ;; call funcref
      instrs += wa.LOCAL_GET(receiverLocalForDispatch)
      instrs += wa.REF_CAST(watpe.RefType(genTypeName.forClass(receiverClassName)))
      instrs += wa.STRUCT_GET(
        genTypeName.forClass(receiverClassName),
        genFieldIdx.objStruct.vtable
      )
      instrs += wa.STRUCT_GET(
        genTypeName.forVTable(receiverClassName),
        genFieldIdx.typeData.vtableMethodIdx(methodIdx)
      )
      instrs += wa.CALL_REF(ctx.tableFunctionType(methodName))
    }

    if (receiverClassInfo.isInterface)
      genITableDispatch()
    else
      genVTableDispatch()
  }

  private def genApplyStatically(t: ApplyStatically): Type = {
    t.receiver.tpe match {
      case NothingType =>
        genTree(t.receiver, NothingType)
        // nothing else to do; this is unreachable
        NothingType

      case NullType =>
        genTree(t.receiver, NullType)
        instrs += wa.UNREACHABLE // trap
        NothingType

      case _ =>
        val namespace = MemberNamespace.forNonStaticCall(t.flags)
        val targetClassName = {
          val classInfo = ctx.getClassInfo(t.className)
          if (!classInfo.isInterface && namespace == MemberNamespace.Public)
            classInfo.resolvedMethodInfos(t.method.name).ownerClass
          else
            t.className
        }

        BoxedClassToPrimType.get(targetClassName) match {
          case None =>
            genTree(t.receiver, ClassType(targetClassName))
            instrs += wa.REF_AS_NOT_NULL

          case Some(primReceiverType) =>
            if (t.receiver.tpe == primReceiverType) {
              genTreeAuto(t.receiver)
            } else {
              genTree(t.receiver, AnyType)
              instrs += wa.REF_AS_NOT_NULL
              genUnbox(primReceiverType)(t.pos)
            }
        }

        genArgs(t.args, t.method.name)

        val funcName = genFunctionName.forMethod(namespace, targetClassName, t.method.name)
        instrs += wa.CALL(funcName)
        if (t.tpe == NothingType)
          instrs += wa.UNREACHABLE
        t.tpe
    }
  }

  private def genApplyStatic(tree: ApplyStatic): Type = {
    genArgs(tree.args, tree.method.name)
    val namespace = MemberNamespace.forStaticCall(tree.flags)
    val funcName = genFunctionName.forMethod(namespace, tree.className, tree.method.name)
    instrs += wa.CALL(funcName)
    if (tree.tpe == NothingType)
      instrs += wa.UNREACHABLE
    tree.tpe
  }

  private def genApplyDynamicImport(tree: ApplyDynamicImport): Type = {
    // As long as we do not support multiple modules, this cannot happen
    throw new AssertionError(
      s"Unexpected $tree at ${tree.pos}; multiple modules are not supported yet"
    )
  }

  private def genArgs(args: List[Tree], methodName: MethodName): Unit = {
    for ((arg, paramTypeRef) <- args.zip(methodName.paramTypeRefs)) {
      val paramType = ctx.inferTypeFromTypeRef(paramTypeRef)
      genTree(arg, paramType)
    }
  }

  private def genLiteral(l: Literal, expectedType: Type): Type = {
    if (expectedType == NoType) {
      /* Since all primitives are pure, we can always get rid of them.
       * This is mostly useful for the argument of `Return` nodes that target a
       * `Labeled` in statement position, since they must have a non-`void`
       * type in the IR but they get a `void` expected type.
       */
      expectedType
    } else {
      markPosition(l)

      l match {
        case BooleanLiteral(v) => instrs += wa.I32_CONST(if (v) 1 else 0)
        case ByteLiteral(v)    => instrs += wa.I32_CONST(v)
        case ShortLiteral(v)   => instrs += wa.I32_CONST(v)
        case IntLiteral(v)     => instrs += wa.I32_CONST(v)
        case CharLiteral(v)    => instrs += wa.I32_CONST(v)
        case LongLiteral(v)    => instrs += wa.I64_CONST(v)
        case FloatLiteral(v)   => instrs += wa.F32_CONST(v)
        case DoubleLiteral(v)  => instrs += wa.F64_CONST(v)

        case v: Undefined =>
          instrs += wa.GLOBAL_GET(genGlobalName.undef)
        case v: Null =>
          instrs += wa.REF_NULL(watpe.HeapType.None)

        case v: StringLiteral =>
          instrs ++= ctx.getConstantStringInstr(v.value)

        case v: ClassOf =>
          v.typeRef match {
            case typeRef: NonArrayTypeRef =>
              genClassOfFromTypeData(getNonArrayTypeDataInstr(typeRef))

            case typeRef: ArrayTypeRef =>
              val typeDataType = watpe.RefType(genTypeName.typeData)
              val typeDataLocal = addSyntheticLocal(typeDataType)

              genLoadArrayTypeData(typeRef)
              instrs += wa.LOCAL_SET(typeDataLocal)
              genClassOfFromTypeData(wa.LOCAL_GET(typeDataLocal))
          }
      }

      l.tpe
    }
  }

  private def getNonArrayTypeDataInstr(typeRef: NonArrayTypeRef): wa.Instr =
    wa.GLOBAL_GET(genGlobalName.forVTable(typeRef))

  private def genLoadArrayTypeData(arrayTypeRef: ArrayTypeRef): Unit = {
    instrs += getNonArrayTypeDataInstr(arrayTypeRef.base)
    instrs += wa.I32_CONST(arrayTypeRef.dimensions)
    instrs += wa.CALL(genFunctionName.arrayTypeData)
  }

  private def genClassOfFromTypeData(loadTypeDataInstr: wa.Instr): Unit = {
    instrs.block(watpe.RefType(genTypeName.ClassStruct)) { nonNullLabel =>
      // fast path first
      instrs += loadTypeDataInstr
      instrs += wa.STRUCT_GET(genTypeName.typeData, genFieldIdx.typeData.classOfIdx)
      instrs += wa.BR_ON_NON_NULL(nonNullLabel)
      // slow path
      instrs += loadTypeDataInstr
      instrs += wa.CALL(genFunctionName.createClassOf)
    }
  }

  private def genSelect(sel: Select): Type = {
    val className = sel.field.name.className
    val classInfo = ctx.getClassInfo(className)

    // For Select, the receiver can never be a hijacked class, so we can use genTreeAuto
    genTreeAuto(sel.qualifier)

    markPosition(sel)

    if (!classInfo.hasInstances) {
      /* The field may not exist in that case, and we cannot look it up.
       * However we necessarily have a `null` receiver if we reach this point,
       * so we can trap as NPE.
       */
      instrs += wa.UNREACHABLE
    } else {
      val fieldName = genFieldName.forClassInstanceField(sel.field.name)
      val idx = classInfo.getFieldIdx(sel.field.name)

      instrs += wa.STRUCT_GET(genTypeName.forClass(className), idx)
    }

    sel.tpe
  }

  private def genSelectStatic(tree: SelectStatic): Type = {
    markPosition(tree)
    instrs += wa.GLOBAL_GET(genGlobalName.forStaticField(tree.field.name))
    tree.tpe
  }

  private def genStoreModule(t: StoreModule): Type = {
    val className = enclosingClassName.getOrElse {
      throw new AssertionError(s"Cannot emit $t at ${t.pos} without enclosing class name")
    }

    genTreeAuto(This()(ClassType(className))(t.pos))

    markPosition(t)
    instrs += wa.GLOBAL_SET(genGlobalName.forModuleInstance(className))
    NoType
  }

  /** Push module class instance to the stack.
    *
    * see: Builder.genLoadModuleFunc
    */
  private def genLoadModule(t: LoadModule): Type = {
    markPosition(t)
    instrs += wa.CALL(genFunctionName.loadModule(t.className))
    t.tpe
  }

  private def genUnaryOp(unary: UnaryOp): Type = {
    import UnaryOp._

    genTreeAuto(unary.lhs)

    markPosition(unary)

    (unary.op: @switch) match {
      case Boolean_! =>
        instrs += wa.I32_CONST(1)
        instrs += wa.I32_XOR

      // Widening conversions
      case CharToInt | ByteToInt | ShortToInt =>
        () // these are no-ops because they are all represented as i32's with the right mathematical value
      case IntToLong =>
        instrs += wa.I64_EXTEND_I32_S
      case IntToDouble =>
        instrs += wa.F64_CONVERT_I32_S
      case FloatToDouble =>
        instrs += wa.F64_PROMOTE_F32

      // Narrowing conversions
      case IntToChar =>
        instrs += wa.I32_CONST(0xFFFF)
        instrs += wa.I32_AND
      case IntToByte =>
        instrs += wa.I32_EXTEND8_S
      case IntToShort =>
        instrs += wa.I32_EXTEND16_S
      case LongToInt =>
        instrs += wa.I32_WRAP_I64
      case DoubleToInt =>
        instrs += wa.I32_TRUNC_SAT_F64_S
      case DoubleToFloat =>
        instrs += wa.F32_DEMOTE_F64

      // Long <-> Double (neither widening nor narrowing)
      case LongToDouble =>
        instrs += wa.F64_CONVERT_I64_S
      case DoubleToLong =>
        instrs += wa.I64_TRUNC_SAT_F64_S

      // Long -> Float (neither widening nor narrowing), introduced in 1.6
      case LongToFloat =>
        instrs += wa.F32_CONVERT_I64_S

      // String.length, introduced in 1.11
      case String_length =>
        instrs += wa.CALL(genFunctionName.stringLength)
    }

    unary.tpe
  }

  private def genBinaryOp(binary: BinaryOp): Type = {
    def genLongShiftOp(shiftInstr: wa.Instr): Type = {
      genTree(binary.lhs, LongType)
      genTree(binary.rhs, IntType)
      markPosition(binary)
      instrs += wa.I64_EXTEND_I32_S
      instrs += shiftInstr
      LongType
    }

    def genThrowArithmeticException(): Unit = {
      implicit val pos = binary.pos
      val divisionByZeroEx = Throw(
        New(
          ArithmeticExceptionClass,
          MethodIdent(
            MethodName.constructor(List(ClassRef(BoxedStringClass)))
          ),
          List(StringLiteral("/ by zero "))
        )
      )
      genThrow(divisionByZeroEx)
    }

    def genDivModByConstant[T](
        isDiv: Boolean,
        rhsValue: T,
        const: T => wa.Instr,
        sub: wa.Instr,
        mainOp: wa.Instr
    )(implicit num: Numeric[T]): Type = {
      /* When we statically know the value of the rhs, we can avoid the
       * dynamic tests for division by zero and overflow. This is quite
       * common in practice.
       */

      val tpe = binary.tpe

      if (rhsValue == num.zero) {
        genTree(binary.lhs, tpe)
        markPosition(binary)
        genThrowArithmeticException()
        NothingType
      } else if (isDiv && rhsValue == num.fromInt(-1)) {
        /* MinValue / -1 overflows; it traps in Wasm but we need to wrap.
         * We rewrite as `0 - lhs` so that we do not need any test.
         */
        markPosition(binary)
        instrs += const(num.zero)
        genTree(binary.lhs, tpe)
        markPosition(binary)
        instrs += sub
        tpe
      } else {
        genTree(binary.lhs, tpe)
        markPosition(binary.rhs)
        instrs += const(rhsValue)
        markPosition(binary)
        instrs += mainOp
        tpe
      }
    }

    def genDivMod[T](
        isDiv: Boolean,
        const: T => wa.Instr,
        eqz: wa.Instr,
        eq: wa.Instr,
        sub: wa.Instr,
        mainOp: wa.Instr
    )(implicit num: Numeric[T]): Type = {
      /* Here we perform the same steps as in the static case, but using
       * value tests at run-time.
       */

      val tpe = binary.tpe
      val wasmTyp = TypeTransformer.transformType(tpe)(ctx)

      val lhsLocal = addSyntheticLocal(wasmTyp)
      val rhsLocal = addSyntheticLocal(wasmTyp)
      genTree(binary.lhs, tpe)
      instrs += wa.LOCAL_SET(lhsLocal)
      genTree(binary.rhs, tpe)
      instrs += wa.LOCAL_TEE(rhsLocal)

      markPosition(binary)

      instrs += eqz
      instrs.ifThen() {
        genThrowArithmeticException()
      }
      if (isDiv) {
        // Handle the MinValue / -1 corner case
        instrs += wa.LOCAL_GET(rhsLocal)
        instrs += const(num.fromInt(-1))
        instrs += eq
        instrs.ifThenElse(wasmTyp) {
          // 0 - lhs
          instrs += const(num.zero)
          instrs += wa.LOCAL_GET(lhsLocal)
          instrs += sub
        } {
          // lhs / rhs
          instrs += wa.LOCAL_GET(lhsLocal)
          instrs += wa.LOCAL_GET(rhsLocal)
          instrs += mainOp
        }
      } else {
        // lhs % rhs
        instrs += wa.LOCAL_GET(lhsLocal)
        instrs += wa.LOCAL_GET(rhsLocal)
        instrs += mainOp
      }

      tpe
    }

    binary.op match {
      case BinaryOp.=== | BinaryOp.!== => genEq(binary)

      case BinaryOp.String_+ => genStringConcat(binary)

      case BinaryOp.Int_/ =>
        binary.rhs match {
          case IntLiteral(rhsValue) =>
            genDivModByConstant(isDiv = true, rhsValue, wa.I32_CONST(_), wa.I32_SUB, wa.I32_DIV_S)
          case _ =>
            genDivMod(
              isDiv = true,
              wa.I32_CONST(_),
              wa.I32_EQZ,
              wa.I32_EQ,
              wa.I32_SUB,
              wa.I32_DIV_S
            )
        }
      case BinaryOp.Int_% =>
        binary.rhs match {
          case IntLiteral(rhsValue) =>
            genDivModByConstant(isDiv = false, rhsValue, wa.I32_CONST(_), wa.I32_SUB, wa.I32_REM_S)
          case _ =>
            genDivMod(
              isDiv = false,
              wa.I32_CONST(_),
              wa.I32_EQZ,
              wa.I32_EQ,
              wa.I32_SUB,
              wa.I32_REM_S
            )
        }
      case BinaryOp.Long_/ =>
        binary.rhs match {
          case LongLiteral(rhsValue) =>
            genDivModByConstant(isDiv = true, rhsValue, wa.I64_CONST(_), wa.I64_SUB, wa.I64_DIV_S)
          case _ =>
            genDivMod(
              isDiv = true,
              wa.I64_CONST(_),
              wa.I64_EQZ,
              wa.I64_EQ,
              wa.I64_SUB,
              wa.I64_DIV_S
            )
        }
      case BinaryOp.Long_% =>
        binary.rhs match {
          case LongLiteral(rhsValue) =>
            genDivModByConstant(isDiv = false, rhsValue, wa.I64_CONST(_), wa.I64_SUB, wa.I64_REM_S)
          case _ =>
            genDivMod(
              isDiv = false,
              wa.I64_CONST(_),
              wa.I64_EQZ,
              wa.I64_EQ,
              wa.I64_SUB,
              wa.I64_REM_S
            )
        }

      case BinaryOp.Long_<<  => genLongShiftOp(wa.I64_SHL)
      case BinaryOp.Long_>>> => genLongShiftOp(wa.I64_SHR_U)
      case BinaryOp.Long_>>  => genLongShiftOp(wa.I64_SHR_S)

      /* Floating point remainders are specified by
       * https://262.ecma-international.org/#sec-numeric-types-number-remainder
       * which says that it is equivalent to the C library function `fmod`.
       * For `Float`s, we promote and demote to `Double`s.
       * `fmod` seems quite hard to correctly implement, so we delegate to a
       * JavaScript Helper.
       * (The naive function `x - trunc(x / y) * y` that we can find on the
       * Web does not work.)
       */
      case BinaryOp.Float_% =>
        genTree(binary.lhs, FloatType)
        instrs += wa.F64_PROMOTE_F32
        genTree(binary.rhs, FloatType)
        instrs += wa.F64_PROMOTE_F32
        markPosition(binary)
        instrs += wa.CALL(genFunctionName.fmod)
        instrs += wa.F32_DEMOTE_F64
        FloatType
      case BinaryOp.Double_% =>
        genTree(binary.lhs, DoubleType)
        genTree(binary.rhs, DoubleType)
        markPosition(binary)
        instrs += wa.CALL(genFunctionName.fmod)
        DoubleType

      // New in 1.11
      case BinaryOp.String_charAt =>
        genTree(binary.lhs, StringType) // push the string
        genTree(binary.rhs, IntType) // push the index
        markPosition(binary)
        instrs += wa.CALL(genFunctionName.stringCharAt)
        CharType

      case _ => genElementaryBinaryOp(binary)
    }
  }

  private def genEq(binary: BinaryOp): Type = {
    // TODO Optimize this when the operands have a better type than `any`
    genTree(binary.lhs, AnyType)
    genTree(binary.rhs, AnyType)

    markPosition(binary)

    instrs += wa.CALL(genFunctionName.is)

    if (binary.op == BinaryOp.!==) {
      instrs += wa.I32_CONST(1)
      instrs += wa.I32_XOR
    }

    BooleanType
  }

  private def genElementaryBinaryOp(binary: BinaryOp): Type = {
    genTreeAuto(binary.lhs)
    genTreeAuto(binary.rhs)

    markPosition(binary)

    val operation = binary.op match {
      case BinaryOp.Boolean_== => wa.I32_EQ
      case BinaryOp.Boolean_!= => wa.I32_NE
      case BinaryOp.Boolean_|  => wa.I32_OR
      case BinaryOp.Boolean_&  => wa.I32_AND

      case BinaryOp.Int_+   => wa.I32_ADD
      case BinaryOp.Int_-   => wa.I32_SUB
      case BinaryOp.Int_*   => wa.I32_MUL
      case BinaryOp.Int_/   => wa.I32_DIV_S // signed division
      case BinaryOp.Int_%   => wa.I32_REM_S // signed remainder
      case BinaryOp.Int_|   => wa.I32_OR
      case BinaryOp.Int_&   => wa.I32_AND
      case BinaryOp.Int_^   => wa.I32_XOR
      case BinaryOp.Int_<<  => wa.I32_SHL
      case BinaryOp.Int_>>> => wa.I32_SHR_U
      case BinaryOp.Int_>>  => wa.I32_SHR_S
      case BinaryOp.Int_==  => wa.I32_EQ
      case BinaryOp.Int_!=  => wa.I32_NE
      case BinaryOp.Int_<   => wa.I32_LT_S
      case BinaryOp.Int_<=  => wa.I32_LE_S
      case BinaryOp.Int_>   => wa.I32_GT_S
      case BinaryOp.Int_>=  => wa.I32_GE_S

      case BinaryOp.Long_+ => wa.I64_ADD
      case BinaryOp.Long_- => wa.I64_SUB
      case BinaryOp.Long_* => wa.I64_MUL
      case BinaryOp.Long_/ => wa.I64_DIV_S
      case BinaryOp.Long_% => wa.I64_REM_S
      case BinaryOp.Long_| => wa.I64_OR
      case BinaryOp.Long_& => wa.I64_AND
      case BinaryOp.Long_^ => wa.I64_XOR

      case BinaryOp.Long_== => wa.I64_EQ
      case BinaryOp.Long_!= => wa.I64_NE
      case BinaryOp.Long_<  => wa.I64_LT_S
      case BinaryOp.Long_<= => wa.I64_LE_S
      case BinaryOp.Long_>  => wa.I64_GT_S
      case BinaryOp.Long_>= => wa.I64_GE_S

      case BinaryOp.Float_+ => wa.F32_ADD
      case BinaryOp.Float_- => wa.F32_SUB
      case BinaryOp.Float_* => wa.F32_MUL
      case BinaryOp.Float_/ => wa.F32_DIV

      case BinaryOp.Double_+ => wa.F64_ADD
      case BinaryOp.Double_- => wa.F64_SUB
      case BinaryOp.Double_* => wa.F64_MUL
      case BinaryOp.Double_/ => wa.F64_DIV

      case BinaryOp.Double_== => wa.F64_EQ
      case BinaryOp.Double_!= => wa.F64_NE
      case BinaryOp.Double_<  => wa.F64_LT
      case BinaryOp.Double_<= => wa.F64_LE
      case BinaryOp.Double_>  => wa.F64_GT
      case BinaryOp.Double_>= => wa.F64_GE
    }
    instrs += operation
    binary.tpe
  }

  private def genStringConcat(binary: BinaryOp): Type = {
    val wasmStringType = watpe.RefType.any

    def genToString(tree: Tree): Unit = {
      def genWithDispatch(isAncestorOfHijackedClass: Boolean): Unit = {
        /* Somewhat duplicated from genApplyNonPrim, but specialized for
         * `toString`, and where the handling of `null` is different.
         *
         * We need to return the `"null"` string in two special cases:
         * - if the value itself is `null`, or
         * - if the value's `toString(): String` method returns `null`!
         */

        // A local for a copy of the receiver that we will use to resolve dispatch
        val receiverLocalForDispatch =
          addSyntheticLocal(watpe.RefType(genTypeName.ObjectStruct))

        val objectClassInfo = ctx.getClassInfo(ObjectClass)

        if (!isAncestorOfHijackedClass) {
          /* Standard dispatch codegen, with dedicated null handling.
           *
           * The overall structure of the generated code is as follows:
           *
           * block (ref any) $done
           *   block $isNull
           *     load receiver as (ref null java.lang.Object)
           *     br_on_null $isNull
           *     generate standard table-based dispatch
           *     br_on_non_null $done
           *   end $isNull
           *   gen "null"
           * end $done
           */

          instrs.block(watpe.RefType.any) { labelDone =>
            instrs.block() { labelIsNull =>
              genTreeAuto(tree)
              markPosition(binary)
              instrs += wa.BR_ON_NULL(labelIsNull)
              instrs += wa.LOCAL_TEE(receiverLocalForDispatch)
              genTableDispatch(objectClassInfo, toStringMethodName, receiverLocalForDispatch)
              instrs += wa.BR_ON_NON_NULL(labelDone)
            }

            instrs ++= ctx.getConstantStringInstr("null")
          }
        } else {
          /* Dispatch where the receiver can be a JS value.
           *
           * The overall structure of the generated code is as follows:
           *
           * block (ref any) $done
           *   block anyref $notOurObject
           *     load receiver
           *     br_on_cast_fail anyref (ref $java.lang.Object) $notOurObject
           *     generate standard table-based dispatch
           *     br_on_non_null $done
           *     ref.null any
           *   end $notOurObject
           *   call the JS helper, also handles `null`
           * end $done
           */

          instrs.block(watpe.RefType.any) { labelDone =>
            // First try the case where the value is one of our objects
            instrs.block(watpe.RefType.anyref) { labelNotOurObject =>
              // Load receiver
              genTreeAuto(tree)

              markPosition(binary)

              instrs += wa.BR_ON_CAST_FAIL(
                labelNotOurObject,
                watpe.RefType.anyref,
                watpe.RefType(genTypeName.ObjectStruct)
              )
              instrs += wa.LOCAL_TEE(receiverLocalForDispatch)
              genTableDispatch(objectClassInfo, toStringMethodName, receiverLocalForDispatch)
              instrs += wa.BR_ON_NON_NULL(labelDone)
              instrs += wa.REF_NULL(watpe.HeapType.Any)
            } // end block labelNotOurObject

            // Now we have a value that is not one of our objects; the anyref is still on the stack
            instrs += wa.CALL(genFunctionName.jsValueToStringForConcat)
          } // end block labelDone
        }
      }

      tree.tpe match {
        case primType: PrimType =>
          genTreeAuto(tree)

          markPosition(binary)

          primType match {
            case StringType =>
              () // no-op
            case BooleanType =>
              instrs += wa.CALL(genFunctionName.booleanToString)
            case CharType =>
              instrs += wa.CALL(genFunctionName.charToString)
            case ByteType | ShortType | IntType =>
              instrs += wa.CALL(genFunctionName.intToString)
            case LongType =>
              instrs += wa.CALL(genFunctionName.longToString)
            case FloatType =>
              instrs += wa.F64_PROMOTE_F32
              instrs += wa.CALL(genFunctionName.doubleToString)
            case DoubleType =>
              instrs += wa.CALL(genFunctionName.doubleToString)
            case NullType | UndefType =>
              instrs += wa.CALL(genFunctionName.jsValueToStringForConcat)
            case NothingType =>
              () // unreachable
            case NoType =>
              throw new AssertionError(
                s"Found expression of type void in String_+ at ${tree.pos}: $tree"
              )
          }

        case ClassType(BoxedStringClass) =>
          // Common case for which we want to avoid the hijacked class dispatch
          genTreeAuto(tree)
          markPosition(binary)
          instrs += wa.CALL(genFunctionName.jsValueToStringForConcat) // for `null`

        case ClassType(className) =>
          genWithDispatch(ctx.getClassInfo(className).isAncestorOfHijackedClass)

        case AnyType =>
          genWithDispatch(isAncestorOfHijackedClass = true)

        case ArrayType(_) =>
          genWithDispatch(isAncestorOfHijackedClass = false)

        case tpe: RecordType =>
          throw new AssertionError(s"Invalid type $tpe for String_+ at ${tree.pos}: $tree")
      }
    }

    binary.lhs match {
      case StringLiteral("") =>
        // Common case where we don't actually need a concatenation
        genToString(binary.rhs)

      case _ =>
        genToString(binary.lhs)
        genToString(binary.rhs)
        markPosition(binary)
        instrs += wa.CALL(genFunctionName.stringConcat)
    }

    StringType
  }

  private def genIsInstanceOf(tree: IsInstanceOf): Type = {
    genTree(tree.expr, AnyType)

    markPosition(tree)

    def genIsPrimType(testType: PrimType): Unit = {
      testType match {
        case UndefType =>
          instrs += wa.CALL(genFunctionName.isUndef)
        case StringType =>
          instrs += wa.CALL(genFunctionName.isString)

        case testType: PrimTypeWithRef =>
          testType match {
            case CharType =>
              val structTypeName = genTypeName.forClass(SpecialNames.CharBoxClass)
              instrs += wa.REF_TEST(watpe.RefType(structTypeName))
            case LongType =>
              val structTypeName = genTypeName.forClass(SpecialNames.LongBoxClass)
              instrs += wa.REF_TEST(watpe.RefType(structTypeName))
            case NoType | NothingType | NullType =>
              throw new AssertionError(s"Illegal isInstanceOf[$testType]")
            case _ =>
              /* Calls the appropriate `tX` JS helper. It dynamically tests whether
               * the value fits in the given primitive type, according to
               * https://www.scala-js.org/doc/semantics.html
               * All the `tX` helpers have Wasm type `anyref -> i32` (interpreted as `boolean`).
               */
              instrs += wa.CALL(genFunctionName.typeTest(testType.primRef))
          }
      }
    }

    tree.testType match {
      case testType: PrimType =>
        genIsPrimType(testType)

      case AnyType | ClassType(ObjectClass) =>
        instrs += wa.REF_IS_NULL
        instrs += wa.I32_CONST(1)
        instrs += wa.I32_XOR

      case ClassType(JLNumberClass) =>
        /* Special case: the only non-Object *class* that is an ancestor of a
         * hijacked class. We need to accept `number` primitives here.
         */
        val tempLocal = addSyntheticLocal(watpe.RefType.anyref)
        instrs += wa.LOCAL_TEE(tempLocal)
        instrs += wa.REF_TEST(watpe.RefType(genTypeName.forClass(JLNumberClass)))
        instrs.ifThenElse(watpe.Int32) {
          instrs += wa.I32_CONST(1)
        } {
          instrs += wa.LOCAL_GET(tempLocal)
          instrs += wa.CALL(genFunctionName.typeTest(DoubleRef))
        }

      case ClassType(testClassName) =>
        BoxedClassToPrimType.get(testClassName) match {
          case Some(primType) =>
            genIsPrimType(primType)
          case None =>
            val info = ctx.getClassInfo(testClassName)

            if (info.isInterface)
              instrs += wa.CALL(genFunctionName.instanceTest(testClassName))
            else
              instrs += wa.REF_TEST(watpe.RefType(genTypeName.forClass(testClassName)))
        }

      case ArrayType(arrayTypeRef) =>
        arrayTypeRef match {
          case ArrayTypeRef(
                ClassRef(ObjectClass) | _: PrimRef,
                1
              ) =>
            // For primitive arrays and exactly Array[Object], a wa.REF_TEST is enough
            val structTypeName = genTypeName.forArrayClass(arrayTypeRef)
            instrs += wa.REF_TEST(watpe.RefType(structTypeName))

          case _ =>
            /* Non-Object reference arra types need a sophisticated type test
             * based on assignability of component types.
             */
            import watpe.RefType.anyref

            instrs.block(Sig(List(anyref), List(watpe.Int32))) { doneLabel =>
              instrs.block(Sig(List(anyref), List(anyref))) { notARefArrayLabel =>
                // Try and cast to the generic representation first
                val refArrayStructTypeName = genTypeName.forArrayClass(arrayTypeRef)
                instrs += wa.BR_ON_CAST_FAIL(
                  notARefArrayLabel,
                  watpe.RefType.anyref,
                  watpe.RefType(refArrayStructTypeName)
                )

                // refArrayValue := the generic representation
                val refArrayValueLocal =
                  addSyntheticLocal(watpe.RefType(refArrayStructTypeName))
                instrs += wa.LOCAL_SET(refArrayValueLocal)

                // Load typeDataOf(arrayTypeRef)
                genLoadArrayTypeData(arrayTypeRef)

                // Load refArrayValue.vtable
                instrs += wa.LOCAL_GET(refArrayValueLocal)
                instrs += wa.STRUCT_GET(refArrayStructTypeName, genFieldIdx.objStruct.vtable)

                // Call isAssignableFrom and return its result
                instrs += wa.CALL(genFunctionName.isAssignableFrom)
                instrs += wa.BR(doneLabel)
              }

              // Here, the value is not a reference array type, so return false
              instrs += wa.DROP
              instrs += wa.I32_CONST(0)
            }
        }

      case testType: RecordType =>
        throw new AssertionError(s"Illegal type in IsInstanceOf: $testType")
    }

    BooleanType
  }

  private def genAsInstanceOf(tree: AsInstanceOf): Type = {
    val sourceTpe = tree.expr.tpe
    val targetTpe = tree.tpe

    if (isSubtype(sourceTpe, targetTpe)(isSubclass(_, _))) {
      // Common case where no cast is necessary
      genTreeAuto(tree.expr)
      sourceTpe
    } else {
      genTree(tree.expr, AnyType)

      markPosition(tree)

      def genAsPrimType(targetTpe: PrimType): Unit = {
        // TODO We could do something better for things like double.asInstanceOf[int]
        genUnbox(targetTpe)(tree.pos)
      }

      targetTpe match {
        case targetTpe: PrimType =>
          genAsPrimType(targetTpe)

        case AnyType =>
          ()

        case ClassType(targetClassName) =>
          val info = ctx.getClassInfo(targetClassName)
          if (info.kind == ClassKind.HijackedClass) {
            BoxedClassToPrimType(targetClassName) match {
              case UndefType | StringType =>
                ()
              case primType: PrimTypeWithRef =>
                primType match {
                  case CharType =>
                    val structTypeName = genTypeName.forClass(SpecialNames.CharBoxClass)
                    instrs += wa.REF_CAST(watpe.RefType.nullable(structTypeName))
                  case LongType =>
                    val structTypeName = genTypeName.forClass(SpecialNames.LongBoxClass)
                    instrs += wa.REF_CAST(watpe.RefType.nullable(structTypeName))
                  case NoType | NothingType | NullType =>
                    throw new AssertionError(s"Unexpected prim type $primType for $targetClassName")
                  case _ =>
                    instrs += wa.CALL(genFunctionName.unboxOrNull(primType.primRef))
                }
            }
          } else if (info.isAncestorOfHijackedClass) {
            // Nothing to do; the translation is `anyref`
            ()
          } else if (info.kind.isClass) {
            instrs += wa.REF_CAST(
              watpe.RefType.nullable(genTypeName.forClass(targetClassName))
            )
          } else if (info.isInterface) {
            instrs += wa.REF_CAST(watpe.RefType.nullable(genTypeName.ObjectStruct))
          }

        case ArrayType(arrayTypeRef) =>
          val structTypeName = genTypeName.forArrayClass(arrayTypeRef)
          instrs += wa.REF_CAST(watpe.RefType.nullable(structTypeName))

        case targetTpe: RecordType =>
          throw new AssertionError(s"Illegal type in AsInstanceOf: $targetTpe")
      }

      targetTpe
    }
  }

  /** Unbox the `anyref` on the stack to the target `PrimType`.
    *
    * `targetTpe` must not be `NothingType`, `NullType` nor `NoType`.
    *
    * The type left on the stack is non-nullable.
    */
  private def genUnbox(targetTpe: PrimType)(implicit pos: Position): Unit = {
    targetTpe match {
      case UndefType =>
        instrs += wa.DROP
        instrs += wa.GLOBAL_GET(genGlobalName.undef)
      case StringType =>
        instrs += wa.REF_AS_NOT_NULL

      case targetTpe: PrimTypeWithRef =>
        targetTpe match {
          case CharType | LongType =>
            // Extract the `value` field (the only field) out of the box class.

            val boxClass =
              if (targetTpe == CharType) SpecialNames.CharBoxClass
              else SpecialNames.LongBoxClass
            val resultType = TypeTransformer.transformType(targetTpe)(ctx)

            instrs.block(Sig(List(watpe.RefType.anyref), List(resultType))) { doneLabel =>
              instrs.block(Sig(List(watpe.RefType.anyref), Nil)) { isNullLabel =>
                instrs += wa.BR_ON_NULL(isNullLabel)
                val structTypeName = genTypeName.forClass(boxClass)
                instrs += wa.REF_CAST(watpe.RefType(structTypeName))
                instrs += wa.STRUCT_GET(structTypeName, genFieldIdx.objStruct.uniqueRegularField)
                instrs += wa.BR(doneLabel)
              }
              genTree(zeroOf(targetTpe), targetTpe)
            }

          case NothingType | NullType | NoType =>
            throw new IllegalArgumentException(s"Illegal type in genUnbox: $targetTpe")
          case _ =>
            instrs += wa.CALL(genFunctionName.unbox(targetTpe.primRef))
        }
    }
  }

  private def isSubclass(subClass: ClassName, superClass: ClassName): Boolean =
    ctx.getClassInfo(subClass).ancestors.contains(superClass)

  private def genGetClass(tree: GetClass): Type = {
    /* Unlike in `genApply` or `genStringConcat`, here we make no effort to
     * optimize known-primitive receivers. In practice, such cases would be
     * useless.
     */

    val needHijackedClassDispatch = tree.expr.tpe match {
      case ClassType(className) =>
        ctx.getClassInfo(className).isAncestorOfHijackedClass
      case ArrayType(_) | NothingType | NullType =>
        false
      case _ =>
        true
    }

    if (!needHijackedClassDispatch) {
      val typeDataType = watpe.RefType(genTypeName.typeData)
      val objectTypeIdx = genTypeName.forClass(ObjectClass)

      val typeDataLocal = addSyntheticLocal(typeDataType)

      genTreeAuto(tree.expr)
      markPosition(tree)
      instrs += wa.STRUCT_GET(objectTypeIdx, genFieldIdx.objStruct.vtable) // implicit trap on null
      instrs += wa.LOCAL_SET(typeDataLocal)
      genClassOfFromTypeData(wa.LOCAL_GET(typeDataLocal))
    } else {
      genTree(tree.expr, AnyType)
      markPosition(tree)
      instrs += wa.REF_AS_NOT_NULL
      instrs += wa.CALL(genFunctionName.anyGetClass)
    }

    tree.tpe
  }

  private def genReadStorage(storage: VarStorage): Unit = {
    storage match {
      case VarStorage.Local(localIdx) =>
        instrs += wa.LOCAL_GET(localIdx)
      case VarStorage.StructField(structLocalIdx, structTypeName, fieldIdx) =>
        instrs += wa.LOCAL_GET(structLocalIdx)
        instrs += wa.STRUCT_GET(structTypeName, fieldIdx)
    }
  }

  private def genVarRef(r: VarRef): Type = {
    markPosition(r)
    genReadStorage(lookupLocal(r.ident.name))
    r.tpe
  }

  private def genThis(t: This): Type = {
    markPosition(t)

    genReadStorage(receiverStorage)

    // Workaround wrong t.tpe for This nodes inside reflective proxies.
    // In a hijacked class, This nodes are supposed to be typed as the corresponding primitive type.
    // However, the Scala.js linker frontend synthesizes reflective proxies that contain This nodes typed as the hijacked class' ClassType instead.
    // This is bad for us because it means genAdapt fails to box the primitives when required.
    // We work around this issue here by re-computing the correct type of This nodes.
    val fixedTpe = t.tpe match {
      case ClassType(cls) => BoxedClassToPrimType.getOrElse(cls, t.tpe)
      case _              => t.tpe
    }

    fixedTpe
  }

  private def genVarDef(r: VarDef): Type = {
    /* This is an isolated VarDef that is not in a Block.
     * Its scope is empty by construction, and therefore it need not be stored.
     */
    genTree(r.rhs, NoType)
    NoType
  }

  private def genIf(t: If, expectedType: Type): Type = {
    val ty = TypeTransformer.transformResultType(expectedType)(ctx)
    genTree(t.cond, BooleanType)

    markPosition(t)

    t.elsep match {
      case Skip() =>
        assert(expectedType == NoType)
        instrs.ifThen() {
          genTree(t.thenp, expectedType)
        }
      case _ =>
        instrs.ifThenElse(ty) {
          genTree(t.thenp, expectedType)
        } {
          genTree(t.elsep, expectedType)
        }
    }

    if (expectedType == NothingType)
      instrs += wa.UNREACHABLE

    expectedType
  }

  private def genWhile(t: While): Type = {
    t.cond match {
      case BooleanLiteral(true) =>
        // infinite loop that must be typed as `nothing`, i.e., unreachable
        // loop $label
        //   body
        //   br $label
        // end
        // unreachable
        markPosition(t)
        instrs.loop() { label =>
          genTree(t.body, NoType)
          markPosition(t)
          instrs += wa.BR(label)
        }
        instrs += wa.UNREACHABLE
        NothingType

      case _ =>
        // loop $label
        //   cond
        //   if
        //     body
        //     br $label
        //   end
        // end
        markPosition(t)
        instrs.loop() { label =>
          genTree(t.cond, BooleanType)
          markPosition(t)
          instrs.ifThen() {
            genTree(t.body, NoType)
            markPosition(t)
            instrs += wa.BR(label)
          }
        }
        NoType
    }
  }

  private def genForIn(t: ForIn): Type = {
    /* This is tricky. In general, the body of a ForIn can be an arbitrary
     * statement, which can refer to the enclosing scope and its locals,
     * including for mutations. Unfortunately, there is no way to implement a
     * ForIn other than actually doing a JS `for (var key in obj) { body }`
     * loop. That means we need to pass the `body` as a JS closure.
     *
     * That is problematic for our backend because we basically need to perform
     * lambda lifting: identifying captures ourselves, and turn references to
     * local variables into accessing the captured environment.
     *
     * We side-step this issue for now by exploiting the known shape of `ForIn`
     * generated by the Scala.js compiler. This is fine as long as we do not
     * support the Scala.js optimizer. We will have to revisit this code when
     * we add that support.
     */

    t.body match {
      case JSFunctionApply(fVarRef: VarRef, List(VarRef(argIdent)))
          if fVarRef.ident.name != t.keyVar.name && argIdent.name == t.keyVar.name =>
        genTree(t.obj, AnyType)
        genTree(fVarRef, AnyType)
        markPosition(t)
        instrs += wa.CALL(genFunctionName.jsForInSimple)

      case _ =>
        throw new NotImplementedError(s"Unsupported shape of ForIn node at ${t.pos}: $t")
    }

    NoType
  }

  private def genTryCatch(t: TryCatch, expectedType: Type): Type = {
    val resultType = TypeTransformer.transformResultType(expectedType)(ctx)

    if (UseLegacyExceptionsForTryCatch) {
      markPosition(t)
      instrs += wa.TRY(instrs.sigToBlockType(Sig(Nil, resultType)))
      genTree(t.block, expectedType)
      markPosition(t)
      instrs += wa.CATCH(genTagName.exceptionTagName)
      withNewLocal(t.errVar.name, watpe.RefType.anyref) { exceptionLocal =>
        instrs += wa.ANY_CONVERT_EXTERN
        instrs += wa.LOCAL_SET(exceptionLocal)
        genTree(t.handler, expectedType)
      }
      instrs += wa.END
    } else {
      markPosition(t)
      instrs.block(resultType) { doneLabel =>
        instrs.block(watpe.RefType.externref) { catchLabel =>
          /* We used to have `resultType` as result of the try_table, with the
           * `wa.BR(doneLabel)` outside of the try_table. Unfortunately it seems
           * V8 cannot handle try_table with a result type that is `(ref ...)`.
           * The current encoding with `externref` as result type (to match the
           * enclosing block) and the `br` *inside* the `try_table` works.
           */
          instrs.tryTable(watpe.RefType.externref)(
            List(wa.CatchClause.Catch(genTagName.exceptionTagName, catchLabel))
          ) {
            genTree(t.block, expectedType)
            markPosition(t)
            instrs += wa.BR(doneLabel)
          }
        } // end block $catch
        withNewLocal(t.errVar.name, watpe.RefType.anyref) { exceptionLocal =>
          instrs += wa.ANY_CONVERT_EXTERN
          instrs += wa.LOCAL_SET(exceptionLocal)
          genTree(t.handler, expectedType)
        }
      } // end block $done
    }

    if (expectedType == NothingType)
      instrs += wa.UNREACHABLE

    expectedType
  }

  private def genThrow(tree: Throw): Type = {
    genTree(tree.expr, AnyType)
    markPosition(tree)
    instrs += wa.EXTERN_CONVERT_ANY
    instrs += wa.THROW(genTagName.exceptionTagName)

    NothingType
  }

  private def genBlock(t: Block, expectedType: Type): Type = {
    genBlockStats(t.stats.init) {
      genTree(t.stats.last, expectedType)
    }
    expectedType
  }

  final def genBlockStats[A](stats: List[Tree])(inner: => A): A = {
    stats match {
      case (stat @ VarDef(name, _, vtpe, _, rhs)) :: rest =>
        genTree(rhs, vtpe)
        markPosition(stat)
        withNewLocal(name.name, TypeTransformer.transformType(vtpe)(ctx)) { local =>
          instrs += wa.LOCAL_SET(local)
          genBlockStats(rest)(inner)
        }
      case stat :: rest =>
        genTree(stat, NoType)
        genBlockStats(rest)(inner)
      case Nil =>
        inner
    }
  }

  private def genNew(n: New): Type = {
    /* Do not use transformType here, because we must get the struct type even
     * if the given class is an ancestor of hijacked classes (which in practice
     * is only the case for j.l.Object).
     */
    val instanceTyp = watpe.RefType(genTypeName.forClass(n.className))
    val localInstance = addSyntheticLocal(instanceTyp)

    markPosition(n)
    instrs += wa.CALL(genFunctionName.newDefault(n.className))
    instrs += wa.LOCAL_TEE(localInstance)

    genArgs(n.args, n.ctor.name)

    markPosition(n)

    instrs += wa.CALL(
      genFunctionName.forMethod(
        MemberNamespace.Constructor,
        n.className,
        n.ctor.name
      )
    )
    instrs += wa.LOCAL_GET(localInstance)
    n.tpe
  }

  /** Codegen to box a primitive `char`/`long` into a `CharacterBox`/`LongBox`. */
  private def genBox(
      primType: PrimTypeWithRef,
      boxClassName: ClassName
  ): Type = {
    // `primTyp` is `i32` for `char` (containing a `u16` value) or `i64` for `long`.
    val primTyp = TypeTransformer.transformType(primType)(ctx)
    val primLocal = addSyntheticLocal(primTyp)

    val boxClassType = ClassType(boxClassName)
    val boxTyp = TypeTransformer.transformClassType(boxClassName)(ctx).toNonNullable
    val instanceLocal = addSyntheticLocal(boxTyp)

    /* The generated code is as follows. Before the codegen, the stack contains
     * a value of the primitive type. After, it contains the reference to box instance.
     *
     *                          ;; [$prim]
     * local.set $primLocal     ;; []
     * call $Box.newDefault     ;; [$boxInstance]
     * local.tee $instanceLocal ;; [$boxInstance]
     * local.get $primLocal     ;; [$boxInstance, $prim]
     * struct.set $Box 2        ;; [] assign $prim to the `value` field of boxInstance
     * local.get $instanceLocal ;; [$boxInstance]
     *
     * We directly set the `value` field (the only field) of the box class,
     * generated by `LibraryPatches.deriveBoxClass`, instead of explicitly
     * calling the constructor. We can do this because we know that this is
     * what the constructor would do anyway (so we're basically inlining it).
     */

    instrs += wa.LOCAL_SET(primLocal)
    instrs += wa.CALL(genFunctionName.newDefault(boxClassName))
    instrs += wa.LOCAL_TEE(instanceLocal)
    instrs += wa.LOCAL_GET(primLocal)
    instrs += wa.STRUCT_SET(
      genTypeName.forClass(boxClassName),
      genFieldIdx.objStruct.uniqueRegularField
    )
    instrs += wa.LOCAL_GET(instanceLocal)

    boxClassType
  }

  private def genIdentityHashCode(tree: IdentityHashCode): Type = {
    // TODO Avoid dispatch when we know a more precise type than any
    genTree(tree.expr, AnyType)

    markPosition(tree)
    instrs += wa.CALL(genFunctionName.identityHashCode)

    IntType
  }

  private def genWrapAsThrowable(tree: WrapAsThrowable): Type = {
    val throwableClassType = ClassType(ThrowableClass)
    val nonNullThrowableTyp = watpe.RefType(genTypeName.ThrowableStruct)

    val jsExceptionTyp =
      TypeTransformer.transformClassType(SpecialNames.JSExceptionClass)(ctx).toNonNullable

    instrs.block(nonNullThrowableTyp) { doneLabel =>
      genTree(tree.expr, AnyType)

      markPosition(tree)

      // if expr.isInstanceOf[Throwable], then br $done
      instrs += wa.BR_ON_CAST(
        doneLabel,
        watpe.RefType.anyref,
        nonNullThrowableTyp
      )

      // otherwise, wrap in a new JavaScriptException

      val exprLocal = addSyntheticLocal(watpe.RefType.anyref)
      val instanceLocal = addSyntheticLocal(jsExceptionTyp)

      instrs += wa.LOCAL_SET(exprLocal)
      instrs += wa.CALL(genFunctionName.newDefault(SpecialNames.JSExceptionClass))
      instrs += wa.LOCAL_TEE(instanceLocal)
      instrs += wa.LOCAL_GET(exprLocal)
      instrs += wa.CALL(
        genFunctionName.forMethod(
          MemberNamespace.Constructor,
          SpecialNames.JSExceptionClass,
          SpecialNames.JSExceptionCtor
        )
      )
      instrs += wa.LOCAL_GET(instanceLocal)
    }

    throwableClassType
  }

  private def genUnwrapFromThrowable(tree: UnwrapFromThrowable): Type = {
    instrs.block(watpe.RefType.anyref) { doneLabel =>
      genTree(tree.expr, ClassType(ThrowableClass))

      markPosition(tree)

      instrs += wa.REF_AS_NOT_NULL

      // if !expr.isInstanceOf[js.JavaScriptException], then br $done
      instrs += wa.BR_ON_CAST_FAIL(
        doneLabel,
        watpe.RefType(genTypeName.ThrowableStruct),
        watpe.RefType(genTypeName.JSExceptionStruct)
      )

      // otherwise, unwrap the JavaScriptException by reading its field

      val idx =
        ctx.getClassInfo(SpecialNames.JSExceptionClass).getFieldIdx(SpecialNames.JSExceptionField)

      instrs += wa.STRUCT_GET(genTypeName.forClass(SpecialNames.JSExceptionClass), idx)
    }

    AnyType
  }

  private def genJSNew(tree: JSNew): Type = {
    genTree(tree.ctor, AnyType)
    genJSArgsArray(tree.args)
    markPosition(tree)
    instrs += wa.CALL(genFunctionName.jsNew)
    AnyType
  }

  private def genJSSelect(tree: JSSelect): Type = {
    genTree(tree.qualifier, AnyType)
    genTree(tree.item, AnyType)
    markPosition(tree)
    instrs += wa.CALL(genFunctionName.jsSelect)
    AnyType
  }

  private def genJSFunctionApply(tree: JSFunctionApply): Type = {
    genTree(tree.fun, AnyType)
    genJSArgsArray(tree.args)
    markPosition(tree)
    instrs += wa.CALL(genFunctionName.jsFunctionApply)
    AnyType
  }

  private def genJSMethodApply(tree: JSMethodApply): Type = {
    genTree(tree.receiver, AnyType)
    genTree(tree.method, AnyType)
    genJSArgsArray(tree.args)
    markPosition(tree)
    instrs += wa.CALL(genFunctionName.jsMethodApply)
    AnyType
  }

  private def genJSImportCall(tree: JSImportCall): Type = {
    genTree(tree.arg, AnyType)
    markPosition(tree)
    instrs += wa.CALL(genFunctionName.jsImportCall)
    AnyType
  }

  private def genJSImportMeta(tree: JSImportMeta): Type = {
    markPosition(tree)
    instrs += wa.CALL(genFunctionName.jsImportMeta)
    AnyType
  }

  private def genLoadJSConstructor(tree: LoadJSConstructor): Type = {
    markPosition(tree)
    SWasmGen.genLoadJSConstructor(instrs, tree.className)(ctx)
    AnyType
  }

  private def genLoadJSModule(tree: LoadJSModule): Type = {
    markPosition(tree)

    val info = ctx.getClassInfo(tree.className)

    info.kind match {
      case ClassKind.NativeJSModuleClass =>
        val jsNativeLoadSpec = info.jsNativeLoadSpec.getOrElse {
          throw new AssertionError(s"Found $tree for class without jsNativeLoadSpec at ${tree.pos}")
        }
        genLoadJSFromSpec(instrs, jsNativeLoadSpec)(ctx)
        AnyType

      case ClassKind.JSModuleClass =>
        instrs += wa.CALL(genFunctionName.loadModule(tree.className))
        AnyType

      case _ =>
        throw new AssertionError(
          s"Invalid LoadJSModule for class ${tree.className.nameString} of kind ${info.kind}"
        )
    }
  }

  private def genSelectJSNativeMember(tree: SelectJSNativeMember): Type = {
    val info = ctx.getClassInfo(tree.className)
    val jsNativeLoadSpec = info.jsNativeMembers.getOrElse(
      tree.member.name, {
        throw new AssertionError(s"Found $tree for non-existing JS native member at ${tree.pos}")
      }
    )
    genLoadJSFromSpec(instrs, jsNativeLoadSpec)(ctx)
    AnyType
  }

  private def genJSDelete(tree: JSDelete): Type = {
    genTree(tree.qualifier, AnyType)
    genTree(tree.item, AnyType)
    markPosition(tree)
    instrs += wa.CALL(genFunctionName.jsDelete)
    NoType
  }

  private def genJSUnaryOp(tree: JSUnaryOp): Type = {
    genTree(tree.lhs, AnyType)
    markPosition(tree)
    instrs += wa.CALL(genFunctionName.jsUnaryOps(tree.op))
    AnyType
  }

  private def genJSBinaryOp(tree: JSBinaryOp): Type = {
    tree.op match {
      case JSBinaryOp.|| | JSBinaryOp.&& =>
        /* Here we need to implement the short-circuiting behavior, with a
         * condition based on the truthy value of the left-hand-side.
         */
        val lhsLocal = addSyntheticLocal(watpe.RefType.anyref)
        genTree(tree.lhs, AnyType)
        markPosition(tree)
        instrs += wa.LOCAL_TEE(lhsLocal)
        instrs += wa.CALL(genFunctionName.jsIsTruthy)
        instrs += wa.IF(wa.BlockType.ValueType(watpe.RefType.anyref))
        if (tree.op == JSBinaryOp.||) {
          instrs += wa.LOCAL_GET(lhsLocal)
          instrs += wa.ELSE
          genTree(tree.rhs, AnyType)
          markPosition(tree)
        } else {
          genTree(tree.rhs, AnyType)
          markPosition(tree)
          instrs += wa.ELSE
          instrs += wa.LOCAL_GET(lhsLocal)
        }
        instrs += wa.END

      case _ =>
        genTree(tree.lhs, AnyType)
        genTree(tree.rhs, AnyType)
        markPosition(tree)
        instrs += wa.CALL(genFunctionName.jsBinaryOps(tree.op))
    }

    tree.tpe
  }

  private def genJSArrayConstr(tree: JSArrayConstr): Type = {
    genJSArgsArray(tree.items)
    AnyType
  }

  private def genJSObjectConstr(tree: JSObjectConstr): Type = {
    markPosition(tree)
    instrs += wa.CALL(genFunctionName.jsNewObject)
    for ((prop, value) <- tree.fields) {
      genTree(prop, AnyType)
      genTree(value, AnyType)
      instrs += wa.CALL(genFunctionName.jsObjectPush)
    }
    AnyType
  }

  private def genJSGlobalRef(tree: JSGlobalRef): Type = {
    markPosition(tree)
    instrs ++= ctx.getConstantStringInstr(tree.name)
    instrs += wa.CALL(genFunctionName.jsGlobalRefGet)
    AnyType
  }

  private def genJSTypeOfGlobalRef(tree: JSTypeOfGlobalRef): Type = {
    markPosition(tree)
    instrs ++= ctx.getConstantStringInstr(tree.globalRef.name)
    instrs += wa.CALL(genFunctionName.jsGlobalRefTypeof)
    AnyType
  }

  private def genJSArgsArray(args: List[TreeOrJSSpread]): Unit = {
    instrs += wa.CALL(genFunctionName.jsNewArray)
    for (arg <- args) {
      arg match {
        case arg: Tree =>
          genTree(arg, AnyType)
          instrs += wa.CALL(genFunctionName.jsArrayPush)
        case JSSpread(items) =>
          genTree(items, AnyType)
          instrs += wa.CALL(genFunctionName.jsArraySpreadPush)
      }
    }
  }

  private def genJSLinkingInfo(tree: JSLinkingInfo): Type = {
    markPosition(tree)
    instrs += wa.CALL(genFunctionName.jsLinkingInfo)
    AnyType
  }

  // ===============================================================================
  // array
  // ===============================================================================
  private def genArrayLength(t: ArrayLength): Type = {
    genTreeAuto(t.array)

    markPosition(t)

    t.array.tpe match {
      case ArrayType(arrayTypeRef) =>
        // Get the underlying array; implicit trap on null
        instrs += wa.STRUCT_GET(
          genTypeName.forArrayClass(arrayTypeRef),
          genFieldIdx.objStruct.uniqueRegularField
        )
        // Get the length
        instrs += wa.ARRAY_LEN
        IntType

      case NothingType =>
        // unreachable
        NothingType
      case NullType =>
        instrs += wa.UNREACHABLE
        NothingType
      case _ =>
        throw new IllegalArgumentException(
          s"ArraySelect.array must be an array type, but has type ${t.array.tpe}"
        )
    }
  }

  private def genNewArray(t: NewArray): Type = {
    val arrayTypeRef = t.typeRef

    if (t.lengths.isEmpty || t.lengths.size > arrayTypeRef.dimensions)
      throw new AssertionError(
        s"invalid lengths ${t.lengths} for array type ${arrayTypeRef.displayName}"
      )

    markPosition(t)

    if (t.lengths.size == 1) {
      genLoadVTableAndITableForArray(arrayTypeRef)

      // Create the underlying array
      genTree(t.lengths.head, IntType)
      markPosition(t)

      val underlyingArrayType = genTypeName.underlyingOf(arrayTypeRef)
      instrs += wa.ARRAY_NEW_DEFAULT(underlyingArrayType)

      // Create the array object
      instrs += wa.STRUCT_NEW(genTypeName.forArrayClass(arrayTypeRef))
    } else {
      /* There is no Scala source code that produces `NewArray` with more than
       * one specified dimension, so this branch is not tested.
       * (The underlying function `newArrayObject` is tested as part of
       * reflective array instantiations, though.)
       */

      // First arg to `newArrayObject`: the typeData of the array to create
      genLoadArrayTypeData(arrayTypeRef)

      // Second arg: an array of the lengths
      for (length <- t.lengths)
        genTree(length, IntType)
      markPosition(t)
      instrs += wa.ARRAY_NEW_FIXED(genTypeName.i32Array, t.lengths.size)

      // Third arg: constant 0 (start index inside the array of lengths)
      instrs += wa.I32_CONST(0)

      instrs += wa.CALL(genFunctionName.newArrayObject)
    }

    t.tpe
  }

  /** Gen code to load the vtable and the itable of the given array type. */
  private def genLoadVTableAndITableForArray(arrayTypeRef: ArrayTypeRef): Unit = {
    // Load the typeData of the resulting array type. It is the vtable of the resulting object.
    genLoadArrayTypeData(arrayTypeRef)

    // Load the itables for the array type
    instrs += wa.GLOBAL_GET(genGlobalName.arrayClassITable)
  }

  /** For getting element from an array, array.set should be generated by transformation of
    * `Assign(ArraySelect(...), ...)`
    */
  private def genArraySelect(t: ArraySelect): Type = {
    genTreeAuto(t.array)

    markPosition(t)

    t.array.tpe match {
      case ArrayType(arrayTypeRef) =>
        // Get the underlying array; implicit trap on null
        instrs += wa.STRUCT_GET(
          genTypeName.forArrayClass(arrayTypeRef),
          genFieldIdx.objStruct.uniqueRegularField
        )

        // Load the index
        genTree(t.index, IntType)

        markPosition(t)

        // Use the appropriate variant of array.get for sign extension
        val typeIdx = genTypeName.underlyingOf(arrayTypeRef)
        arrayTypeRef match {
          case ArrayTypeRef(BooleanRef | CharRef, 1) =>
            instrs += wa.ARRAY_GET_U(typeIdx)
          case ArrayTypeRef(ByteRef | ShortRef, 1) =>
            instrs += wa.ARRAY_GET_S(typeIdx)
          case _ =>
            instrs += wa.ARRAY_GET(typeIdx)
        }

        /* If it is a reference array type whose element type does not translate
         * to `anyref`, we must cast down the result.
         */
        arrayTypeRef match {
          case ArrayTypeRef(_: PrimRef, 1) =>
            // a primitive array type always has the correct
            ()
          case _ =>
            TypeTransformer.transformType(t.tpe)(ctx) match {
              case watpe.RefType.anyref =>
                // nothing to do
                ()
              case refType: watpe.RefType =>
                instrs += wa.REF_CAST(refType)
              case typ =>
                throw new AssertionError(s"Unexpected result type for reference array: $typ")
            }
        }

        t.tpe

      case NothingType =>
        // unreachable
        NothingType
      case NullType =>
        instrs += wa.UNREACHABLE
        NothingType
      case _ =>
        throw new IllegalArgumentException(
          s"ArraySelect.array must be an array type, but has type ${t.array.tpe}"
        )
    }
  }

  private def genArrayValue(t: ArrayValue): Type = {
    val arrayTypeRef = t.typeRef

    markPosition(t)

    genLoadVTableAndITableForArray(arrayTypeRef)

    val expectedElemType = arrayTypeRef match {
      case ArrayTypeRef(base: PrimRef, 1) => base.tpe
      case _                              => AnyType
    }

    // Create the underlying array
    t.elems.foreach(genTree(_, expectedElemType))
    markPosition(t)
    val underlyingArrayType = genTypeName.underlyingOf(arrayTypeRef)
    instrs += wa.ARRAY_NEW_FIXED(underlyingArrayType, t.elems.size)

    // Create the array object
    instrs += wa.STRUCT_NEW(genTypeName.forArrayClass(arrayTypeRef))

    t.tpe
  }

  private def genClosure(tree: Closure): Type = {
    implicit val pos = tree.pos
    implicit val ctx = this.ctx

    val hasThis = !tree.arrow
    val hasRestParam = tree.restParam.isDefined
    val dataStructTypeName = ctx.getClosureDataStructType(tree.captureParams.map(_.ptpe))

    // Define the function where captures are reified as a `__captureData` argument.
    val closureFuncName = genInnerFuncName()
    emitFunction(
      closureFuncName,
      enclosingClassName = None,
      Some(tree.captureParams),
      receiverTyp = if (!hasThis) None else Some(watpe.RefType.anyref),
      tree.params,
      tree.restParam,
      tree.body,
      resultType = AnyType
    )

    markPosition(tree)

    // Put a reference to the function on the stack
    instrs += ctx.refFuncWithDeclaration(closureFuncName)

    // Evaluate the capture values and instantiate the capture data struct
    for ((param, value) <- tree.captureParams.zip(tree.captureValues))
      genTree(value, param.ptpe)
    markPosition(tree)
    instrs += wa.STRUCT_NEW(dataStructTypeName)

    /* If there is a ...rest param, the helper requires as third argument the
     * number of regular arguments.
     */
    if (hasRestParam)
      instrs += wa.I32_CONST(tree.params.size)

    // Call the appropriate helper
    val helper = (hasThis, hasRestParam) match {
      case (false, false) => genFunctionName.closure
      case (true, false)  => genFunctionName.closureThis
      case (false, true)  => genFunctionName.closureRest
      case (true, true)   => genFunctionName.closureThisRest
    }
    instrs += wa.CALL(helper)

    AnyType
  }

  private def genClone(t: Clone): Type = {
    val expr = addSyntheticLocal(TypeTransformer.transformType(t.expr.tpe)(ctx))

    genTree(t.expr, ClassType(CloneableClass))

    markPosition(t)

    instrs += wa.REF_CAST(watpe.RefType(genTypeName.ObjectStruct))
    instrs += wa.LOCAL_TEE(expr)
    instrs += wa.REF_AS_NOT_NULL // cloneFunction argument is not nullable

    instrs += wa.LOCAL_GET(expr)
    instrs += wa.STRUCT_GET(
      genTypeName.forClass(ObjectClass),
      genFieldIdx.objStruct.vtable
    )
    instrs += wa.STRUCT_GET(
      genTypeName.typeData,
      genFieldIdx.typeData.cloneFunctionIdx
    )
    // cloneFunction: (ref j.l.Object) -> ref j.l.Object
    instrs += wa.CALL_REF(genTypeName.cloneFunctionType)

    t.tpe match {
      case ClassType(className) =>
        val info = ctx.getClassInfo(className)
        if (!info.isInterface) // if it's interface, no need to cast from j.l.Object
          instrs += wa.REF_CAST(watpe.RefType(genTypeName.forClass(className)))
      case _ =>
        throw new IllegalArgumentException(
          s"Clone result type must be a class type, but is ${t.tpe}"
        )
    }
    t.tpe
  }

  private def genMatch(tree: Match, expectedType: Type): Type = {
    val Match(selector, cases, defaultBody) = tree
    val selectorLocal = addSyntheticLocal(TypeTransformer.transformType(selector.tpe)(ctx))

    genTreeAuto(selector)

    markPosition(tree)

    instrs += wa.LOCAL_SET(selectorLocal)

    instrs.block(TypeTransformer.transformResultType(expectedType)(ctx)) { doneLabel =>
      instrs.block() { defaultLabel =>
        val caseLabels = cases.map(c => c._1 -> instrs.genLabel())
        for (caseLabel <- caseLabels)
          instrs += wa.BLOCK(wa.BlockType.ValueType(), Some(caseLabel._2))

        for {
          caseLabel <- caseLabels
          matchableLiteral <- caseLabel._1
        } {
          markPosition(matchableLiteral)
          val label = caseLabel._2
          instrs += wa.LOCAL_GET(selectorLocal)
          matchableLiteral match {
            case IntLiteral(value) =>
              instrs += wa.I32_CONST(value)
              instrs += wa.I32_EQ
              instrs += wa.BR_IF(label)
            case StringLiteral(value) =>
              instrs ++= ctx.getConstantStringInstr(value)
              instrs += wa.CALL(genFunctionName.is)
              instrs += wa.BR_IF(label)
            case Null() =>
              instrs += wa.REF_IS_NULL
              instrs += wa.BR_IF(label)
          }
        }
        instrs += wa.BR(defaultLabel)

        for ((caseLabel, caze) <- caseLabels.zip(cases).reverse) {
          markPosition(caze._2)
          instrs += wa.END
          genTree(caze._2, expectedType)
          instrs += wa.BR(doneLabel)
        }
      }
      genTree(defaultBody, expectedType)
    }

    if (expectedType == NothingType)
      instrs += wa.UNREACHABLE

    expectedType
  }

  private def genCreateJSClass(tree: CreateJSClass): Type = {
    val classInfo = ctx.getClassInfo(tree.className)
    val jsClassCaptures = classInfo.jsClassCaptures.getOrElse {
      throw new AssertionError(
        s"Illegal CreateJSClass of top-level class ${tree.className.nameString}"
      )
    }

    for ((captureValue, captureParam) <- tree.captureValues.zip(jsClassCaptures))
      genTree(captureValue, captureParam.ptpe)

    markPosition(tree)

    instrs += wa.CALL(genFunctionName.createJSClassOf(tree.className))

    AnyType
  }

  private def genJSPrivateSelect(tree: JSPrivateSelect): Type = {
    genTree(tree.qualifier, AnyType)

    markPosition(tree)

    instrs += wa.GLOBAL_GET(genGlobalName.forJSPrivateField(tree.field.name))
    instrs += wa.CALL(genFunctionName.jsSelect)

    AnyType
  }

  private def genJSSuperSelect(tree: JSSuperSelect): Type = {
    genTree(tree.superClass, AnyType)
    genTree(tree.receiver, AnyType)
    genTree(tree.item, AnyType)

    markPosition(tree)

    instrs += wa.CALL(genFunctionName.jsSuperGet)

    AnyType
  }

  private def genJSSuperMethodCall(tree: JSSuperMethodCall): Type = {
    genTree(tree.superClass, AnyType)
    genTree(tree.receiver, AnyType)
    genTree(tree.method, AnyType)
    genJSArgsArray(tree.args)

    markPosition(tree)

    instrs += wa.CALL(genFunctionName.jsSuperCall)

    AnyType
  }

  private def genJSNewTarget(tree: JSNewTarget): Type = {
    markPosition(tree)

    genReadStorage(newTargetStorage)

    AnyType
  }

  /*--------------------------------------------------------------------*
   * HERE BE DRAGONS --- Handling of TryFinally, Labeled and Return --- *
   *--------------------------------------------------------------------*/

  /* From this point onwards, and until the end of the file, you will find
   * the infrastructure required to handle TryFinally and Labeled/Return pairs.
   *
   * Independently, TryFinally and Labeled/Return are not very difficult to
   * handle. The dragons come when they interact, and in particular when a
   * TryFinally stands in the middle of a Labeled/Return pair.
   *
   * For example:
   *
   * val foo: int = alpha[int]: {
   *   val bar: string = try {
   *     if (somethingHappens)
   *       return@alpha 5
   *     "bar"
   *   } finally {
   *     doTheFinally()
   *   }
   *   someOtherThings(bar)
   * }
   *
   * In that situation, if we naively translate the `return@alpha` into
   * `br $alpha`, we bypass the `finally` block, which goes against the spec.
   *
   * Instead, we must stash the result 5 in a local and jump to the finally
   * block. The issue is that, at the end of `doTheFinally()`, we need to keep
   * propagating further up (instead of executing `someOtherThings()`).
   *
   * That means that there are 3 possible outcomes after the `finally` block:
   *
   * - Rethrow the exception if we caught one.
   * - Reload the stashed result and branch further to `alpha`.
   * - Otherwise keep going to do `someOtherThings()`.
   *
   * Now what if there are *several* labels for which we cross that
   * `try..finally`? Well we need to deal with all the possible labels. This
   * means that, in general, we in fact have `2 + n` possible outcomes, where
   * `n` is the number of labels for which we found a `Return` that crosses the
   * boundary.
   *
   * In order to know whether we need to rethrow, we look at a nullable
   * `exnref`. For the remaining cases, we use a separate `destinationTag`
   * local. Every label gets assigned a distinct tag > 0. Fall-through is
   * always represented by 0. Before branching to a `finally` block, we set the
   * appropriate value to the `destinationTag` value.
   *
   * Since the various labels can have different result types, and since they
   * can be different from the result of the regular flow of the `try` block,
   * we have to normalize to `void` for the `try_table` itself. Each label has
   * a dedicated local for its result if it comes from such a crossing
   * `return`.
   *
   * Two more complications:
   *
   * - If the `finally` block itself contains another `try..finally`, they may
   *   need a `destinationTag` concurrently. Therefore, every `try..finally`
   *   gets its own `destinationTag` local.
   * - If the `try` block contains another `try..finally`, so that there are
   *   two (or more) `try..finally` in the way between a `Return` and a
   *   `Labeled`, we must forward to the next `finally` in line (and its own
   *   `destinationTag` local) so that the whole chain gets executed before
   *   reaching the `Labeled`.
   *
   * ---
   *
   * As an evil example of everything that can happen, consider:
   *
   * alpha[double]: { // allocated destinationTag = 1
   *   val foo: int = try { // uses the local destinationTagOuter
   *     beta[int]: { // allocated destinationTag = 2
   *       val bar: int = try { // uses the local destinationTagInner
   *         if (A) return@alpha 5
   *         if (B) return@beta 10
   *         56
   *       } finally {
   *         doTheFinally()
   *         // not shown: there is another try..finally here
   *         // its destinationTagLocal must be different than destinationTag
   *         // since both are live at the same time.
   *       }
   *       someOtherThings(bar)
   *     }
   *   } finally {
   *     doTheOuterFinally()
   *   }
   *   moreOtherThings(foo)
   * }
   *
   * The whole compiled code is too overwhelming to be useful, so we show the
   * important aspects piecemiel, from the bottom up.
   *
   * First, the compiled code for `return@alpha 5`:
   *
   * i32.const 5                    ; eval the argument of the return
   * local.set $alphaResult         ; store it in $alphaResult because we are cross a try..finally
   * i32.const 1                    ; the destination tag of alpha
   * local.set $destinationTagInner ; store it in the destinationTag local of the inner try..finally
   * br $innerCross                 ; branch to the cross label of the inner try..finally
   *
   * Second, we look at the shape generated for the inner try..finally:
   *
   * block $innerDone (result i32)
   *   block $innerCatch (result exnref)
   *     block $innerCross
   *       try_table (catch_all_ref $innerCatch)
   *         ; [...] body of the try
   *
   *         local.set $innerTryResult
   *       end ; try_table
   *
   *       ; set destinationTagInner := 0 to mean fall-through
   *       i32.const 0
   *       local.set $destinationTagInner
   *     end ; block $innerCross
   *
   *     ; no exception thrown
   *     ref.null exn
   *   end ; block $innerCatch
   *
   *   ; now we have the common code with the finally
   *
   *   ; [...] body of the finally
   *
   *   ; maybe re-throw
   *   block $innerExnIsNull (param exnref)
   *     br_on_null $innerExnIsNull
   *     throw_ref
   *   end
   *
   *   ; re-dispatch after the outer finally based on $destinationTagInner
   *
   *   ; first transfer our destination tag to the outer try's destination tag
   *   local.get $destinationTagInner
   *   local.set $destinationTagOuter
   *
   *   ; now use a br_table to jump to the appropriate destination
   *   ; if 0, fall-through
   *   ; if 1, go the outer try's cross label because it is still on the way to alpha
   *   ; if 2, go to beta's cross label
   *   ; default to fall-through (never used but br_table needs a default)
   *   br_table $innerDone $outerCross $betaCross $innerDone
   * end ; block $innerDone
   *
   * We omit the shape of beta and of the outer try. There are similar to the
   * shape of alpha and inner try, respectively.
   *
   * We conclude with the shape of the alpha block:
   *
   * block $alpha (result f64)
   *   block $alphaCross
   *     ; begin body of alpha
   *
   *     ; [...]              ; the try..finally
   *     local.set $foo       ; val foo =
   *     moreOtherThings(foo)
   *
   *     ; end body of alpha
   *
   *     br $alpha ; if alpha finished normally, jump over `local.get $alphaResult`
   *   end ; block $alphaCross
   *
   *   ; if we returned from alpha across a try..finally, fetch the result from the local
   *   local.get $alphaResult
   * end ; block $alpha
   */

  /** This object namespaces everything related to unwinding, so that we don't pollute too much the
    * overall internal scope of `ExpressionBuilder`.
    */
  private object unwinding {

    /** The number of enclosing `Labeled` and `TryFinally` blocks.
      *
      * For `TryFinally`, it is only enclosing if we are in the `try` branch, not the `finally`
      * branch.
      *
      * Invariant:
      * {{{
      * currentUnwindingStackDepth == enclosingTryFinallyStack.size + enclosingLabeledBlocks.size
      * }}}
      */
    private var currentUnwindingStackDepth: Int = 0

    private var enclosingTryFinallyStack: List[TryFinallyEntry] = Nil

    private var enclosingLabeledBlocks: Map[LabelName, LabeledEntry] = Map.empty

    private def innermostTryFinally: Option[TryFinallyEntry] =
      enclosingTryFinallyStack.headOption

    private def enterTryFinally(entry: TryFinallyEntry)(body: => Unit): Unit = {
      assert(entry.depth == currentUnwindingStackDepth)
      enclosingTryFinallyStack ::= entry
      currentUnwindingStackDepth += 1
      try {
        body
      } finally {
        currentUnwindingStackDepth -= 1
        enclosingTryFinallyStack = enclosingTryFinallyStack.tail
      }
    }

    private def enterLabeled(entry: LabeledEntry)(body: => Unit): Unit = {
      assert(entry.depth == currentUnwindingStackDepth)
      val savedLabeledBlocks = enclosingLabeledBlocks
      enclosingLabeledBlocks = enclosingLabeledBlocks.updated(entry.irLabelName, entry)
      currentUnwindingStackDepth += 1
      try {
        body
      } finally {
        currentUnwindingStackDepth -= 1
        enclosingLabeledBlocks = savedLabeledBlocks
      }
    }

    /** The last destination tag that was allocated to a LabeledEntry. */
    private var lastDestinationTag: Int = 0

    private def allocateDestinationTag(): Int = {
      lastDestinationTag += 1
      lastDestinationTag
    }

    /** Information about an enclosing `TryFinally` block. */
    private final class TryFinallyEntry(val depth: Int) {
      private var _crossInfo: Option[(wanme.LocalName, wanme.LabelName)] = None

      def isInside(labeledEntry: LabeledEntry): Boolean =
        this.depth > labeledEntry.depth

      def wasCrossed: Boolean = _crossInfo.isDefined

      def requireCrossInfo(): (wanme.LocalName, wanme.LabelName) = {
        _crossInfo.getOrElse {
          val info = (addSyntheticLocal(watpe.Int32), instrs.genLabel())
          _crossInfo = Some(info)
          info
        }
      }
    }

    /** Information about an enclosing `Labeled` block. */
    private final class LabeledEntry(
        val depth: Int,
        val irLabelName: LabelName,
        val expectedType: Type
    ) {

      /** The regular label for this `Labeled` block, used for `Return`s that do not cross a
        * `TryFinally`.
        */
      val regularWasmLabel: wanme.LabelName = instrs.genLabel()

      /** The destination tag allocated to this label, used by the `finally` blocks to keep
        * propagating to the right destination.
        *
        * Destination tags are always `> 0`. The value `0` is reserved for fall-through.
        */
      private var destinationTag: Int = 0

      /** The locals in which to store the result of the label if we have to cross a `try..finally`.
        */
      private var resultLocals: List[wanme.LocalName] = null

      /** An additional Wasm label that has a `[]` result, and which will get its result from the
        * `resultLocal` instead of expecting it on the stack.
        */
      private var crossLabel: wanme.LabelName = null

      def wasCrossUsed: Boolean = destinationTag != 0

      def requireCrossInfo(): (Int, List[wanme.LocalName], wanme.LabelName) = {
        if (destinationTag == 0) {
          destinationTag = allocateDestinationTag()
          val resultTypes = TypeTransformer.transformResultType(expectedType)(ctx)
          resultLocals = resultTypes.map(addSyntheticLocal(_))
          crossLabel = instrs.genLabel()
        }

        (destinationTag, resultLocals, crossLabel)
      }
    }

    def genLabeled(t: Labeled, expectedType: Type): Type = {
      val entry = new LabeledEntry(currentUnwindingStackDepth, t.label.name, expectedType)

      val ty = TypeTransformer.transformResultType(expectedType)(ctx)

      markPosition(t)

      // Manual wa.BLOCK here because we have a specific `label`
      instrs += wa.BLOCK(
        instrs.sigToBlockType(Sig(Nil, ty)),
        Some(entry.regularWasmLabel)
      )

      /* Remember the position in the instruction stream, in case we need to
       * come back and insert the wa.BLOCK for the cross handling.
       */
      val instrsBlockBeginIndex = instrs.markCurrentInstructionIndex()

      // Emit the body
      enterLabeled(entry) {
        genTree(t.body, expectedType)
      }

      markPosition(t)

      // Deal with crossing behavior
      if (entry.wasCrossUsed) {
        assert(
          expectedType != NothingType,
          "The tryFinallyCrossLabel should not have been used for label " +
            s"${t.label.name.nameString} of type nothing"
        )

        /* In this case we need to handle situations where we receive the value
         * from the label's `result` local, branching out of the label's
         * `crossLabel`.
         *
         * Instead of the standard shape
         *
         * block $labeled (result t)
         *   body
         * end
         *
         * We need to amend the shape to become
         *
         * block $labeled (result t)
         *   block $crossLabel
         *     body            ; inside the body, jumps to this label after a
         *                     ; `finally` are compiled as `br $crossLabel`
         *     br $labeled
         *   end
         *   local.get $label.resultLocals ; (0 to many)
         * end
         */

        val (_, resultLocals, crossLabel) = entry.requireCrossInfo()

        // Go back and insert the `block $crossLabel` right after `block $labeled`
        instrs.insert(instrsBlockBeginIndex, wa.BLOCK(wa.BlockType.ValueType(), Some(crossLabel)))

        // Add the `br`, `end` and `local.get` at the current position, as usual
        instrs += wa.BR(entry.regularWasmLabel)
        instrs += wa.END
        for (local <- resultLocals)
          instrs += wa.LOCAL_GET(local)
      }

      instrs += wa.END

      if (expectedType == NothingType)
        instrs += wa.UNREACHABLE

      expectedType
    }

    def genTryFinally(t: TryFinally, expectedType: Type): Type = {
      val entry = new TryFinallyEntry(currentUnwindingStackDepth)

      val resultType = TypeTransformer.transformResultType(expectedType)(ctx)
      val resultLocals = resultType.map(addSyntheticLocal(_))

      markPosition(t)

      instrs.block() { doneLabel =>
        instrs.block(watpe.RefType.exnref) { catchLabel =>
          /* Remember the position in the instruction stream, in case we need
           * to come back and insert the wa.BLOCK for the cross handling.
           */
          val instrsBlockBeginIndex = instrs.markCurrentInstructionIndex()

          instrs.tryTable()(List(wa.CatchClause.CatchAllRef(catchLabel))) {
            // try block
            enterTryFinally(entry) {
              genTree(t.block, expectedType)
            }

            markPosition(t)

            // store the result in locals during the finally block
            for (resultLocal <- resultLocals.reverse)
              instrs += wa.LOCAL_SET(resultLocal)
          }

          /* If this try..finally was crossed by a `Return`, we need to amend
           * the shape of our try part to
           *
           * block $catch (result exnref)
           *   block $cross
           *     try_table (catch_all_ref $catch)
           *       body
           *       set_local $results ; 0 to many
           *     end
           *     i32.const 0 ; 0 always means fall-through
           *     local.set $destinationTag
           *   end
           *   ref.null exn
           * end
           */
          if (entry.wasCrossed) {
            val (destinationTagLocal, crossLabel) = entry.requireCrossInfo()

            // Go back and insert the `block $cross` right after `block $catch`
            instrs.insert(
              instrsBlockBeginIndex,
              wa.BLOCK(wa.BlockType.ValueType(), Some(crossLabel))
            )

            // And the other amendments normally
            instrs += wa.I32_CONST(0)
            instrs += wa.LOCAL_SET(destinationTagLocal)
            instrs += wa.END // of the inserted wa.BLOCK
          }

          // on success, push a `null_ref exn` on the stack
          instrs += wa.REF_NULL(watpe.HeapType.Exn)
        } // end block $catch

        // finally block (during which we leave the `(ref null exn)` on the stack)
        genTree(t.finalizer, NoType)

        markPosition(t)

        if (!entry.wasCrossed) {
          // If the `exnref` is non-null, rethrow it
          instrs += wa.BR_ON_NULL(doneLabel)
          instrs += wa.THROW_REF
        } else {
          /* If the `exnref` is non-null, rethrow it.
           * Otherwise, stay within the `$done` block.
           */
          instrs.block(Sig(List(watpe.RefType.exnref), Nil)) { exnrefIsNullLabel =>
            instrs += wa.BR_ON_NULL(exnrefIsNullLabel)
            instrs += wa.THROW_REF
          }

          /* Otherwise, use a br_table to dispatch to the right destination
           * based on the value of the try..finally's destinationTagLocal,
           * which is set by `Return` or to 0 for fall-through.
           */

          // The order does not matter here because they will be "re-sorted" by emitwa.BRTable
          val possibleTargetEntries =
            enclosingLabeledBlocks.valuesIterator.filter(_.wasCrossUsed).toList

          val nextTryFinallyEntry = innermostTryFinally // note that we're out of ourselves already
            .filter(nextTry => possibleTargetEntries.exists(nextTry.isInside(_)))

          /* Build the destination table for `br_table`. Target Labeled's that
           * are outside of the next try..finally in line go to the latter;
           * for other `Labeled`'s, we go to their cross label.
           */
          val brTableDests: List[(Int, wanme.LabelName)] = possibleTargetEntries.map {
            targetEntry =>
              val (destinationTag, _, crossLabel) = targetEntry.requireCrossInfo()
              val label = nextTryFinallyEntry.filter(_.isInside(targetEntry)) match {
                case None          => crossLabel
                case Some(nextTry) => nextTry.requireCrossInfo()._2
              }
              destinationTag -> label
          }

          instrs += wa.LOCAL_GET(entry.requireCrossInfo()._1)
          for (nextTry <- nextTryFinallyEntry) {
            // Transfer the destinationTag to the next try..finally in line
            instrs += wa.LOCAL_TEE(nextTry.requireCrossInfo()._1)
          }
          emitBRTable(brTableDests, doneLabel)
        }
      } // end block $done

      // reload the result onto the stack
      for (resultLocal <- resultLocals)
        instrs += wa.LOCAL_GET(resultLocal)

      if (expectedType == NothingType)
        instrs += wa.UNREACHABLE

      expectedType
    }

    private def emitBRTable(
        dests: List[(Int, wanme.LabelName)],
        defaultLabel: wanme.LabelName
    ): Unit = {
      dests match {
        case Nil =>
          instrs += wa.DROP
          instrs += wa.BR(defaultLabel)

        case (singleDestValue, singleDestLabel) :: Nil =>
          /* Common case (as far as getting here in the first place is concerned):
           * All the `Return`s that cross the current `TryFinally` have the same
           * target destination (namely the enclosing `def` in the original program).
           */
          instrs += wa.I32_CONST(singleDestValue)
          instrs += wa.I32_EQ
          instrs += wa.BR_IF(singleDestLabel)
          instrs += wa.BR(defaultLabel)

        case _ :: _ =>
          // `max` is safe here because the list is non-empty
          val table = Array.fill(dests.map(_._1).max + 1)(defaultLabel)
          for (dest <- dests)
            table(dest._1) = dest._2
          instrs += wa.BR_TABLE(table.toList, defaultLabel)
      }
    }

    def genReturn(t: Return): Type = {
      val targetEntry = enclosingLabeledBlocks(t.label.name)

      genTree(t.expr, targetEntry.expectedType)

      markPosition(t)

      if (targetEntry.expectedType != NothingType) {
        innermostTryFinally.filter(_.isInside(targetEntry)) match {
          case None =>
            // Easy case: directly branch out of the block
            instrs += wa.BR(targetEntry.regularWasmLabel)

          case Some(tryFinallyEntry) =>
            /* Here we need to branch to the innermost enclosing `finally` block,
             * while remembering the destination label and the result value.
             */
            val (destinationTag, resultLocals, _) = targetEntry.requireCrossInfo()
            val (destinationTagLocal, crossLabel) = tryFinallyEntry.requireCrossInfo()

            // 1. Store the result in the label's result locals.
            for (local <- resultLocals.reverse)
              instrs += wa.LOCAL_SET(local)

            // 2. Store the label's destination tag into the try..finally's destination local.
            instrs += wa.I32_CONST(destinationTag)
            instrs += wa.LOCAL_SET(destinationTagLocal)

            // 3. Branch to the enclosing `finally` block's cross label.
            instrs += wa.BR(crossLabel)
        }
      }

      NothingType
    }
  }
}
