package wasm
package ir2wasm

import scala.annotation.switch

import scala.collection.mutable

import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Names => IRNames}

import wasm4s._
import wasm4s.Names._
import wasm4s.Names.WasmTypeName._
import wasm4s.WasmInstr._
import org.scalajs.ir.Types.ClassType
import org.scalajs.ir.ClassKind
import org.scalajs.ir.Position

import EmbeddedConstants._

object WasmExpressionBuilder {

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

  def generateIRBody(tree: IRTrees.Tree, resultType: IRTypes.Type)(implicit
      ctx: TypeDefinableWasmContext,
      fctx: WasmFunctionContext
  ): Unit = {
    val builder = new WasmExpressionBuilder(ctx, fctx)
    builder.genBody(tree, resultType)
  }

  def generateBlockStats[A](stats: List[IRTrees.Tree])(inner: => A)(implicit
      ctx: TypeDefinableWasmContext,
      fctx: WasmFunctionContext
  ): A = {
    val builder = new WasmExpressionBuilder(ctx, fctx)
    builder.genBlockStats(stats)(inner)
  }

  def genLoadJSNativeLoadSpec(fctx: WasmFunctionContext, loadSpec: IRTrees.JSNativeLoadSpec)(
      implicit ctx: TypeDefinableWasmContext
  ): IRTypes.Type = {
    import IRTrees.JSNativeLoadSpec._

    import fctx.instrs

    def genFollowPath(path: List[String]): Unit = {
      for (prop <- path) {
        instrs ++= ctx.getConstantStringInstr(prop)
        instrs += CALL(WasmFunctionName.jsSelect)
      }
    }

    loadSpec match {
      case Global(globalRef, path) =>
        instrs ++= ctx.getConstantStringInstr(globalRef)
        instrs += CALL(WasmFunctionName.jsGlobalRefGet)
        genFollowPath(path)
        IRTypes.AnyType
      case Import(module, path) =>
        instrs += GLOBAL_GET(ctx.getImportedModuleGlobal(module))
        genFollowPath(path)
        IRTypes.AnyType
      case ImportWithGlobalFallback(importSpec, globalSpec) =>
        genLoadJSNativeLoadSpec(fctx, importSpec)
    }
  }

  private val ObjectRef = IRTypes.ClassRef(IRNames.ObjectClass)
  private val BoxedStringRef = IRTypes.ClassRef(IRNames.BoxedStringClass)
  private val toStringMethodName = IRNames.MethodName("toString", Nil, BoxedStringRef)
  private val equalsMethodName = IRNames.MethodName("equals", List(ObjectRef), IRTypes.BooleanRef)
  private val compareToMethodName = IRNames.MethodName("compareTo", List(ObjectRef), IRTypes.IntRef)

  private val CharSequenceClass = IRNames.ClassName("java.lang.CharSequence")
  private val ComparableClass = IRNames.ClassName("java.lang.Comparable")
  private val JLNumberClass = IRNames.ClassName("java.lang.Number")
}

private class WasmExpressionBuilder private (
    ctx: TypeDefinableWasmContext,
    fctx: WasmFunctionContext
) {
  import WasmExpressionBuilder._

  private val instrs = fctx.instrs

  def genBody(tree: IRTrees.Tree, expectedType: IRTypes.Type): Unit =
    genTree(tree, expectedType)

  def genTrees(trees: List[IRTrees.Tree], expectedTypes: List[IRTypes.Type]): Unit = {
    for ((tree, expectedType) <- trees.zip(expectedTypes))
      genTree(tree, expectedType)
  }

  def genTreeAuto(tree: IRTrees.Tree): Unit =
    genTree(tree, tree.tpe)

  def genTree(tree: IRTrees.Tree, expectedType: IRTypes.Type): Unit = {
    val generatedType: IRTypes.Type = tree match {
      case t: IRTrees.Literal             => genLiteral(t, expectedType)
      case t: IRTrees.UnaryOp             => genUnaryOp(t)
      case t: IRTrees.BinaryOp            => genBinaryOp(t)
      case t: IRTrees.VarRef              => genVarRef(t)
      case t: IRTrees.LoadModule          => genLoadModule(t)
      case t: IRTrees.StoreModule         => genStoreModule(t)
      case t: IRTrees.This                => genThis(t)
      case t: IRTrees.ApplyStatically     => genApplyStatically(t)
      case t: IRTrees.Apply               => genApply(t)
      case t: IRTrees.ApplyStatic         => genApplyStatic(t)
      case t: IRTrees.ApplyDynamicImport  => genApplyDynamicImport(t)
      case t: IRTrees.IsInstanceOf        => genIsInstanceOf(t)
      case t: IRTrees.AsInstanceOf        => genAsInstanceOf(t)
      case t: IRTrees.GetClass            => genGetClass(t)
      case t: IRTrees.Block               => genBlock(t, expectedType)
      case t: IRTrees.Labeled             => unwinding.genLabeled(t, expectedType)
      case t: IRTrees.Return              => unwinding.genReturn(t)
      case t: IRTrees.Select              => genSelect(t)
      case t: IRTrees.SelectStatic        => genSelectStatic(t)
      case t: IRTrees.Assign              => genAssign(t)
      case t: IRTrees.VarDef              => genVarDef(t)
      case t: IRTrees.New                 => genNew(t)
      case t: IRTrees.If                  => genIf(t, expectedType)
      case t: IRTrees.While               => genWhile(t)
      case t: IRTrees.ForIn               => genForIn(t)
      case t: IRTrees.TryCatch            => genTryCatch(t, expectedType)
      case t: IRTrees.TryFinally          => unwinding.genTryFinally(t, expectedType)
      case t: IRTrees.Throw               => genThrow(t)
      case t: IRTrees.Match               => genMatch(t, expectedType)
      case t: IRTrees.Debugger            => IRTypes.NoType // ignore
      case t: IRTrees.Skip                => IRTypes.NoType
      case t: IRTrees.Clone               => genClone(t)
      case t: IRTrees.IdentityHashCode    => genIdentityHashCode(t)
      case t: IRTrees.WrapAsThrowable     => genWrapAsThrowable(t)
      case t: IRTrees.UnwrapFromThrowable => genUnwrapFromThrowable(t)

      // JavaScript expressions
      case t: IRTrees.JSNew                => genJSNew(t)
      case t: IRTrees.JSSelect             => genJSSelect(t)
      case t: IRTrees.JSFunctionApply      => genJSFunctionApply(t)
      case t: IRTrees.JSMethodApply        => genJSMethodApply(t)
      case t: IRTrees.JSImportCall         => genJSImportCall(t)
      case t: IRTrees.JSImportMeta         => genJSImportMeta(t)
      case t: IRTrees.LoadJSConstructor    => genLoadJSConstructor(t)
      case t: IRTrees.LoadJSModule         => genLoadJSModule(t)
      case t: IRTrees.SelectJSNativeMember => genSelectJSNativeMember(t)
      case t: IRTrees.JSDelete             => genJSDelete(t)
      case t: IRTrees.JSUnaryOp            => genJSUnaryOp(t)
      case t: IRTrees.JSBinaryOp           => genJSBinaryOp(t)
      case t: IRTrees.JSArrayConstr        => genJSArrayConstr(t)
      case t: IRTrees.JSObjectConstr       => genJSObjectConstr(t)
      case t: IRTrees.JSGlobalRef          => genJSGlobalRef(t)
      case t: IRTrees.JSTypeOfGlobalRef    => genJSTypeOfGlobalRef(t)
      case t: IRTrees.JSLinkingInfo        => genJSLinkingInfo(t)
      case t: IRTrees.Closure              => genClosure(t)

      // array
      case t: IRTrees.ArrayLength => genArrayLength(t)
      case t: IRTrees.NewArray    => genNewArray(t)
      case t: IRTrees.ArraySelect => genArraySelect(t)
      case t: IRTrees.ArrayValue  => genArrayValue(t)

      // Non-native JS classes
      case t: IRTrees.CreateJSClass     => genCreateJSClass(t)
      case t: IRTrees.JSPrivateSelect   => genJSPrivateSelect(t)
      case t: IRTrees.JSSuperSelect     => genJSSuperSelect(t)
      case t: IRTrees.JSSuperMethodCall => genJSSuperMethodCall(t)
      case t: IRTrees.JSNewTarget       => genJSNewTarget(t)

      case _: IRTrees.RecordSelect | _: IRTrees.RecordValue | _: IRTrees.Transient |
          _: IRTrees.JSSuperConstructorCall =>
        throw new AssertionError(s"Invalid tree: $tree")
    }

    genAdapt(generatedType, expectedType)
  }

  private def genAdapt(generatedType: IRTypes.Type, expectedType: IRTypes.Type): Unit = {
    (generatedType, expectedType) match {
      case _ if generatedType == expectedType =>
        ()
      case (IRTypes.NothingType, _) =>
        ()
      case (_, IRTypes.NoType) =>
        instrs += DROP
      case (primType: IRTypes.PrimTypeWithRef, _) =>
        // box
        primType match {
          case IRTypes.NullType =>
            ()
          case IRTypes.CharType =>
            /* `char` and `long` are opaque to JS in the Scala.js semantics.
             * We implement them with real Wasm classes following the correct
             * vtable. Upcasting wraps a primitive into the corresponding class.
             */
            genBox(IRTypes.CharType, SpecialNames.CharBoxClass)
          case IRTypes.LongType =>
            genBox(IRTypes.LongType, SpecialNames.LongBoxClass)
          case IRTypes.NoType | IRTypes.NothingType =>
            throw new AssertionError(s"Unexpected adaptation from $primType to $expectedType")
          case _ =>
            /* Calls a `bX` helper. Most of them are of the form
             *   bX: (x) => x
             * at the JavaScript level, but with a primType->anyref Wasm type.
             * For example, for `IntType`, `bI` has type `i32 -> anyref`. This
             * asks the JS host to turn a primitive `i32` into its generic
             * representation, which we can store in an `anyref`.
             */
            instrs += CALL(WasmFunctionName.box(primType.primRef))
        }
      case _ =>
        ()
    }
  }

  private def genAssign(t: IRTrees.Assign): IRTypes.Type = {
    t.lhs match {
      case sel: IRTrees.Select =>
        val className = sel.field.name.className
        val classInfo = ctx.getClassInfo(className)

        // For Select, the receiver can never be a hijacked class, so we can use genTreeAuto
        genTreeAuto(sel.qualifier)

        if (!classInfo.hasInstances) {
          /* The field may not exist in that case, and we cannot look it up.
           * However we necessarily have a `null` receiver if we reach this
           * point, so we can trap as NPE.
           */
          instrs += UNREACHABLE
        } else {
          val fieldName = WasmFieldName.forClassInstanceField(sel.field.name)
          val idx = ctx.getClassInfo(className).getFieldIdx(sel.field.name)

          genTree(t.rhs, t.lhs.tpe)
          instrs += STRUCT_SET(WasmStructTypeName.forClass(className), idx)
        }

      case sel: IRTrees.SelectStatic =>
        genTree(t.rhs, sel.tpe)
        instrs += GLOBAL_SET(Names.WasmGlobalName.forStaticField(sel.field.name))

      case sel: IRTrees.ArraySelect =>
        genTreeAuto(sel.array)
        sel.array.tpe match {
          case IRTypes.ArrayType(arrayTypeRef) =>
            // Get the underlying array; implicit trap on null
            instrs += STRUCT_GET(
              WasmStructTypeName.forArrayClass(arrayTypeRef),
              WasmFieldIdx.uniqueRegularField
            )
            genTree(sel.index, IRTypes.IntType)
            genTree(t.rhs, sel.tpe)
            instrs += ARRAY_SET(WasmArrayTypeName.underlyingOf(arrayTypeRef))
          case IRTypes.NothingType =>
            // unreachable
            ()
          case IRTypes.NullType =>
            instrs += UNREACHABLE
          case _ =>
            throw new IllegalArgumentException(
              s"ArraySelect.array must be an array type, but has type ${sel.array.tpe}"
            )
        }

      case sel: IRTrees.JSPrivateSelect =>
        genTree(sel.qualifier, IRTypes.AnyType)
        instrs += GLOBAL_GET(WasmGlobalName.forJSPrivateField(sel.field.name))
        genTree(t.rhs, IRTypes.AnyType)
        instrs += CALL(WasmFunctionName.jsSelectSet)

      case assign: IRTrees.JSSelect =>
        genTree(assign.qualifier, IRTypes.AnyType)
        genTree(assign.item, IRTypes.AnyType)
        genTree(t.rhs, IRTypes.AnyType)
        instrs += CALL(WasmFunctionName.jsSelectSet)

      case assign: IRTrees.JSSuperSelect =>
        genTree(assign.superClass, IRTypes.AnyType)
        genTree(assign.receiver, IRTypes.AnyType)
        genTree(assign.item, IRTypes.AnyType)
        genTree(t.rhs, IRTypes.AnyType)
        instrs += CALL(WasmFunctionName.jsSuperSet)

      case assign: IRTrees.JSGlobalRef =>
        instrs ++= ctx.getConstantStringInstr(assign.name)
        genTree(t.rhs, IRTypes.AnyType)
        instrs += CALL(WasmFunctionName.jsGlobalRefSet)

      case ref: IRTrees.VarRef =>
        import WasmFunctionContext.VarStorage
        fctx.lookupLocal(ref.ident.name) match {
          case VarStorage.Local(local) =>
            genTree(t.rhs, t.lhs.tpe)
            instrs += LOCAL_SET(local)
          case VarStorage.StructField(structLocal, structTypeName, fieldIdx) =>
            instrs += LOCAL_GET(structLocal)
            genTree(t.rhs, t.lhs.tpe)
            instrs += STRUCT_SET(structTypeName, fieldIdx)
        }

      case assign: IRTrees.RecordSelect =>
        throw new AssertionError(s"Invalid tree: $t")
    }

    IRTypes.NoType
  }

  private def genApply(t: IRTrees.Apply): IRTypes.Type = {
    t.receiver.tpe match {
      case IRTypes.NothingType =>
        genTree(t.receiver, IRTypes.NothingType)
        // nothing else to do; this is unreachable
        IRTypes.NothingType

      case IRTypes.NullType =>
        genTree(t.receiver, IRTypes.NullType)
        instrs += UNREACHABLE // trap
        IRTypes.NothingType

      case prim: IRTypes.PrimType =>
        // statically resolved call with non-null argument
        val receiverClassName = IRTypes.PrimTypeToBoxedClass(prim)
        genApplyStatically(
          IRTrees.ApplyStatically(t.flags, t.receiver, receiverClassName, t.method, t.args)(t.tpe)(
            t.pos
          )
        )

      case IRTypes.ClassType(className) if IRNames.HijackedClasses.contains(className) =>
        // statically resolved call with maybe-null argument
        genApplyStatically(
          IRTrees.ApplyStatically(t.flags, t.receiver, className, t.method, t.args)(t.tpe)(t.pos)
        )

      case _ if t.method.name.isReflectiveProxy =>
        genReflectiveCall(t)

      case _ =>
        genApplyNonPrim(t)
    }
  }

  private def genReflectiveCall(t: IRTrees.Apply): IRTypes.Type = {
    assert(t.method.name.isReflectiveProxy)
    val receiverLocalForDispatch =
      fctx.addSyntheticLocal(Types.WasmRefType.any)

    val proxyId = ctx.getReflectiveProxyId(t.method.name.nameString)
    val receiverType = TypeTransformer.makeReceiverType
    val paramTys = t.method.name.paramTypeRefs.map(TypeTransformer.transformTypeRef(_)(ctx))
    val sig = WasmFunctionSignature(
      receiverType +: paramTys,
      TypeTransformer.transformResultType(IRTypes.AnyType)(ctx)
    )
    val funcTypeName = ctx.addFunctionTypeInMainRecType(sig)

    fctx.block(sig.results) { done =>
      fctx.block(Types.WasmRefType.any) { labelNotOurObject =>
        // arguments
        genTree(t.receiver, IRTypes.AnyType)
        instrs += REF_AS_NOT_NULL
        instrs += LOCAL_TEE(receiverLocalForDispatch)
        genArgs(t.args, t.method.name)

        // Looks up the method to be (reflectively) called
        instrs += LOCAL_GET(receiverLocalForDispatch)
        instrs += BR_ON_CAST_FAIL(
          labelNotOurObject,
          Types.WasmRefType.any,
          Types.WasmRefType(Types.WasmHeapType.ObjectType)
        )
        instrs += STRUCT_GET(
          WasmStructTypeName.forClass(IRNames.ObjectClass),
          WasmFieldIdx.vtable
        )
        instrs += I32_CONST(proxyId)
        // `searchReflectiveProxy`: [typeData, i32] -> [(ref func)]
        instrs += CALL(WasmFunctionName.searchReflectiveProxy)

        instrs += REF_CAST(Types.WasmRefType(Types.WasmHeapType(funcTypeName)))
        instrs += CALL_REF(funcTypeName)
        instrs += BR(done)
      } // labelNotFound
      instrs += UNREACHABLE
      // TODO? reflective call on primitive types
      t.tpe
    }
    // done
  }

  /** Generates the code an `Apply` call where the receiver's type is not statically known to be a
    * primitive or hijacked class.
    *
    * In that case, there is always at least a vtable/itable-based dispatch. It may also contain
    * primitive-based dispatch if the receiver's type is an ancestor of a hijacked class.
    */
  private def genApplyNonPrim(t: IRTrees.Apply): IRTypes.Type = {
    implicit val pos: Position = t.pos

    val receiverClassName = t.receiver.tpe match {
      case ClassType(className) => className
      case IRTypes.AnyType      => IRNames.ObjectClass
      case IRTypes.ArrayType(_) => IRNames.ObjectClass
      case _                    => throw new Error(s"Invalid receiver type ${t.receiver.tpe}")
    }
    val receiverClassInfo = ctx.getClassInfo(receiverClassName)

    /* Similar to transformType(t.receiver.tpe), but:
     * - it is non-null,
     * - ancestors of hijacked classes are not treated specially,
     * - array types are treated as j.l.Object.
     *
     * This is used in the code paths where we have already ruled out `null`
     * values and primitive values (that implement hijacked classes).
     */
    val heapTypeForDispatch: Types.WasmHeapType = {
      if (receiverClassInfo.isInterface)
        Types.WasmHeapType.ObjectType
      else
        Types.WasmHeapType(Names.WasmTypeName.WasmStructTypeName.forClass(receiverClassName))
    }

    // A local for a copy of the receiver that we will use to resolve dispatch
    val receiverLocalForDispatch =
      fctx.addSyntheticLocal(Types.WasmRefType(heapTypeForDispatch))

    /* Gen loading of the receiver and check that it is non-null.
     * After this codegen, the non-null receiver is on the stack.
     */
    def genReceiverNotNull(): Unit = {
      genTreeAuto(t.receiver)
      instrs += REF_AS_NOT_NULL
    }

    /* Generates a resolved call to a method of a hijacked class.
     * Before this code gen, the stack must contain the receiver and the args.
     * After this code gen, the stack contains the result.
     */
    def genHijackedClassCall(hijackedClass: IRNames.ClassName): Unit = {
      val funcName = Names.WasmFunctionName(
        IRTrees.MemberNamespace.Public,
        hijackedClass,
        t.method.name
      )
      instrs += CALL(funcName)
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
      instrs += UNREACHABLE // NPE
    } else if (!receiverClassInfo.isAncestorOfHijackedClass) {
      // Standard dispatch codegen
      genReceiverNotNull()
      instrs += LOCAL_TEE(receiverLocalForDispatch)
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

      fctx.block(resultTyp) { labelDone =>
        def pushArgs(argsLocals: List[WasmLocalName]): Unit =
          argsLocals.foreach(argLocal => instrs += LOCAL_GET(argLocal))

        // First try the case where the value is one of our objects
        val argsLocals = fctx.block(Types.WasmRefType.any) { labelNotOurObject =>
          // Load receiver and arguments and store them in temporary variables
          genReceiverNotNull()
          val argsLocals = if (t.args.isEmpty) {
            /* When there are no arguments, we can leave the receiver directly on
             * the stack instead of going through a local. We will still need a
             * local for the table-based dispatch, though.
             */
            Nil
          } else {
            val receiverLocal = fctx.addSyntheticLocal(Types.WasmRefType.any)

            instrs += LOCAL_SET(receiverLocal)
            val argsLocals: List[WasmLocalName] =
              for ((arg, typeRef) <- t.args.zip(t.method.name.paramTypeRefs)) yield {
                val typ = ctx.inferTypeFromTypeRef(typeRef)
                genTree(arg, typ)
                val localName = fctx.addSyntheticLocal(TypeTransformer.transformType(typ)(ctx))
                instrs += LOCAL_SET(localName)
                localName
              }
            instrs += LOCAL_GET(receiverLocal)
            argsLocals
          }

          instrs += BR_ON_CAST_FAIL(
            labelNotOurObject,
            Types.WasmRefType.any,
            Types.WasmRefType(heapTypeForDispatch)
          )
          instrs += LOCAL_TEE(receiverLocalForDispatch)
          pushArgs(argsLocals)
          genTableDispatch(receiverClassInfo, t.method.name, receiverLocalForDispatch)
          instrs += BR(labelDone)

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
          instrs += CALL(WasmFunctionName.jsValueToString)
        } else if (receiverClassName == JLNumberClass) {
          // the value must be a `number`, hence we can unbox to `double`
          genUnbox(IRTypes.DoubleType)
          pushArgs(argsLocals)
          genHijackedClassCall(IRNames.BoxedDoubleClass)
        } else if (receiverClassName == CharSequenceClass) {
          // the value must be a `string`; it already has the right type
          pushArgs(argsLocals)
          genHijackedClassCall(IRNames.BoxedStringClass)
        } else if (t.method.name == compareToMethodName) {
          /* The only method of jl.Comparable. Here the value can be a boolean,
           * a number or a string. We use `jsValueType` to dispatch to Wasm-side
           * implementations because they have to perform casts on their arguments.
           */
          assert(argsLocals.size == 1)

          val receiverLocal = fctx.addSyntheticLocal(Types.WasmRefType.any)
          instrs += LOCAL_TEE(receiverLocal)

          val jsValueTypeLocal = fctx.addSyntheticLocal(Types.WasmInt32)
          instrs += CALL(WasmFunctionName.jsValueType)
          instrs += LOCAL_TEE(jsValueTypeLocal)

          import wasm.wasm4s.{WasmFunctionSignature => Sig}
          fctx.switch(Sig(List(Types.WasmInt32), Nil), Sig(Nil, List(Types.WasmInt32))) { () =>
            // scrutinee is already on the stack
          }(
            // case JSValueTypeFalse | JSValueTypeTrue =>
            List(JSValueTypeFalse, JSValueTypeTrue) -> { () =>
              // the jsValueTypeLocal is the boolean value, thanks to the chosen encoding
              instrs += LOCAL_GET(jsValueTypeLocal)
              pushArgs(argsLocals)
              genHijackedClassCall(IRNames.BoxedBooleanClass)
            },
            // case JSValueTypeString =>
            List(JSValueTypeString) -> { () =>
              instrs += LOCAL_GET(receiverLocal)
              // no need to unbox for string
              pushArgs(argsLocals)
              genHijackedClassCall(IRNames.BoxedStringClass)
            }
          ) { () =>
            // case _ (JSValueTypeNumber) =>
            instrs += LOCAL_GET(receiverLocal)
            genUnbox(IRTypes.DoubleType)
            pushArgs(argsLocals)
            genHijackedClassCall(IRNames.BoxedDoubleClass)
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
              instrs += CALL(WasmFunctionName.identityHashCode)
            case `equalsMethodName` =>
              instrs += CALL(WasmFunctionName.is)
            case _ =>
              genHijackedClassCall(IRNames.ObjectClass)
          }
        }
      } // end block labelDone
    }

    if (t.tpe == IRTypes.NothingType)
      instrs += UNREACHABLE

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
      receiverClassInfo: WasmContext.WasmClassInfo,
      methodName: IRNames.MethodName,
      receiverLocalForDispatch: WasmLocalName
  ): Unit = {
    // Generates an itable-based dispatch.
    def genITableDispatch(): Unit = {
      val itableIdx = ctx.getItableIdx(receiverClassInfo.name)
      val methodIdx =
        receiverClassInfo.methods.indexWhere(meth => meth.name.simpleName == methodName.nameString)
      if (methodIdx < 0)
        throw new Error(
          s"Method ${methodName.nameString} not found in class ${receiverClassInfo.name}"
        )

      val methodInfo = receiverClassInfo.methods(methodIdx)

      instrs += LOCAL_GET(receiverLocalForDispatch)
      instrs += STRUCT_GET(
        // receiver type should be upcasted into `Object` if it's interface
        // by TypeTransformer#transformType
        WasmStructTypeName.forClass(IRNames.ObjectClass),
        WasmFieldIdx.itables
      )
      instrs += I32_CONST(itableIdx)
      instrs += ARRAY_GET(WasmArrayTypeName.itables)
      instrs += REF_CAST(Types.WasmRefType(WasmStructTypeName.forITable(receiverClassInfo.name)))
      instrs += STRUCT_GET(
        WasmStructTypeName.forITable(receiverClassInfo.name),
        WasmFieldIdx(methodIdx)
      )
      instrs += CALL_REF(methodInfo.toWasmFunctionType()(ctx))
    }

    // Generates a vtable-based dispatch.
    def genVTableDispatch(): Unit = {
      val receiverClassName = receiverClassInfo.name

      val (methodIdx, info) = ctx
        .calculateVtableType(receiverClassName)
        .resolveWithIdx(
          WasmFunctionName(
            IRTrees.MemberNamespace.Public,
            receiverClassName,
            methodName
          )
        )

      // // push args to the stacks
      // local.get $this ;; for accessing funcref
      // local.get $this ;; for accessing vtable
      // struct.get $classType 0 ;; get vtable
      // struct.get $vtableType $methodIdx ;; get funcref
      // call.ref (type $funcType) ;; call funcref
      instrs += LOCAL_GET(receiverLocalForDispatch)
      instrs += REF_CAST(Types.WasmRefType(WasmStructTypeName.forClass(receiverClassName)))
      instrs += STRUCT_GET(
        WasmStructTypeName.forClass(receiverClassName),
        WasmFieldIdx.vtable
      )
      instrs += STRUCT_GET(
        WasmStructTypeName.forVTable(receiverClassName),
        WasmFieldIdx(WasmStructType.typeDataFieldCount(ctx) + methodIdx)
      )
      instrs += CALL_REF(info.toWasmFunctionType()(ctx))
    }

    if (receiverClassInfo.isInterface)
      genITableDispatch()
    else
      genVTableDispatch()
  }

  private def genApplyStatically(t: IRTrees.ApplyStatically): IRTypes.Type = {
    t.receiver.tpe match {
      case IRTypes.NothingType =>
        genTree(t.receiver, IRTypes.NothingType)
        // nothing else to do; this is unreachable
        IRTypes.NothingType

      case IRTypes.NullType =>
        genTree(t.receiver, IRTypes.NullType)
        instrs += UNREACHABLE // trap
        IRTypes.NothingType

      case _ =>
        val namespace = IRTrees.MemberNamespace.forNonStaticCall(t.flags)
        val targetClassName =
          ctx.getClassInfo(t.className).resolvePublicMethod(namespace, t.method.name)(ctx)

        IRTypes.BoxedClassToPrimType.get(targetClassName) match {
          case None =>
            genTree(t.receiver, IRTypes.ClassType(targetClassName))
            instrs += REF_AS_NOT_NULL

          case Some(primReceiverType) =>
            if (t.receiver.tpe == primReceiverType) {
              genTreeAuto(t.receiver)
            } else {
              genTree(t.receiver, IRTypes.AnyType)
              instrs += REF_AS_NOT_NULL
              genUnbox(primReceiverType)(t.pos)
            }
        }

        genArgs(t.args, t.method.name)

        val funcName = Names.WasmFunctionName(namespace, targetClassName, t.method.name)
        instrs += CALL(funcName)
        if (t.tpe == IRTypes.NothingType)
          instrs += UNREACHABLE
        t.tpe
    }
  }

  private def genApplyStatic(tree: IRTrees.ApplyStatic): IRTypes.Type = {
    genArgs(tree.args, tree.method.name)
    val namespace = IRTrees.MemberNamespace.forStaticCall(tree.flags)
    val funcName = Names.WasmFunctionName(namespace, tree.className, tree.method.name)
    instrs += CALL(funcName)
    if (tree.tpe == IRTypes.NothingType)
      instrs += UNREACHABLE
    tree.tpe
  }

  private def genApplyDynamicImport(tree: IRTrees.ApplyDynamicImport): IRTypes.Type = {
    // As long as we do not support multiple modules, this cannot happen
    throw new AssertionError(
      s"Unexpected $tree at ${tree.pos}; multiple modules are not supported yet"
    )
  }

  private def genArgs(args: List[IRTrees.Tree], methodName: IRNames.MethodName): Unit = {
    for ((arg, paramTypeRef) <- args.zip(methodName.paramTypeRefs)) {
      val paramType = ctx.inferTypeFromTypeRef(paramTypeRef)
      genTree(arg, paramType)
    }
  }

  private def genLiteral(l: IRTrees.Literal, expectedType: IRTypes.Type): IRTypes.Type = {
    if (expectedType == IRTypes.NoType) {
      /* Since all primitives are pure, we can always get rid of them.
       * This is mostly useful for the argument of `Return` nodes that target a
       * `Labeled` in statement position, since they must have a non-`void`
       * type in the IR but they get a `void` expected type.
       */
      expectedType
    } else {
      l match {
        case IRTrees.BooleanLiteral(v) => instrs += WasmInstr.I32_CONST(if (v) 1 else 0)
        case IRTrees.ByteLiteral(v)    => instrs += WasmInstr.I32_CONST(v)
        case IRTrees.ShortLiteral(v)   => instrs += WasmInstr.I32_CONST(v)
        case IRTrees.IntLiteral(v)     => instrs += WasmInstr.I32_CONST(v)
        case IRTrees.CharLiteral(v)    => instrs += WasmInstr.I32_CONST(v)
        case IRTrees.LongLiteral(v)    => instrs += WasmInstr.I64_CONST(v)
        case IRTrees.FloatLiteral(v)   => instrs += WasmInstr.F32_CONST(v)
        case IRTrees.DoubleLiteral(v)  => instrs += WasmInstr.F64_CONST(v)

        case v: IRTrees.Undefined =>
          instrs += CALL(WasmFunctionName.undef)
        case v: IRTrees.Null =>
          instrs += WasmInstr.REF_NULL(Types.WasmHeapType.None)

        case v: IRTrees.StringLiteral =>
          instrs ++= ctx.getConstantStringInstr(v.value)

        case v: IRTrees.ClassOf =>
          v.typeRef match {
            case typeRef: IRTypes.NonArrayTypeRef =>
              genClassOfFromTypeData(getNonArrayTypeDataInstr(typeRef))

            case typeRef: IRTypes.ArrayTypeRef =>
              val typeDataType = Types.WasmRefType(WasmStructTypeName.typeData)
              val typeDataLocal = fctx.addSyntheticLocal(typeDataType)

              genLoadArrayTypeData(typeRef)
              instrs += LOCAL_SET(typeDataLocal)
              genClassOfFromTypeData(LOCAL_GET(typeDataLocal))
          }
      }

      l.tpe
    }
  }

  private def getNonArrayTypeDataInstr(typeRef: IRTypes.NonArrayTypeRef): WasmInstr =
    GLOBAL_GET(WasmGlobalName.forVTable(typeRef))

  private def genLoadArrayTypeData(arrayTypeRef: IRTypes.ArrayTypeRef): Unit = {
    instrs += getNonArrayTypeDataInstr(arrayTypeRef.base)
    instrs += I32_CONST(arrayTypeRef.dimensions)
    instrs += CALL(WasmFunctionName.arrayTypeData)
  }

  private def genClassOfFromTypeData(loadTypeDataInstr: WasmInstr): Unit = {
    fctx.block(Types.WasmRefType(Types.WasmHeapType.ClassType)) { nonNullLabel =>
      // fast path first
      instrs += loadTypeDataInstr
      instrs += STRUCT_GET(WasmStructTypeName.typeData, WasmFieldIdx.typeData.classOfIdx)
      instrs += BR_ON_NON_NULL(nonNullLabel)
      // slow path
      instrs += loadTypeDataInstr
      instrs += CALL(WasmFunctionName.createClassOf)
    }
  }

  private def genSelect(sel: IRTrees.Select): IRTypes.Type = {
    val className = sel.field.name.className
    val classInfo = ctx.getClassInfo(className)

    // For Select, the receiver can never be a hijacked class, so we can use genTreeAuto
    genTreeAuto(sel.qualifier)

    if (!classInfo.hasInstances) {
      /* The field may not exist in that case, and we cannot look it up.
       * However we necessarily have a `null` receiver if we reach this point,
       * so we can trap as NPE.
       */
      instrs += UNREACHABLE
    } else {
      val fieldName = WasmFieldName.forClassInstanceField(sel.field.name)
      val idx = classInfo.getFieldIdx(sel.field.name)

      instrs += STRUCT_GET(WasmStructTypeName.forClass(className), idx)
    }

    sel.tpe
  }

  private def genSelectStatic(tree: IRTrees.SelectStatic): IRTypes.Type = {
    instrs += GLOBAL_GET(Names.WasmGlobalName.forStaticField(tree.field.name))
    tree.tpe
  }

  private def genStoreModule(t: IRTrees.StoreModule): IRTypes.Type = {
    val className = fctx.enclosingClassName.getOrElse {
      throw new AssertionError(s"Cannot emit $t at ${t.pos} without enclosing class name")
    }
    genTreeAuto(IRTrees.This()(IRTypes.ClassType(className))(t.pos))
    instrs += GLOBAL_SET(WasmGlobalName.forModuleInstance(className))
    IRTypes.NoType
  }

  /** Push module class instance to the stack.
    *
    * see: WasmBuilder.genLoadModuleFunc
    */
  private def genLoadModule(t: IRTrees.LoadModule): IRTypes.Type = {
    instrs += CALL(Names.WasmFunctionName.loadModule(t.className))
    t.tpe
  }

  private def genUnaryOp(unary: IRTrees.UnaryOp): IRTypes.Type = {
    import IRTrees.UnaryOp._

    genTreeAuto(unary.lhs)

    (unary.op: @switch) match {
      case Boolean_! =>
        instrs += I32_CONST(1)
        instrs += I32_XOR

      // Widening conversions
      case CharToInt | ByteToInt | ShortToInt =>
        () // these are no-ops because they are all represented as i32's with the right mathematical value
      case IntToLong =>
        instrs += I64_EXTEND_I32_S
      case IntToDouble =>
        instrs += F64_CONVERT_I32_S
      case FloatToDouble =>
        instrs += F64_PROMOTE_F32

      // Narrowing conversions
      case IntToChar =>
        instrs += I32_CONST(0xFFFF)
        instrs += I32_AND
      case IntToByte =>
        instrs += I32_EXTEND8_S
      case IntToShort =>
        instrs += I32_EXTEND16_S
      case LongToInt =>
        instrs += I32_WRAP_I64
      case DoubleToInt =>
        instrs += I32_TRUNC_SAT_F64_S
      case DoubleToFloat =>
        instrs += F32_DEMOTE_F64

      // Long <-> Double (neither widening nor narrowing)
      case LongToDouble =>
        instrs += F64_CONVERT_I64_S
      case DoubleToLong =>
        instrs += I64_TRUNC_SAT_F64_S

      // Long -> Float (neither widening nor narrowing), introduced in 1.6
      case LongToFloat =>
        instrs += F32_CONVERT_I64_S

      // String.length, introduced in 1.11
      case String_length =>
        instrs += CALL(WasmFunctionName.stringLength)
    }

    unary.tpe
  }

  private def genBinaryOp(binary: IRTrees.BinaryOp): IRTypes.Type = {
    import IRTrees.BinaryOp

    def genLongShiftOp(shiftInstr: WasmInstr): IRTypes.Type = {
      genTree(binary.lhs, IRTypes.LongType)
      genTree(binary.rhs, IRTypes.IntType)
      instrs += I64_EXTEND_I32_S
      instrs += shiftInstr
      IRTypes.LongType
    }

    binary.op match {
      case BinaryOp.=== | BinaryOp.!== => genEq(binary)

      case BinaryOp.String_+ => genStringConcat(binary.lhs, binary.rhs)

      case BinaryOp.Long_<<  => genLongShiftOp(I64_SHL)
      case BinaryOp.Long_>>> => genLongShiftOp(I64_SHR_U)
      case BinaryOp.Long_>>  => genLongShiftOp(I64_SHR_S)

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
        genTree(binary.lhs, IRTypes.FloatType)
        instrs += F64_PROMOTE_F32
        genTree(binary.rhs, IRTypes.FloatType)
        instrs += F64_PROMOTE_F32
        instrs += CALL(WasmFunctionName.fmod)
        instrs += F32_DEMOTE_F64
        IRTypes.FloatType
      case BinaryOp.Double_% =>
        genTree(binary.lhs, IRTypes.DoubleType)
        genTree(binary.rhs, IRTypes.DoubleType)
        instrs += CALL(WasmFunctionName.fmod)
        IRTypes.DoubleType

      // New in 1.11
      case BinaryOp.String_charAt =>
        genTree(binary.lhs, IRTypes.StringType) // push the string
        genTree(binary.rhs, IRTypes.IntType) // push the index
        instrs += CALL(WasmFunctionName.stringCharAt)
        IRTypes.CharType

      // Check division by zero
      // (Int|Long).MinValue / -1 = (Int|Long).MinValue because of overflow
      case BinaryOp.Int_/ | BinaryOp.Long_/ | BinaryOp.Int_% | BinaryOp.Long_% =>
        implicit val noPos = Position.NoPosition
        val divisionByZeroEx = IRTrees.Throw(
          IRTrees.New(
            IRNames.ArithmeticExceptionClass,
            IRTrees.MethodIdent(
              IRNames.MethodName.constructor(List(IRTypes.ClassRef(IRNames.BoxedStringClass)))
            ),
            List(IRTrees.StringLiteral("/ by zero "))
          )
        )
        val resType = TypeTransformer.transformType(binary.tpe)(ctx)

        val lhs = fctx.addSyntheticLocal(TypeTransformer.transformType(binary.lhs.tpe)(ctx))
        val rhs = fctx.addSyntheticLocal(TypeTransformer.transformType(binary.rhs.tpe)(ctx))
        genTreeAuto(binary.lhs)
        instrs += LOCAL_SET(lhs)
        genTreeAuto(binary.rhs)
        instrs += LOCAL_SET(rhs)

        fctx.block(resType) { done =>
          fctx.block() { default =>
            fctx.block() { divisionByZero =>
              instrs += LOCAL_GET(rhs)
              binary.op match {
                case BinaryOp.Int_/ | BinaryOp.Int_%   => instrs += I32_EQZ
                case BinaryOp.Long_/ | BinaryOp.Long_% => instrs += I64_EQZ
              }
              instrs += BR_IF(divisionByZero)

              // Check overflow for division
              if (binary.op == BinaryOp.Int_/ || binary.op == BinaryOp.Long_/) {
                fctx.block() { overflow =>
                  instrs += LOCAL_GET(rhs)
                  if (binary.op == BinaryOp.Int_/) instrs ++= List(I32_CONST(-1), I32_EQ)
                  else instrs ++= List(I64_CONST(-1), I64_EQ)
                  fctx.ifThen() { // if (rhs == -1)
                    instrs += LOCAL_GET(lhs)
                    if (binary.op == BinaryOp.Int_/)
                      instrs ++= List(I32_CONST(Int.MinValue), I32_EQ)
                    else instrs ++= List(I64_CONST(Long.MinValue), I64_EQ)
                    instrs += BR_IF(overflow)
                  }
                  instrs += BR(default)
                }
                // overflow
                if (binary.op == BinaryOp.Int_/) instrs += I32_CONST(Int.MinValue)
                else instrs += I64_CONST(Long.MinValue)
                instrs += BR(done)
              }

              // remainder
              instrs += BR(default)
            }
            // division by zero
            genThrow(divisionByZeroEx)
          }
          // default
          instrs += LOCAL_GET(lhs)
          instrs += LOCAL_GET(rhs)
          instrs +=
            (binary.op match {
              case BinaryOp.Int_/  => I32_DIV_S
              case BinaryOp.Int_%  => I32_REM_S
              case BinaryOp.Long_/ => I64_DIV_S
              case BinaryOp.Long_% => I64_REM_S
            })
          binary.tpe
        }

      case _ => genElementaryBinaryOp(binary)
    }
  }

  private def genEq(binary: IRTrees.BinaryOp): IRTypes.Type = {
    // TODO Optimize this when the operands have a better type than `any`
    genTree(binary.lhs, IRTypes.AnyType)
    genTree(binary.rhs, IRTypes.AnyType)
    instrs += CALL(WasmFunctionName.is)

    if (binary.op == IRTrees.BinaryOp.!==) {
      instrs += I32_CONST(1)
      instrs += I32_XOR
    }

    IRTypes.BooleanType
  }

  private def genElementaryBinaryOp(binary: IRTrees.BinaryOp): IRTypes.Type = {
    import IRTrees.BinaryOp
    genTreeAuto(binary.lhs)
    genTreeAuto(binary.rhs)
    val operation = binary.op match {
      case BinaryOp.Boolean_== => I32_EQ
      case BinaryOp.Boolean_!= => I32_NE
      case BinaryOp.Boolean_|  => I32_OR
      case BinaryOp.Boolean_&  => I32_AND

      case BinaryOp.Int_+   => I32_ADD
      case BinaryOp.Int_-   => I32_SUB
      case BinaryOp.Int_*   => I32_MUL
      case BinaryOp.Int_/   => I32_DIV_S // signed division
      case BinaryOp.Int_%   => I32_REM_S // signed remainder
      case BinaryOp.Int_|   => I32_OR
      case BinaryOp.Int_&   => I32_AND
      case BinaryOp.Int_^   => I32_XOR
      case BinaryOp.Int_<<  => I32_SHL
      case BinaryOp.Int_>>> => I32_SHR_U
      case BinaryOp.Int_>>  => I32_SHR_S
      case BinaryOp.Int_==  => I32_EQ
      case BinaryOp.Int_!=  => I32_NE
      case BinaryOp.Int_<   => I32_LT_S
      case BinaryOp.Int_<=  => I32_LE_S
      case BinaryOp.Int_>   => I32_GT_S
      case BinaryOp.Int_>=  => I32_GE_S

      case BinaryOp.Long_+ => I64_ADD
      case BinaryOp.Long_- => I64_SUB
      case BinaryOp.Long_* => I64_MUL
      case BinaryOp.Long_/ => I64_DIV_S
      case BinaryOp.Long_% => I64_REM_S
      case BinaryOp.Long_| => I64_OR
      case BinaryOp.Long_& => I64_AND
      case BinaryOp.Long_^ => I64_XOR

      case BinaryOp.Long_== => I64_EQ
      case BinaryOp.Long_!= => I64_NE
      case BinaryOp.Long_<  => I64_LT_S
      case BinaryOp.Long_<= => I64_LE_S
      case BinaryOp.Long_>  => I64_GT_S
      case BinaryOp.Long_>= => I64_GE_S

      case BinaryOp.Float_+ => F32_ADD
      case BinaryOp.Float_- => F32_SUB
      case BinaryOp.Float_* => F32_MUL
      case BinaryOp.Float_/ => F32_DIV

      case BinaryOp.Double_+ => F64_ADD
      case BinaryOp.Double_- => F64_SUB
      case BinaryOp.Double_* => F64_MUL
      case BinaryOp.Double_/ => F64_DIV

      case BinaryOp.Double_== => F64_EQ
      case BinaryOp.Double_!= => F64_NE
      case BinaryOp.Double_<  => F64_LT
      case BinaryOp.Double_<= => F64_LE
      case BinaryOp.Double_>  => F64_GT
      case BinaryOp.Double_>= => F64_GE
    }
    instrs += operation
    binary.tpe
  }

  private def genStringConcat(lhs: IRTrees.Tree, rhs: IRTrees.Tree): IRTypes.Type = {
    val wasmStringType = Types.WasmRefType.any

    def genToString(tree: IRTrees.Tree): Unit = {
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
          fctx.addSyntheticLocal(Types.WasmRefType(Types.WasmHeapType.ObjectType))

        val objectClassInfo = ctx.getClassInfo(IRNames.ObjectClass)

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

          fctx.block(Types.WasmRefType.any) { labelDone =>
            fctx.block() { labelIsNull =>
              genTreeAuto(tree)
              instrs += BR_ON_NULL(labelIsNull)
              instrs += LOCAL_TEE(receiverLocalForDispatch)
              genTableDispatch(objectClassInfo, toStringMethodName, receiverLocalForDispatch)
              instrs += BR_ON_NON_NULL(labelDone)
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

          fctx.block(Types.WasmRefType.any) { labelDone =>
            // First try the case where the value is one of our objects
            fctx.block(Types.WasmRefType.anyref) { labelNotOurObject =>
              // Load receiver
              genTreeAuto(tree)

              instrs += BR_ON_CAST_FAIL(
                labelNotOurObject,
                Types.WasmRefType.anyref,
                Types.WasmRefType(Types.WasmHeapType.ObjectType)
              )
              instrs += LOCAL_TEE(receiverLocalForDispatch)
              genTableDispatch(objectClassInfo, toStringMethodName, receiverLocalForDispatch)
              instrs += BR_ON_NON_NULL(labelDone)
              instrs += REF_NULL(Types.WasmHeapType.Any)
            } // end block labelNotOurObject

            // Now we have a value that is not one of our objects; the anyref is still on the stack
            instrs += CALL(WasmFunctionName.jsValueToStringForConcat)
          } // end block labelDone
        }
      }

      tree.tpe match {
        case primType: IRTypes.PrimType =>
          genTreeAuto(tree)
          primType match {
            case IRTypes.StringType =>
              () // no-op
            case IRTypes.BooleanType =>
              instrs += CALL(WasmFunctionName.booleanToString)
            case IRTypes.CharType =>
              instrs += CALL(WasmFunctionName.charToString)
            case IRTypes.ByteType | IRTypes.ShortType | IRTypes.IntType =>
              instrs += CALL(WasmFunctionName.intToString)
            case IRTypes.LongType =>
              instrs += CALL(WasmFunctionName.longToString)
            case IRTypes.FloatType =>
              instrs += F64_PROMOTE_F32
              instrs += CALL(WasmFunctionName.doubleToString)
            case IRTypes.DoubleType =>
              instrs += CALL(WasmFunctionName.doubleToString)
            case IRTypes.NullType | IRTypes.UndefType =>
              instrs += CALL(WasmFunctionName.jsValueToStringForConcat)
            case IRTypes.NothingType =>
              () // unreachable
            case IRTypes.NoType =>
              throw new AssertionError(
                s"Found expression of type void in String_+ at ${tree.pos}: $tree"
              )
          }

        case IRTypes.ClassType(IRNames.BoxedStringClass) =>
          // Common case for which we want to avoid the hijacked class dispatch
          genTreeAuto(tree)
          instrs += CALL(WasmFunctionName.jsValueToStringForConcat) // for `null`

        case IRTypes.ClassType(className) =>
          genWithDispatch(ctx.getClassInfo(className).isAncestorOfHijackedClass)

        case IRTypes.AnyType =>
          genWithDispatch(isAncestorOfHijackedClass = true)

        case IRTypes.ArrayType(_) =>
          genWithDispatch(isAncestorOfHijackedClass = false)

        case tpe: IRTypes.RecordType =>
          throw new AssertionError(s"Invalid type $tpe for String_+ at ${tree.pos}: $tree")
      }
    }

    lhs match {
      case IRTrees.StringLiteral("") =>
        // Common case where we don't actually need a concatenation
        genToString(rhs)

      case _ =>
        genToString(lhs)
        genToString(rhs)
        instrs += CALL(WasmFunctionName.stringConcat)
    }

    IRTypes.StringType
  }

  private def genIsInstanceOf(tree: IRTrees.IsInstanceOf): IRTypes.Type = {
    genTree(tree.expr, IRTypes.AnyType)

    def genIsPrimType(testType: IRTypes.PrimType): Unit = {
      testType match {
        case IRTypes.UndefType =>
          instrs += CALL(WasmFunctionName.isUndef)
        case IRTypes.StringType =>
          instrs += CALL(WasmFunctionName.isString)

        case testType: IRTypes.PrimTypeWithRef =>
          testType match {
            case IRTypes.CharType =>
              val structTypeName = WasmStructTypeName.forClass(SpecialNames.CharBoxClass)
              instrs += REF_TEST(Types.WasmRefType(structTypeName))
            case IRTypes.LongType =>
              val structTypeName = WasmStructTypeName.forClass(SpecialNames.LongBoxClass)
              instrs += REF_TEST(Types.WasmRefType(structTypeName))
            case IRTypes.NoType | IRTypes.NothingType | IRTypes.NullType =>
              throw new AssertionError(s"Illegal isInstanceOf[$testType]")
            case _ =>
              /* Calls the appropriate `tX` JS helper. It dynamically tests whether
               * the value fits in the given primitive type, according to
               * https://www.scala-js.org/doc/semantics.html
               * All the `tX` helpers have Wasm type `anyref -> i32` (interpreted as `boolean`).
               */
              instrs += CALL(WasmFunctionName.typeTest(testType.primRef))
          }
      }
    }

    tree.testType match {
      case testType: IRTypes.PrimType =>
        genIsPrimType(testType)

      case IRTypes.AnyType | IRTypes.ClassType(IRNames.ObjectClass) =>
        instrs += REF_IS_NULL
        instrs += I32_CONST(1)
        instrs += I32_XOR

      case IRTypes.ClassType(JLNumberClass) =>
        /* Special case: the only non-Object *class* that is an ancestor of a
         * hijacked class. We need to accept `number` primitives here.
         */
        val tempLocal = fctx.addSyntheticLocal(Types.WasmRefType.anyref)
        instrs += LOCAL_TEE(tempLocal)
        instrs += REF_TEST(Types.WasmRefType(WasmStructTypeName.forClass(JLNumberClass)))
        fctx.ifThenElse(Types.WasmInt32) {
          instrs += I32_CONST(1)
        } {
          instrs += LOCAL_GET(tempLocal)
          instrs += CALL(WasmFunctionName.typeTest(IRTypes.DoubleRef))
        }

      case IRTypes.ClassType(testClassName) =>
        IRTypes.BoxedClassToPrimType.get(testClassName) match {
          case Some(primType) =>
            genIsPrimType(primType)
          case None =>
            val info = ctx.getClassInfo(testClassName)

            if (info.isInterface)
              instrs += CALL(WasmFunctionName.instanceTest(testClassName))
            else
              instrs += REF_TEST(Types.WasmRefType(WasmStructTypeName.forClass(testClassName)))
        }

      case IRTypes.ArrayType(arrayTypeRef) =>
        arrayTypeRef match {
          case IRTypes.ArrayTypeRef(
                IRTypes.ClassRef(IRNames.ObjectClass) | _: IRTypes.PrimRef,
                1
              ) =>
            // For primitive arrays and exactly Array[Object], a REF_TEST is enough
            val structTypeName = WasmStructTypeName.forArrayClass(arrayTypeRef)
            instrs += REF_TEST(Types.WasmRefType(structTypeName))

          case _ =>
            /* Non-Object reference arra types need a sophisticated type test
             * based on assignability of component types.
             */
            import wasm.wasm4s.{WasmFunctionSignature => Sig}
            import wasm.wasm4s.Types.WasmRefType.anyref

            fctx.block(Sig(List(anyref), List(Types.WasmInt32))) { doneLabel =>
              fctx.block(Sig(List(anyref), List(anyref))) { notARefArrayLabel =>
                // Try and cast to the generic representation first
                val refArrayStructTypeName = WasmStructTypeName.forArrayClass(arrayTypeRef)
                instrs += BR_ON_CAST_FAIL(
                  notARefArrayLabel,
                  Types.WasmRefType.anyref,
                  Types.WasmRefType(refArrayStructTypeName)
                )

                // refArrayValue := the generic representation
                val refArrayValueLocal =
                  fctx.addSyntheticLocal(Types.WasmRefType(refArrayStructTypeName))
                instrs += LOCAL_SET(refArrayValueLocal)

                // Load typeDataOf(arrayTypeRef)
                genLoadArrayTypeData(arrayTypeRef)

                // Load refArrayValue.vtable
                instrs += LOCAL_GET(refArrayValueLocal)
                instrs += STRUCT_GET(refArrayStructTypeName, WasmFieldIdx.vtable)

                // Call isAssignableFrom and return its result
                instrs += CALL(WasmFunctionName.isAssignableFrom)
                instrs += BR(doneLabel)
              }

              // Here, the value is not a reference array type, so return false
              instrs += DROP
              instrs += I32_CONST(0)
            }
        }

      case testType: IRTypes.RecordType =>
        throw new AssertionError(s"Illegal type in IsInstanceOf: $testType")
    }

    IRTypes.BooleanType
  }

  private def genAsInstanceOf(tree: IRTrees.AsInstanceOf): IRTypes.Type = {
    val sourceTpe = tree.expr.tpe
    val targetTpe = tree.tpe

    if (IRTypes.isSubtype(sourceTpe, targetTpe)(isSubclass(_, _))) {
      // Common case where no cast is necessary
      genTreeAuto(tree.expr)
      sourceTpe
    } else {
      genTree(tree.expr, IRTypes.AnyType)

      def genAsPrimType(targetTpe: IRTypes.PrimType): Unit = {
        // TODO We could do something better for things like double.asInstanceOf[int]
        genUnbox(targetTpe)(tree.pos)
      }

      targetTpe match {
        case targetTpe: IRTypes.PrimType =>
          genAsPrimType(targetTpe)

        case IRTypes.AnyType =>
          ()

        case IRTypes.ClassType(targetClassName) =>
          val info = ctx.getClassInfo(targetClassName)
          if (info.kind == ClassKind.HijackedClass) {
            IRTypes.BoxedClassToPrimType(targetClassName) match {
              case IRTypes.UndefType | IRTypes.StringType =>
                ()
              case primType: IRTypes.PrimTypeWithRef =>
                primType match {
                  case IRTypes.CharType =>
                    val structTypeName = WasmStructTypeName.forClass(SpecialNames.CharBoxClass)
                    instrs += REF_CAST(Types.WasmRefType.nullable(structTypeName))
                  case IRTypes.LongType =>
                    val structTypeName = WasmStructTypeName.forClass(SpecialNames.LongBoxClass)
                    instrs += REF_CAST(Types.WasmRefType.nullable(structTypeName))
                  case IRTypes.NoType | IRTypes.NothingType | IRTypes.NullType =>
                    throw new AssertionError(s"Unexpected prim type $primType for $targetClassName")
                  case _ =>
                    instrs += CALL(WasmFunctionName.unboxOrNull(primType.primRef))
                }
            }
          } else if (info.isAncestorOfHijackedClass) {
            // Nothing to do; the translation is `anyref`
            ()
          } else if (info.kind.isClass) {
            instrs += REF_CAST(
              Types.WasmRefType.nullable(WasmStructTypeName.forClass(targetClassName))
            )
          } else if (info.isInterface) {
            instrs += REF_CAST(Types.WasmRefType.nullable(Types.WasmHeapType.ObjectType))
          }

        case IRTypes.ArrayType(arrayTypeRef) =>
          val structTypeName = WasmStructTypeName.forArrayClass(arrayTypeRef)
          instrs += REF_CAST(Types.WasmRefType.nullable(structTypeName))

        case targetTpe: IRTypes.RecordType =>
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
  private def genUnbox(targetTpe: IRTypes.PrimType)(implicit pos: Position): Unit = {
    targetTpe match {
      case IRTypes.UndefType =>
        instrs += DROP
        instrs += CALL(WasmFunctionName.undef)
      case IRTypes.StringType =>
        instrs += REF_AS_NOT_NULL

      case targetTpe: IRTypes.PrimTypeWithRef =>
        targetTpe match {
          case IRTypes.CharType | IRTypes.LongType =>
            // Extract the `value` field (the only field) out of the box class.
            import wasm.wasm4s.{WasmFunctionSignature => Sig}

            val boxClass =
              if (targetTpe == IRTypes.CharType) SpecialNames.CharBoxClass
              else SpecialNames.LongBoxClass
            val resultType = TypeTransformer.transformType(targetTpe)(ctx)

            fctx.block(Sig(List(Types.WasmRefType.anyref), List(resultType))) { doneLabel =>
              fctx.block(Sig(List(Types.WasmRefType.anyref), Nil)) { isNullLabel =>
                instrs += BR_ON_NULL(isNullLabel)
                val structTypeName = WasmStructTypeName.forClass(boxClass)
                instrs += REF_CAST(Types.WasmRefType(structTypeName))
                instrs += STRUCT_GET(structTypeName, WasmFieldIdx.uniqueRegularField)
                instrs += BR(doneLabel)
              }
              genTree(IRTypes.zeroOf(targetTpe), targetTpe)
            }

          case IRTypes.NothingType | IRTypes.NullType | IRTypes.NoType =>
            throw new IllegalArgumentException(s"Illegal type in genUnbox: $targetTpe")
          case _ =>
            instrs += CALL(WasmFunctionName.unbox(targetTpe.primRef))
        }
    }
  }

  private def isSubclass(subClass: IRNames.ClassName, superClass: IRNames.ClassName): Boolean =
    ctx.getClassInfo(subClass).ancestors.contains(superClass)

  private def genGetClass(tree: IRTrees.GetClass): IRTypes.Type = {
    /* Unlike in `genApply` or `genStringConcat`, here we make no effort to
     * optimize known-primitive receivers. In practice, such cases would be
     * useless.
     */

    val needHijackedClassDispatch = tree.expr.tpe match {
      case IRTypes.ClassType(className) =>
        ctx.getClassInfo(className).isAncestorOfHijackedClass
      case IRTypes.ArrayType(_) | IRTypes.NothingType | IRTypes.NullType =>
        false
      case _ =>
        true
    }

    if (!needHijackedClassDispatch) {
      val typeDataType = Types.WasmRefType(WasmStructTypeName.typeData)
      val objectTypeIdx = WasmStructTypeName.forClass(IRNames.ObjectClass)

      val typeDataLocal = fctx.addSyntheticLocal(typeDataType)

      genTreeAuto(tree.expr)
      instrs += STRUCT_GET(objectTypeIdx, WasmFieldIdx.vtable) // implicit trap on null
      instrs += LOCAL_SET(typeDataLocal)
      genClassOfFromTypeData(LOCAL_GET(typeDataLocal))
    } else {
      genTree(tree.expr, IRTypes.AnyType)
      instrs += REF_AS_NOT_NULL
      instrs += CALL(WasmFunctionName.anyGetClass)
    }

    tree.tpe
  }

  private def genReadStorage(storage: WasmFunctionContext.VarStorage): Unit = {
    import WasmFunctionContext.VarStorage

    storage match {
      case VarStorage.Local(localIdx) =>
        instrs += LOCAL_GET(localIdx)
      case VarStorage.StructField(structLocalIdx, structTypeName, fieldIdx) =>
        instrs += LOCAL_GET(structLocalIdx)
        instrs += STRUCT_GET(structTypeName, fieldIdx)
    }
  }

  private def genVarRef(r: IRTrees.VarRef): IRTypes.Type = {
    genReadStorage(fctx.lookupLocal(r.ident.name))
    r.tpe
  }

  private def genThis(t: IRTrees.This): IRTypes.Type = {
    genReadStorage(fctx.receiverStorage)

    // Workaround wrong t.tpe for This nodes inside reflective proxies.
    // In a hijacked class, This nodes are supposed to be typed as the corresponding primitive type.
    // However, the Scala.js linker frontend synthesizes reflective proxies that contain This nodes typed as the hijacked class' ClassType instead.
    // This is bad for us because it means genAdapt fails to box the primitives when required.
    // We work around this issue here by re-computing the correct type of This nodes.
    val fixedTpe = t.tpe match {
      case IRTypes.ClassType(cls) => IRTypes.BoxedClassToPrimType.getOrElse(cls, t.tpe)
      case _                      => t.tpe
    }

    /* If the receiver is a Class/ModuleClass, its wasm type will be declared
     * as `(ref any)`, and therefore we must cast it down.
     */
    fixedTpe match {
      case IRTypes.ClassType(className) if className != IRNames.ObjectClass =>
        val info = ctx.getClassInfo(className)
        if (info.kind.isClass) {
          instrs += REF_CAST(Types.WasmRefType(WasmStructTypeName.forClass(className)))
        } else if (info.isInterface) {
          instrs += REF_CAST(Types.WasmRefType(Types.WasmHeapType.ObjectType))
        }
      case _ =>
        ()
    }

    fixedTpe
  }

  private def genVarDef(r: IRTrees.VarDef): IRTypes.Type = {
    /* This is an isolated VarDef that is not in a Block.
     * Its scope is empty by construction, and therefore it need not be stored.
     */
    genTree(r.rhs, IRTypes.NoType)
    IRTypes.NoType
  }

  private def genIf(t: IRTrees.If, expectedType: IRTypes.Type): IRTypes.Type = {
    val ty = TypeTransformer.transformResultType(expectedType)(ctx)
    genTree(t.cond, IRTypes.BooleanType)

    t.elsep match {
      case IRTrees.Skip() =>
        assert(expectedType == IRTypes.NoType)
        fctx.ifThen() {
          genTree(t.thenp, expectedType)
        }
      case _ =>
        fctx.ifThenElse(ty) {
          genTree(t.thenp, expectedType)
        } {
          genTree(t.elsep, expectedType)
        }
    }

    if (expectedType == IRTypes.NothingType)
      instrs += UNREACHABLE

    expectedType
  }

  private def genWhile(t: IRTrees.While): IRTypes.Type = {
    t.cond match {
      case IRTrees.BooleanLiteral(true) =>
        // infinite loop that must be typed as `nothing`, i.e., unreachable
        // loop $label
        //   body
        //   br $label
        // end
        // unreachable
        fctx.loop() { label =>
          genTree(t.body, IRTypes.NoType)
          instrs += BR(label)
        }
        instrs += UNREACHABLE
        IRTypes.NothingType

      case _ =>
        // loop $label
        //   cond
        //   if
        //     body
        //     br $label
        //   end
        // end
        fctx.loop() { label =>
          genTree(t.cond, IRTypes.BooleanType)
          fctx.ifThen() {
            genTree(t.body, IRTypes.NoType)
            instrs += BR(label)
          }
        }
        IRTypes.NoType
    }
  }

  private def genForIn(t: IRTrees.ForIn): IRTypes.Type = {
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
      case IRTrees.JSFunctionApply(fVarRef: IRTrees.VarRef, List(IRTrees.VarRef(argIdent)))
          if fVarRef.ident.name != t.keyVar.name && argIdent.name == t.keyVar.name =>
        genTree(t.obj, IRTypes.AnyType)
        genTree(fVarRef, IRTypes.AnyType)
        instrs += CALL(WasmFunctionName.jsForInSimple)

      case _ =>
        throw new NotImplementedError(s"Unsupported shape of ForIn node at ${t.pos}: $t")
    }

    IRTypes.NoType
  }

  private def genTryCatch(t: IRTrees.TryCatch, expectedType: IRTypes.Type): IRTypes.Type = {
    val resultType = TypeTransformer.transformResultType(expectedType)(ctx)

    if (UseLegacyExceptionsForTryCatch) {
      instrs += TRY(fctx.sigToBlockType(WasmFunctionSignature(Nil, resultType)))
      genTree(t.block, expectedType)
      instrs += CATCH(ctx.exceptionTagName)
      fctx.withNewLocal(t.errVar.name, Types.WasmRefType.anyref) { exceptionLocal =>
        instrs += ANY_CONVERT_EXTERN
        instrs += LOCAL_SET(exceptionLocal)
        genTree(t.handler, expectedType)
      }
      instrs += END
    } else {
      fctx.block(resultType) { doneLabel =>
        fctx.block(Types.WasmRefType.externref) { catchLabel =>
          /* We used to have `resultType` as result of the try_table, with the
           * `BR(doneLabel)` outside of the try_table. Unfortunately it seems
           * V8 cannot handle try_table with a result type that is `(ref ...)`.
           * The current encoding with `externref` as result type (to match the
           * enclosing block) and the `br` *inside* the `try_table` works.
           */
          fctx.tryTable(Types.WasmRefType.externref)(
            List(CatchClause.Catch(ctx.exceptionTagName, catchLabel))
          ) {
            genTree(t.block, expectedType)
            instrs += BR(doneLabel)
          }
        } // end block $catch
        fctx.withNewLocal(t.errVar.name, Types.WasmRefType.anyref) { exceptionLocal =>
          instrs += ANY_CONVERT_EXTERN
          instrs += LOCAL_SET(exceptionLocal)
          genTree(t.handler, expectedType)
        }
      } // end block $done
    }

    if (expectedType == IRTypes.NothingType)
      instrs += UNREACHABLE

    expectedType
  }

  private def genThrow(tree: IRTrees.Throw): IRTypes.Type = {
    genTree(tree.expr, IRTypes.AnyType)
    instrs += EXTERN_CONVERT_ANY
    instrs += THROW(ctx.exceptionTagName)

    IRTypes.NothingType
  }

  private def genBlock(t: IRTrees.Block, expectedType: IRTypes.Type): IRTypes.Type = {
    genBlockStats(t.stats.init) {
      genTree(t.stats.last, expectedType)
    }
    expectedType
  }

  final def genBlockStats[A](stats: List[IRTrees.Tree])(inner: => A): A = {
    stats match {
      case IRTrees.VarDef(name, _, vtpe, _, rhs) :: rest =>
        genTree(rhs, vtpe)
        fctx.withNewLocal(name.name, TypeTransformer.transformType(vtpe)(ctx)) { local =>
          instrs += LOCAL_SET(local)
          genBlockStats(rest)(inner)
        }
      case stat :: rest =>
        genTree(stat, IRTypes.NoType)
        genBlockStats(rest)(inner)
      case Nil =>
        inner
    }
  }

  private def genNew(n: IRTrees.New): IRTypes.Type = {
    /* Do not use transformType here, because we must get the struct type even
     * if the given class is an ancestor of hijacked classes (which in practice
     * is only the case for j.l.Object).
     */
    val instanceTyp =
      Types.WasmRefType.nullable(WasmStructTypeName.forClass(n.className))
    val localInstance = fctx.addSyntheticLocal(instanceTyp)

    instrs += CALL(WasmFunctionName.newDefault(n.className))
    instrs += LOCAL_TEE(localInstance)
    genArgs(n.args, n.ctor.name)
    instrs += CALL(
      WasmFunctionName(
        IRTrees.MemberNamespace.Constructor,
        n.className,
        n.ctor.name
      )
    )
    instrs += LOCAL_GET(localInstance)
    n.tpe
  }

  /** Codegen to box a primitive `char`/`long` into a `CharacterBox`/`LongBox`. */
  private def genBox(
      primType: IRTypes.PrimTypeWithRef,
      boxClassName: IRNames.ClassName
  ): IRTypes.Type = {
    // `primTyp` is `i32` for `char` (containing a `u16` value) or `i64` for `long`.
    val primTyp = TypeTransformer.transformType(primType)(ctx)
    val primLocal = fctx.addSyntheticLocal(primTyp)

    val boxClassType = IRTypes.ClassType(boxClassName)
    val boxTyp = TypeTransformer.transformType(boxClassType)(ctx)
    val instanceLocal = fctx.addSyntheticLocal(boxTyp)

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

    instrs += LOCAL_SET(primLocal)
    instrs += CALL(WasmFunctionName.newDefault(boxClassName))
    instrs += LOCAL_TEE(instanceLocal)
    instrs += LOCAL_GET(primLocal)
    instrs += STRUCT_SET(
      WasmStructTypeName.forClass(boxClassName),
      WasmFieldIdx.uniqueRegularField
    )
    instrs += LOCAL_GET(instanceLocal)

    boxClassType
  }

  private def genIdentityHashCode(tree: IRTrees.IdentityHashCode): IRTypes.Type = {
    // TODO Avoid dispatch when we know a more precise type than any
    genTree(tree.expr, IRTypes.AnyType)
    instrs += CALL(WasmFunctionName.identityHashCode)

    IRTypes.IntType
  }

  private def genWrapAsThrowable(tree: IRTrees.WrapAsThrowable): IRTypes.Type = {
    val throwableClassType = IRTypes.ClassType(IRNames.ThrowableClass)
    val throwableTyp = TypeTransformer.transformType(throwableClassType)(ctx)

    val jsExceptionClassType = IRTypes.ClassType(SpecialNames.JSExceptionClass)
    val jsExceptionTyp = TypeTransformer.transformType(jsExceptionClassType)(ctx)

    fctx.block(throwableTyp) { doneLabel =>
      genTree(tree.expr, IRTypes.AnyType)

      // if expr.isInstanceOf[Throwable], then br $done
      instrs += BR_ON_CAST(
        doneLabel,
        Types.WasmRefType.anyref,
        Types.WasmRefType(Types.WasmHeapType.ThrowableType)
      )

      // otherwise, wrap in a new JavaScriptException

      val exprLocal = fctx.addSyntheticLocal(Types.WasmRefType.anyref)
      val instanceLocal = fctx.addSyntheticLocal(jsExceptionTyp)

      instrs += LOCAL_SET(exprLocal)
      instrs += CALL(WasmFunctionName.newDefault(SpecialNames.JSExceptionClass))
      instrs += LOCAL_TEE(instanceLocal)
      instrs += LOCAL_GET(exprLocal)
      instrs += CALL(
        WasmFunctionName(
          IRTrees.MemberNamespace.Constructor,
          SpecialNames.JSExceptionClass,
          SpecialNames.JSExceptionCtor
        )
      )
      instrs += LOCAL_GET(instanceLocal)
    }

    throwableClassType
  }

  private def genUnwrapFromThrowable(tree: IRTrees.UnwrapFromThrowable): IRTypes.Type = {
    fctx.block(Types.WasmRefType.anyref) { doneLabel =>
      genTree(tree.expr, IRTypes.ClassType(IRNames.ThrowableClass))

      instrs += REF_AS_NOT_NULL

      // if !expr.isInstanceOf[js.JavaScriptException], then br $done
      instrs += BR_ON_CAST_FAIL(
        doneLabel,
        Types.WasmRefType(Types.WasmHeapType.ThrowableType),
        Types.WasmRefType(Types.WasmHeapType.JSExceptionType)
      )

      // otherwise, unwrap the JavaScriptException by reading its field

      val idx =
        ctx.getClassInfo(SpecialNames.JSExceptionClass).getFieldIdx(SpecialNames.JSExceptionField)

      instrs += STRUCT_GET(WasmStructTypeName.forClass(SpecialNames.JSExceptionClass), idx)
    }

    IRTypes.AnyType
  }

  private def genJSNew(tree: IRTrees.JSNew): IRTypes.Type = {
    genTree(tree.ctor, IRTypes.AnyType)
    genJSArgsArray(tree.args)
    instrs += CALL(WasmFunctionName.jsNew)
    IRTypes.AnyType
  }

  private def genJSSelect(tree: IRTrees.JSSelect): IRTypes.Type = {
    genTree(tree.qualifier, IRTypes.AnyType)
    genTree(tree.item, IRTypes.AnyType)
    instrs += CALL(WasmFunctionName.jsSelect)
    IRTypes.AnyType
  }

  private def genJSFunctionApply(tree: IRTrees.JSFunctionApply): IRTypes.Type = {
    genTree(tree.fun, IRTypes.AnyType)
    genJSArgsArray(tree.args)
    instrs += CALL(WasmFunctionName.jsFunctionApply)
    IRTypes.AnyType
  }

  private def genJSMethodApply(tree: IRTrees.JSMethodApply): IRTypes.Type = {
    genTree(tree.receiver, IRTypes.AnyType)
    genTree(tree.method, IRTypes.AnyType)
    genJSArgsArray(tree.args)
    instrs += CALL(WasmFunctionName.jsMethodApply)
    IRTypes.AnyType
  }

  private def genJSImportCall(tree: IRTrees.JSImportCall): IRTypes.Type = {
    genTree(tree.arg, IRTypes.AnyType)
    instrs += CALL(WasmFunctionName.jsImportCall)
    IRTypes.AnyType
  }

  private def genJSImportMeta(tree: IRTrees.JSImportMeta): IRTypes.Type = {
    instrs += CALL(WasmFunctionName.jsImportMeta)
    IRTypes.AnyType
  }

  private def genLoadJSConstructor(tree: IRTrees.LoadJSConstructor): IRTypes.Type = {
    val info = ctx.getClassInfo(tree.className)

    info.kind match {
      case ClassKind.NativeJSClass =>
        val jsNativeLoadSpec = info.jsNativeLoadSpec.getOrElse {
          throw new AssertionError(s"Found $tree for class without jsNativeLoadSpec at ${tree.pos}")
        }
        genLoadJSNativeLoadSpec(fctx, jsNativeLoadSpec)(ctx)

      case ClassKind.JSClass =>
        instrs += CALL(WasmFunctionName.loadJSClass(tree.className))
        IRTypes.AnyType

      case _ =>
        throw new AssertionError(
          s"Invalid LoadJSConstructor for class ${tree.className.nameString} of kind ${info.kind}"
        )
    }
  }

  private def genLoadJSModule(tree: IRTrees.LoadJSModule): IRTypes.Type = {
    val info = ctx.getClassInfo(tree.className)

    info.kind match {
      case ClassKind.NativeJSModuleClass =>
        val jsNativeLoadSpec = info.jsNativeLoadSpec.getOrElse {
          throw new AssertionError(s"Found $tree for class without jsNativeLoadSpec at ${tree.pos}")
        }
        genLoadJSNativeLoadSpec(fctx, jsNativeLoadSpec)(ctx)

      case ClassKind.JSModuleClass =>
        instrs += CALL(WasmFunctionName.loadModule(tree.className))
        IRTypes.AnyType

      case _ =>
        throw new AssertionError(
          s"Invalid LoadJSModule for class ${tree.className.nameString} of kind ${info.kind}"
        )
    }
  }

  private def genSelectJSNativeMember(tree: IRTrees.SelectJSNativeMember): IRTypes.Type = {
    val info = ctx.getClassInfo(tree.className)
    val jsNativeLoadSpec = info.jsNativeMembers.getOrElse(
      tree.member.name, {
        throw new AssertionError(s"Found $tree for non-existing JS native member at ${tree.pos}")
      }
    )
    genLoadJSNativeLoadSpec(fctx, jsNativeLoadSpec)(ctx)
  }

  private def genJSDelete(tree: IRTrees.JSDelete): IRTypes.Type = {
    genTree(tree.qualifier, IRTypes.AnyType)
    genTree(tree.item, IRTypes.AnyType)
    instrs += CALL(WasmFunctionName.jsDelete)
    IRTypes.NoType
  }

  private def genJSUnaryOp(tree: IRTrees.JSUnaryOp): IRTypes.Type = {
    genTree(tree.lhs, IRTypes.AnyType)
    instrs += CALL(WasmFunctionName.jsUnaryOps(tree.op))
    IRTypes.AnyType
  }

  private def genJSBinaryOp(tree: IRTrees.JSBinaryOp): IRTypes.Type = {
    import IRTrees.JSBinaryOp

    tree.op match {
      case JSBinaryOp.|| | JSBinaryOp.&& =>
        /* Here we need to implement the short-circuiting behavior, with a
         * condition based on the truthy value of the left-hand-side.
         */
        val lhsLocal = fctx.addSyntheticLocal(Types.WasmRefType.anyref)
        genTree(tree.lhs, IRTypes.AnyType)
        instrs += LOCAL_TEE(lhsLocal)
        instrs += CALL(WasmFunctionName.jsIsTruthy)
        instrs += IF(BlockType.ValueType(Types.WasmRefType.anyref))
        if (tree.op == JSBinaryOp.||) {
          instrs += LOCAL_GET(lhsLocal)
          instrs += ELSE
          genTree(tree.rhs, IRTypes.AnyType)
        } else {
          genTree(tree.rhs, IRTypes.AnyType)
          instrs += ELSE
          instrs += LOCAL_GET(lhsLocal)
        }
        instrs += END

      case _ =>
        genTree(tree.lhs, IRTypes.AnyType)
        genTree(tree.rhs, IRTypes.AnyType)
        instrs += CALL(WasmFunctionName.jsBinaryOps(tree.op))
    }

    tree.tpe
  }

  private def genJSArrayConstr(tree: IRTrees.JSArrayConstr): IRTypes.Type = {
    genJSArgsArray(tree.items)
    IRTypes.AnyType
  }

  private def genJSObjectConstr(tree: IRTrees.JSObjectConstr): IRTypes.Type = {
    instrs += CALL(WasmFunctionName.jsNewObject)
    for ((prop, value) <- tree.fields) {
      genTree(prop, IRTypes.AnyType)
      genTree(value, IRTypes.AnyType)
      instrs += CALL(WasmFunctionName.jsObjectPush)
    }
    IRTypes.AnyType
  }

  private def genJSGlobalRef(tree: IRTrees.JSGlobalRef): IRTypes.Type = {
    instrs ++= ctx.getConstantStringInstr(tree.name)
    instrs += CALL(WasmFunctionName.jsGlobalRefGet)
    IRTypes.AnyType
  }

  private def genJSTypeOfGlobalRef(tree: IRTrees.JSTypeOfGlobalRef): IRTypes.Type = {
    instrs ++= ctx.getConstantStringInstr(tree.globalRef.name)
    instrs += CALL(WasmFunctionName.jsGlobalRefTypeof)
    IRTypes.AnyType
  }

  private def genJSArgsArray(args: List[IRTrees.TreeOrJSSpread]): Unit = {
    instrs += CALL(WasmFunctionName.jsNewArray)
    for (arg <- args) {
      arg match {
        case arg: IRTrees.Tree =>
          genTree(arg, IRTypes.AnyType)
          instrs += CALL(WasmFunctionName.jsArrayPush)
        case IRTrees.JSSpread(items) =>
          genTree(items, IRTypes.AnyType)
          instrs += CALL(WasmFunctionName.jsArraySpreadPush)
      }
    }
  }

  private def genJSLinkingInfo(tree: IRTrees.JSLinkingInfo): IRTypes.Type = {
    instrs += CALL(WasmFunctionName.jsLinkingInfo)
    IRTypes.AnyType
  }

  // ===============================================================================
  // array
  // ===============================================================================
  private def genArrayLength(t: IRTrees.ArrayLength): IRTypes.Type = {
    genTreeAuto(t.array)

    t.array.tpe match {
      case IRTypes.ArrayType(arrayTypeRef) =>
        // Get the underlying array; implicit trap on null
        instrs += STRUCT_GET(
          WasmStructTypeName.forArrayClass(arrayTypeRef),
          WasmFieldIdx.uniqueRegularField
        )
        // Get the length
        instrs += ARRAY_LEN
        IRTypes.IntType

      case IRTypes.NothingType =>
        // unreachable
        IRTypes.NothingType
      case IRTypes.NullType =>
        instrs += UNREACHABLE
        IRTypes.NothingType
      case _ =>
        throw new IllegalArgumentException(
          s"ArraySelect.array must be an array type, but has type ${t.array.tpe}"
        )
    }
  }

  private def genNewArray(t: IRTrees.NewArray): IRTypes.Type = {
    val arrayTypeRef = t.typeRef

    if (t.lengths.isEmpty || t.lengths.size > arrayTypeRef.dimensions)
      throw new AssertionError(
        s"invalid lengths ${t.lengths} for array type ${arrayTypeRef.displayName}"
      )

    if (t.lengths.size == 1) {
      genLoadVTableAndITableForArray(arrayTypeRef)

      // Create the underlying array
      genTree(t.lengths.head, IRTypes.IntType)
      val underlyingArrayType = WasmArrayTypeName.underlyingOf(arrayTypeRef)
      instrs += ARRAY_NEW_DEFAULT(underlyingArrayType)

      // Create the array object
      instrs += STRUCT_NEW(WasmStructTypeName.forArrayClass(arrayTypeRef))
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
        genTree(length, IRTypes.IntType)
      instrs += ARRAY_NEW_FIXED(WasmArrayTypeName.i32Array, t.lengths.size)

      // Third arg: constant 0 (start index inside the array of lengths)
      instrs += I32_CONST(0)

      instrs += CALL(WasmFunctionName.newArrayObject)
    }

    t.tpe
  }

  /** Gen code to load the vtable and the itable of the given array type. */
  private def genLoadVTableAndITableForArray(arrayTypeRef: IRTypes.ArrayTypeRef): Unit = {
    // Load the typeData of the resulting array type. It is the vtable of the resulting object.
    genLoadArrayTypeData(arrayTypeRef)

    // Load the itables for the array type
    instrs += GLOBAL_GET(WasmGlobalName.arrayClassITable)
  }

  /** For getting element from an array, array.set should be generated by transformation of
    * `Assign(ArraySelect(...), ...)`
    */
  private def genArraySelect(t: IRTrees.ArraySelect): IRTypes.Type = {
    genTreeAuto(t.array)

    t.array.tpe match {
      case IRTypes.ArrayType(arrayTypeRef) =>
        // Get the underlying array; implicit trap on null
        instrs += STRUCT_GET(
          WasmStructTypeName.forArrayClass(arrayTypeRef),
          WasmFieldIdx.uniqueRegularField
        )

        // Load the index
        genTree(t.index, IRTypes.IntType)

        // Use the appropriate variant of array.get for sign extension
        val typeIdx = WasmArrayTypeName.underlyingOf(arrayTypeRef)
        arrayTypeRef match {
          case IRTypes.ArrayTypeRef(IRTypes.BooleanRef | IRTypes.CharRef, 1) =>
            instrs += ARRAY_GET_U(typeIdx)
          case IRTypes.ArrayTypeRef(IRTypes.ByteRef | IRTypes.ShortRef, 1) =>
            instrs += ARRAY_GET_S(typeIdx)
          case _ =>
            instrs += ARRAY_GET(typeIdx)
        }

        /* If it is a reference array type whose element type does not translate
         * to `anyref`, we must cast down the result.
         */
        arrayTypeRef match {
          case IRTypes.ArrayTypeRef(_: IRTypes.PrimRef, 1) =>
            // a primitive array type always has the correct
            ()
          case _ =>
            TypeTransformer.transformType(t.tpe)(ctx) match {
              case Types.WasmRefType.anyref =>
                // nothing to do
                ()
              case refType: Types.WasmRefType =>
                instrs += REF_CAST(refType)
              case typ =>
                throw new AssertionError(s"Unexpected result type for reference array: $typ")
            }
        }

        t.tpe

      case IRTypes.NothingType =>
        // unreachable
        IRTypes.NothingType
      case IRTypes.NullType =>
        instrs += UNREACHABLE
        IRTypes.NothingType
      case _ =>
        throw new IllegalArgumentException(
          s"ArraySelect.array must be an array type, but has type ${t.array.tpe}"
        )
    }
  }

  private def genArrayValue(t: IRTrees.ArrayValue): IRTypes.Type = {
    val arrayTypeRef = t.typeRef

    genLoadVTableAndITableForArray(arrayTypeRef)

    val expectedElemType = arrayTypeRef match {
      case IRTypes.ArrayTypeRef(base: IRTypes.PrimRef, 1) => base.tpe
      case _                                              => IRTypes.AnyType
    }

    // Create the underlying array
    t.elems.foreach(genTree(_, expectedElemType))
    val underlyingArrayType = WasmArrayTypeName.underlyingOf(arrayTypeRef)
    instrs += ARRAY_NEW_FIXED(underlyingArrayType, t.elems.size)

    // Create the array object
    instrs += STRUCT_NEW(WasmStructTypeName.forArrayClass(arrayTypeRef))

    t.tpe
  }

  private def genClosure(tree: IRTrees.Closure): IRTypes.Type = {
    implicit val ctx = this.ctx

    val hasThis = !tree.arrow
    val hasRestParam = tree.restParam.isDefined
    val dataStructTypeName = ctx.getClosureDataStructType(tree.captureParams.map(_.ptpe))

    // Define the function where captures are reified as a `__captureData` argument.
    val closureFuncName = fctx.genInnerFuncName()
    locally {
      val receiverTyp =
        if (!hasThis) None
        else Some(Types.WasmRefType.anyref)

      val resultTyps = TypeTransformer.transformResultType(IRTypes.AnyType)

      implicit val fctx = WasmFunctionContext(
        enclosingClassName = None,
        closureFuncName,
        Some(tree.captureParams),
        receiverTyp,
        tree.params ::: tree.restParam.toList,
        resultTyps
      )

      // Transform the body - use AnyType as result type to box potential primitives
      WasmExpressionBuilder.generateIRBody(tree.body, IRTypes.AnyType)

      fctx.buildAndAddToContext()
    }

    // Put a reference to the function on the stack
    instrs += ctx.refFuncWithDeclaration(closureFuncName)

    // Evaluate the capture values and instantiate the capture data struct
    for ((param, value) <- tree.captureParams.zip(tree.captureValues))
      genTree(value, param.ptpe)
    instrs += STRUCT_NEW(dataStructTypeName)

    /* If there is a ...rest param, the helper requires as third argument the
     * number of regular arguments.
     */
    if (hasRestParam)
      instrs += I32_CONST(tree.params.size)

    // Call the appropriate helper
    val helper = (hasThis, hasRestParam) match {
      case (false, false) => WasmFunctionName.closure
      case (true, false)  => WasmFunctionName.closureThis
      case (false, true)  => WasmFunctionName.closureRest
      case (true, true)   => WasmFunctionName.closureThisRest
    }
    instrs += CALL(helper)

    IRTypes.AnyType
  }

  private def genClone(t: IRTrees.Clone): IRTypes.Type = {
    val expr = fctx.addSyntheticLocal(TypeTransformer.transformType(t.expr.tpe)(ctx))
    genTree(t.expr, IRTypes.ClassType(IRNames.CloneableClass))
    instrs += REF_CAST(Types.WasmRefType(Types.WasmHeapType.ObjectType))
    instrs += LOCAL_TEE(expr)
    instrs += REF_AS_NOT_NULL // cloneFunction argument is not nullable

    instrs += LOCAL_GET(expr)
    instrs += STRUCT_GET(
      WasmStructTypeName.forClass(IRNames.ObjectClass),
      WasmFieldIdx.vtable
    )
    instrs += STRUCT_GET(
      WasmTypeName.WasmStructTypeName.typeData,
      WasmFieldIdx.typeData.cloneFunctionIdx
    )
    // cloneFunction: (ref j.l.Object) -> ref j.l.Object
    instrs += CALL_REF(ctx.cloneFunctionTypeName)

    t.tpe match {
      case ClassType(className) =>
        val info = ctx.getClassInfo(className)
        if (!info.isInterface) // if it's interface, no need to cast from j.l.Object
          instrs += REF_CAST(Types.WasmRefType(WasmTypeName.WasmStructTypeName.forClass(className)))
      case _ =>
        throw new IllegalArgumentException(
          s"Clone result type must be a class type, but is ${t.tpe}"
        )
    }
    t.tpe
  }

  private def genMatch(tree: IRTrees.Match, expectedType: IRTypes.Type): IRTypes.Type = {
    val IRTrees.Match(selector, cases, defaultBody) = tree
    val selectorLocal = fctx.addSyntheticLocal(TypeTransformer.transformType(selector.tpe)(ctx))
    genTreeAuto(selector)
    instrs += LOCAL_SET(selectorLocal)

    fctx.block(TypeTransformer.transformResultType(expectedType)(ctx)) { doneLabel =>
      fctx.block() { defaultLabel =>
        val caseLabels = cases.map(c => c._1 -> fctx.genLabel())
        for (caseLabel <- caseLabels)
          instrs += BLOCK(BlockType.ValueType(), Some(caseLabel._2))

        for {
          caseLabel <- caseLabels
          matchableLiteral <- caseLabel._1
        } {
          val label = caseLabel._2
          instrs += LOCAL_GET(selectorLocal)
          matchableLiteral match {
            case IRTrees.IntLiteral(value) =>
              instrs += I32_CONST(value)
              instrs += I32_EQ
              instrs += BR_IF(label)
            case IRTrees.StringLiteral(value) =>
              instrs ++= ctx.getConstantStringInstr(value)
              instrs += CALL(WasmFunctionName.is)
              instrs += BR_IF(label)
            case IRTrees.Null() =>
              instrs += REF_IS_NULL
              instrs += BR_IF(label)
          }
        }
        instrs += BR(defaultLabel)

        for ((caseLabel, caze) <- caseLabels.zip(cases).reverse) {
          instrs += END
          genTree(caze._2, expectedType)
          instrs += BR(doneLabel)
        }
      }
      genTree(defaultBody, expectedType)
    }

    if (expectedType == IRTypes.NothingType)
      instrs += UNREACHABLE

    expectedType
  }

  private def genCreateJSClass(tree: IRTrees.CreateJSClass): IRTypes.Type = {
    val classInfo = ctx.getClassInfo(tree.className)
    val jsClassCaptures = classInfo.jsClassCaptures.getOrElse {
      throw new AssertionError(
        s"Illegal CreateJSClass of top-level class ${tree.className.nameString}"
      )
    }

    for ((captureValue, captureParam) <- tree.captureValues.zip(jsClassCaptures))
      genTree(captureValue, captureParam.ptpe)

    instrs += CALL(WasmFunctionName.createJSClassOf(tree.className))

    IRTypes.AnyType
  }

  private def genJSPrivateSelect(tree: IRTrees.JSPrivateSelect): IRTypes.Type = {
    genTree(tree.qualifier, IRTypes.AnyType)
    instrs += GLOBAL_GET(WasmGlobalName.forJSPrivateField(tree.field.name))
    instrs += CALL(WasmFunctionName.jsSelect)

    IRTypes.AnyType
  }

  private def genJSSuperSelect(tree: IRTrees.JSSuperSelect): IRTypes.Type = {
    genTree(tree.superClass, IRTypes.AnyType)
    genTree(tree.receiver, IRTypes.AnyType)
    genTree(tree.item, IRTypes.AnyType)
    instrs += CALL(WasmFunctionName.jsSuperGet)

    IRTypes.AnyType
  }

  private def genJSSuperMethodCall(tree: IRTrees.JSSuperMethodCall): IRTypes.Type = {
    genTree(tree.superClass, IRTypes.AnyType)
    genTree(tree.receiver, IRTypes.AnyType)
    genTree(tree.method, IRTypes.AnyType)
    genJSArgsArray(tree.args)
    instrs += CALL(WasmFunctionName.jsSuperCall)

    IRTypes.AnyType
  }

  private def genJSNewTarget(tree: IRTrees.JSNewTarget): IRTypes.Type = {
    genReadStorage(fctx.newTargetStorage)

    IRTypes.AnyType
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
    * overall internal scope of `WasmExpressionBuilder`.
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

    private var enclosingLabeledBlocks: Map[IRNames.LabelName, LabeledEntry] = Map.empty

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
      private var _crossInfo: Option[(WasmLocalName, WasmLabelName)] = None

      def isInside(labeledEntry: LabeledEntry): Boolean =
        this.depth > labeledEntry.depth

      def wasCrossed: Boolean = _crossInfo.isDefined

      def requireCrossInfo(): (WasmLocalName, WasmLabelName) = {
        _crossInfo.getOrElse {
          val info = (fctx.addSyntheticLocal(Types.WasmInt32), fctx.genLabel())
          _crossInfo = Some(info)
          info
        }
      }
    }

    /** Information about an enclosing `Labeled` block. */
    private final class LabeledEntry(
        val depth: Int,
        val irLabelName: IRNames.LabelName,
        val expectedType: IRTypes.Type
    ) {

      /** The regular label for this `Labeled` block, used for `Return`s that do not cross a
        * `TryFinally`.
        */
      val regularWasmLabel: WasmLabelName = fctx.genLabel()

      /** The destination tag allocated to this label, used by the `finally` blocks to keep
        * propagating to the right destination.
        *
        * Destination tags are always `> 0`. The value `0` is reserved for fall-through.
        */
      private var destinationTag: Int = 0

      /** The locals in which to store the result of the label if we have to cross a `try..finally`.
        */
      private var resultLocals: List[WasmLocalName] = null

      /** An additional Wasm label that has a `[]` result, and which will get its result from the
        * `resultLocal` instead of expecting it on the stack.
        */
      private var crossLabel: WasmLabelName = null

      def wasCrossUsed: Boolean = destinationTag != 0

      def requireCrossInfo(): (Int, List[WasmLocalName], WasmLabelName) = {
        if (destinationTag == 0) {
          destinationTag = allocateDestinationTag()
          val resultTypes = TypeTransformer.transformResultType(expectedType)(ctx)
          resultLocals = resultTypes.map(fctx.addSyntheticLocal(_))
          crossLabel = fctx.genLabel()
        }

        (destinationTag, resultLocals, crossLabel)
      }
    }

    def genLabeled(t: IRTrees.Labeled, expectedType: IRTypes.Type): IRTypes.Type = {
      val entry = new LabeledEntry(currentUnwindingStackDepth, t.label.name, expectedType)

      val ty = TypeTransformer.transformResultType(expectedType)(ctx)

      // Manual BLOCK here because we have a specific `label`
      instrs += BLOCK(
        fctx.sigToBlockType(WasmFunctionSignature(Nil, ty)),
        Some(entry.regularWasmLabel)
      )

      /* Remember the position in the instruction stream, in case we need to
       * come back and insert the BLOCK for the cross handling.
       */
      val instrsBlockBeginIndex = instrs.size

      // Emit the body
      enterLabeled(entry) {
        genTree(t.body, expectedType)
      }

      // Deal with crossing behavior
      if (entry.wasCrossUsed) {
        assert(
          expectedType != IRTypes.NothingType,
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
        instrs.insert(instrsBlockBeginIndex, BLOCK(BlockType.ValueType(), Some(crossLabel)))

        // Add the `br`, `end` and `local.get` at the current position, as usual
        instrs += BR(entry.regularWasmLabel)
        instrs += END
        for (local <- resultLocals)
          instrs += LOCAL_GET(local)
      }

      instrs += END

      if (expectedType == IRTypes.NothingType)
        instrs += UNREACHABLE

      expectedType
    }

    def genTryFinally(t: IRTrees.TryFinally, expectedType: IRTypes.Type): IRTypes.Type = {
      val entry = new TryFinallyEntry(currentUnwindingStackDepth)

      val resultType = TypeTransformer.transformResultType(expectedType)(ctx)
      val resultLocals = resultType.map(fctx.addSyntheticLocal(_))

      fctx.block() { doneLabel =>
        fctx.block(Types.WasmRefType.exnref) { catchLabel =>
          /* Remember the position in the instruction stream, in case we need
           * to come back and insert the BLOCK for the cross handling.
           */
          val instrsBlockBeginIndex = instrs.size

          fctx.tryTable()(List(CatchClause.CatchAllRef(catchLabel))) {
            // try block
            enterTryFinally(entry) {
              genTree(t.block, expectedType)
            }

            // store the result in locals during the finally block
            for (resultLocal <- resultLocals.reverse)
              instrs += LOCAL_SET(resultLocal)
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
              BLOCK(BlockType.ValueType(), Some(crossLabel))
            )

            // And the other amendments normally
            instrs += I32_CONST(0)
            instrs += LOCAL_SET(destinationTagLocal)
            instrs += END // of the inserted BLOCK
          }

          // on success, push a `null_ref exn` on the stack
          instrs += REF_NULL(Types.WasmHeapType.Exn)
        } // end block $catch

        // finally block (during which we leave the `(ref null exn)` on the stack)
        genTree(t.finalizer, IRTypes.NoType)

        if (!entry.wasCrossed) {
          // If the `exnref` is non-null, rethrow it
          instrs += BR_ON_NULL(doneLabel)
          instrs += THROW_REF
        } else {
          /* If the `exnref` is non-null, rethrow it.
           * Otherwise, stay within the `$done` block.
           */
          fctx.block(WasmFunctionSignature(List(Types.WasmRefType.exnref), Nil)) {
            exnrefIsNullLabel =>
              instrs += BR_ON_NULL(exnrefIsNullLabel)
              instrs += THROW_REF
          }

          /* Otherwise, use a br_table to dispatch to the right destination
           * based on the value of the try..finally's destinationTagLocal,
           * which is set by `Return` or to 0 for fall-through.
           */

          // The order does not matter here because they will be "re-sorted" by emitBRTable
          val possibleTargetEntries =
            enclosingLabeledBlocks.valuesIterator.filter(_.wasCrossUsed).toList

          val nextTryFinallyEntry = innermostTryFinally // note that we're out of ourselves already
            .filter(nextTry => possibleTargetEntries.exists(nextTry.isInside(_)))

          /* Build the destination table for `br_table`. Target Labeled's that
           * are outside of the next try..finally in line go to the latter;
           * for other `Labeled`'s, we go to their cross label.
           */
          val brTableDests: List[(Int, WasmLabelName)] = possibleTargetEntries.map { targetEntry =>
            val (destinationTag, _, crossLabel) = targetEntry.requireCrossInfo()
            val label = nextTryFinallyEntry.filter(_.isInside(targetEntry)) match {
              case None          => crossLabel
              case Some(nextTry) => nextTry.requireCrossInfo()._2
            }
            destinationTag -> label
          }

          instrs += LOCAL_GET(entry.requireCrossInfo()._1)
          for (nextTry <- nextTryFinallyEntry) {
            // Transfer the destinationTag to the next try..finally in line
            instrs += LOCAL_TEE(nextTry.requireCrossInfo()._1)
          }
          emitBRTable(brTableDests, doneLabel)
        }
      } // end block $done

      // reload the result onto the stack
      for (resultLocal <- resultLocals)
        instrs += LOCAL_GET(resultLocal)

      if (expectedType == IRTypes.NothingType)
        instrs += UNREACHABLE

      expectedType
    }

    private def emitBRTable(
        dests: List[(Int, WasmLabelName)],
        defaultLabel: WasmLabelName
    ): Unit = {
      dests match {
        case Nil =>
          instrs += DROP
          instrs += BR(defaultLabel)

        case (singleDestValue, singleDestLabel) :: Nil =>
          /* Common case (as far as getting here in the first place is concerned):
           * All the `Return`s that cross the current `TryFinally` have the same
           * target destination (namely the enclosing `def` in the original program).
           */
          instrs += I32_CONST(singleDestValue)
          instrs += I32_EQ
          instrs += BR_IF(singleDestLabel)
          instrs += BR(defaultLabel)

        case _ :: _ =>
          // `max` is safe here because the list is non-empty
          val table = Array.fill(dests.map(_._1).max + 1)(defaultLabel)
          for (dest <- dests)
            table(dest._1) = dest._2
          instrs += BR_TABLE(table.toList, defaultLabel)
      }
    }

    def genReturn(t: IRTrees.Return): IRTypes.Type = {
      val targetEntry = enclosingLabeledBlocks(t.label.name)

      genTree(t.expr, targetEntry.expectedType)

      if (targetEntry.expectedType != IRTypes.NothingType) {
        innermostTryFinally.filter(_.isInside(targetEntry)) match {
          case None =>
            // Easy case: directly branch out of the block
            instrs += BR(targetEntry.regularWasmLabel)

          case Some(tryFinallyEntry) =>
            /* Here we need to branch to the innermost enclosing `finally` block,
             * while remembering the destination label and the result value.
             */
            val (destinationTag, resultLocals, _) = targetEntry.requireCrossInfo()
            val (destinationTagLocal, crossLabel) = tryFinallyEntry.requireCrossInfo()

            // 1. Store the result in the label's result locals.
            for (local <- resultLocals.reverse)
              instrs += LOCAL_SET(local)

            // 2. Store the label's destination tag into the try..finally's destination local.
            instrs += I32_CONST(destinationTag)
            instrs += LOCAL_SET(destinationTagLocal)

            // 3. Branch to the enclosing `finally` block's cross label.
            instrs += BR(crossLabel)
        }
      }

      IRTypes.NothingType
    }
  }
}
