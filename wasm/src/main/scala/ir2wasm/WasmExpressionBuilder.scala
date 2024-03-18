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
import wasm4s.WasmImmediate._
import org.scalajs.ir.Types.ClassType
import org.scalajs.ir.ClassKind
import org.scalajs.ir.Position
import _root_.wasm4s.Defaults

object WasmExpressionBuilder {
  def generateIRBody(tree: IRTrees.Tree, resultType: IRTypes.Type)(implicit
      ctx: TypeDefinableWasmContext,
      fctx: WasmFunctionContext
  ): Unit = {
    val builder = new WasmExpressionBuilder(ctx, fctx)
    builder.genBody(tree, resultType)
  }

  private val ObjectRef = IRTypes.ClassRef(IRNames.ObjectClass)
  private val BoxedStringRef = IRTypes.ClassRef(IRNames.BoxedStringClass)
  private val toStringMethodName = IRNames.MethodName("toString", Nil, BoxedStringRef)
  private val hashCodeMethodName = IRNames.MethodName("hashCode", Nil, IRTypes.IntRef)
  private val equalsMethodName = IRNames.MethodName("equals", List(ObjectRef), IRTypes.BooleanRef)

  private val CharSequenceClass = IRNames.ClassName("java.lang.CharSequence")
  private val ComparableClass = IRNames.ClassName("java.lang.Comparable")
  private val JLNumberClass = IRNames.ClassName("java.lang.Number")

  private object PrimTypeWithBoxUnbox {
    def unapply(primType: IRTypes.PrimTypeWithRef): Option[IRTypes.PrimTypeWithRef] = {
      primType match {
        case IRTypes.BooleanType | IRTypes.ByteType | IRTypes.ShortType | IRTypes.IntType |
            IRTypes.FloatType | IRTypes.DoubleType =>
          Some(primType)
        case _ =>
          None
      }
    }
  }
}

private class WasmExpressionBuilder private (
    ctx: TypeDefinableWasmContext,
    fctx: WasmFunctionContext
) {
  import WasmExpressionBuilder._

  private val instrs = fctx.instrs

  def genBody(tree: IRTrees.Tree, expectedType: IRTypes.Type): Unit =
    genTree(tree, expectedType)

  /** object creation prefix
    * ```
    * local.get $receiver ;; (ref null $struct)
    * ref.is_null
    * if
    *   call $newDefault
    *   local.set $receiver
    * end
    * ```
    *
    * Maybe we can unreachable in else branch?
    */
  // def objectCreationPrefix(clazz: IRTrees.ClassDef, method: IRTrees.MethodDef): List[WasmInstr] = {
  //   assert(method.flags.namespace.isConstructor)
  //   val recieverGCTypeName = fctx.receiver.typ match {
  //     case Types.WasmRefNullType(Types.WasmHeapType.Type(gcType)) => gcType
  //     case Types.WasmRefType(Types.WasmHeapType.Type(gcType))     => gcType
  //     case _                                                      => ???
  //   }
  //   Nil
  //   // List(
  //   //   LOCAL_GET(LocalIdx(fctx.receiver.name)),
  //   //   REF_IS_NULL,
  //   //   IF(WasmImmediate.BlockType.ValueType(None)),
  //   //   CALL(FuncIdx(WasmFunctionName.newDefault(clazz.name.name))),
  //   //   LOCAL_SET(LocalIdx(fctx.receiver.name)),
  //   //   END
  //   // )
  // }

  def genTrees(trees: List[IRTrees.Tree], expectedTypes: List[IRTypes.Type]): Unit =
    trees.lazyZip(expectedTypes).foreach(genTree(_, _))

  def genTreeAuto(tree: IRTrees.Tree): Unit =
    genTree(tree, tree.tpe)

  def genTree(tree: IRTrees.Tree, expectedType: IRTypes.Type): Unit = {
    val generatedType: IRTypes.Type = tree match {
      case t: IRTrees.Literal          => genLiteral(t)
      case t: IRTrees.UnaryOp          => genUnaryOp(t)
      case t: IRTrees.BinaryOp         => genBinaryOp(t)
      case t: IRTrees.VarRef           => genVarRef(t)
      case t: IRTrees.LoadModule       => genLoadModule(t)
      case t: IRTrees.StoreModule      => genStoreModule(t)
      case t: IRTrees.This             => genThis(t)
      case t: IRTrees.ApplyStatically  => genApplyStatically(t)
      case t: IRTrees.Apply            => genApply(t)
      case t: IRTrees.ApplyStatic      => genApplyStatic(t)
      case t: IRTrees.IsInstanceOf     => genIsInstanceOf(t)
      case t: IRTrees.AsInstanceOf     => genAsInstanceOf(t)
      case t: IRTrees.GetClass         => genGetClass(t)
      case t: IRTrees.Block            => genBlock(t, expectedType)
      case t: IRTrees.Labeled          => genLabeled(t, expectedType)
      case t: IRTrees.Return           => genReturn(t)
      case t: IRTrees.Select           => genSelect(t)
      case t: IRTrees.Assign           => genAssign(t)
      case t: IRTrees.VarDef           => genVarDef(t)
      case t: IRTrees.New              => genNew(t)
      case t: IRTrees.If               => genIf(t, expectedType)
      case t: IRTrees.While            => genWhile(t)
      case t: IRTrees.Debugger         => IRTypes.NoType // ignore
      case t: IRTrees.Skip             => IRTypes.NoType
      case t: IRTrees.IdentityHashCode => genIdentityHashCode(t)

      // JavaScript expressions
      case t: IRTrees.JSNew                => genJSNew(t)
      case t: IRTrees.JSSelect             => genJSSelect(t)
      case t: IRTrees.JSFunctionApply      => genJSFunctionApply(t)
      case t: IRTrees.JSMethodApply        => genJSMethodApply(t)
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
      case _ =>
        println(tree)
        ???

      // case select: IRTrees.JSPrivateSelect => ???
      // case v: IRTrees.UnwrapFromThrowable => ???
      // case IRTrees.RecordValue(pos) =>
      // case IRTrees.JSNewTarget(pos) =>
      // case IRTrees.SelectStatic(tpe) =>
      // case IRTrees.JSSuperMethodCall(pos) =>
      // case IRTrees.Match(tpe) =>
      // case IRTrees.RecordSelect(tpe) =>
      // case IRTrees.TryFinally(pos) =>
      // case IRTrees.JSImportMeta(pos) =>
      // case IRTrees.JSSuperSelect(pos) =>
      // case IRTrees.WrapAsThrowable(pos) =>
      // case IRTrees.JSSuperConstructorCall(pos) =>
      // case IRTrees.Clone(pos) =>
      // case IRTrees.CreateJSClass(pos) =>
      // case IRTrees.Transient(pos) =>
      // case IRTrees.ForIn(pos) =>
      // case tc: IRTrees.TryCatch => ???
      // case IRTrees.JSImportCall(pos) =>
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
      case (primType: IRTypes.PrimType, _) =>
        // box
        primType match {
          case IRTypes.UndefType | IRTypes.StringType | IRTypes.NullType =>
            ()
          case PrimTypeWithBoxUnbox(primType) =>
            /* Calls a `bX` helper. Most of them are of the form
             *   bX: (x) => x
             * at the JavaScript level, but with a primType->anyref Wasm type.
             * For example, for `IntType`, `bI` has type `i32 -> anyref`. This
             * asks the JS host to turn a primitive `i32` into its generic
             * representation, which we can store in an `anyref`.
             */
            instrs += CALL(WasmImmediate.FuncIdx(WasmFunctionName.box(primType.primRef)))
          case IRTypes.CharType =>
            /* `char` and `long` are opaque to JS in the Scala.js semantics.
             * We implement them with real Wasm classes following the correct
             * vtable. Upcasting wraps a primitive into the corresponding class.
             */
            genBox(IRTypes.CharType, SpecialNames.CharBoxClass)
          case IRTypes.LongType =>
            genBox(IRTypes.LongType, SpecialNames.LongBoxClass)
          case _ =>
            println(s"adapt($primType, $expectedType)")
            ???
        }
      case _ =>
        ()
    }
  }

  private def genAssign(t: IRTrees.Assign): IRTypes.Type = {
    t.lhs match {
      case sel: IRTrees.Select =>
        val className = sel.field.name.className
        val fieldName = WasmFieldName(sel.field.name)
        val idx = ctx.getClassInfo(className).getFieldIdx(sel.field.name)

        // For Select, the receiver can never be a hijacked class, so we can use genTreeAuto
        genTreeAuto(sel.qualifier)

        genTree(t.rhs, t.lhs.tpe)
        instrs += STRUCT_SET(TypeIdx(WasmStructTypeName(className)), idx)

      case sel: IRTrees.SelectStatic => // OK?
        val className = sel.field.name.className
        val fieldName = WasmFieldName(sel.field.name)
        val idx = ctx.getClassInfo(className).getFieldIdx(sel.field.name)
        instrs += GLOBAL_GET(
          GlobalIdx(Names.WasmGlobalName.WasmModuleInstanceName.fromIR(className))
        )
        genTree(t.rhs, t.lhs.tpe)
        instrs += STRUCT_SET(TypeIdx(WasmStructTypeName(className)), idx)

      case sel: IRTrees.ArraySelect =>
        val typeName = sel.array.tpe match {
          case arrTy: IRTypes.ArrayType => WasmArrayTypeName(arrTy.arrayTypeRef)
          case _ =>
            throw new IllegalArgumentException(
              s"ArraySelect.array must be an array type, but has type ${sel.array.tpe}"
            )
        }
        genTreeAuto(sel.array)
        genTree(sel.index, IRTypes.IntType)
        genTree(t.rhs, t.lhs.tpe)
        instrs += ARRAY_SET(TypeIdx(typeName))
      case assign: IRTrees.RecordSelect    => ??? // struct.set
      case assign: IRTrees.JSPrivateSelect => ???

      case assign: IRTrees.JSSelect =>
        genTree(assign.qualifier, IRTypes.AnyType)
        genTree(assign.item, IRTypes.AnyType)
        genTree(t.rhs, IRTypes.AnyType)
        instrs += CALL(FuncIdx(WasmFunctionName.jsSelectSet))

      case assign: IRTrees.JSSuperSelect => ???

      case assign: IRTrees.JSGlobalRef =>
        genLiteral(IRTrees.StringLiteral(assign.name)(assign.pos))
        genTree(t.rhs, IRTypes.AnyType)
        instrs += CALL(FuncIdx(WasmFunctionName.jsGlobalRefSet))

      case ref: IRTrees.VarRef =>
        genTree(t.rhs, t.lhs.tpe)
        instrs += LOCAL_SET(LocalIdx(Names.WasmLocalName.fromIR(ref.ident.name)))
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

      case _ =>
        genApplyNonPrim(t)
    }
  }

  private def genApplyNonPrim(t: IRTrees.Apply): IRTypes.Type = {
    implicit val pos: Position = t.pos

    val receiverClassName = t.receiver.tpe match {
      case ClassType(className) => className
      case IRTypes.AnyType      => IRNames.ObjectClass
      case _                    => throw new Error(s"Invalid receiver type ${t.receiver.tpe}")
    }
    val receiverClassInfo = ctx.getClassInfo(receiverClassName)

    /* Similar to transformType(t.receiver.tpe), but:
     * - it is non-null, and
     * - ancestors of hijacked classes are not treated specially.
     *
     * This is used in the code paths where we have already ruled out `null`
     * values and primitive values (that implement hijacked classes).
     */
    val heapTypeForDispatch: Types.WasmHeapType = {
      if (receiverClassInfo.isInterface)
        Types.WasmHeapType.ObjectType
      else
        Types.WasmHeapType.Type(Names.WasmTypeName.WasmStructTypeName(receiverClassName))
    }

    // A local for a copy of the receiver that we will use to resolve dispatch
    val receiverLocalForDispatch: WasmLocalName = {
      val name = fctx.genSyntheticLocalName()
      val typ = Types.WasmRefType(heapTypeForDispatch)
      fctx.locals.define(WasmLocal(name, typ, isParameter = false))
      name
    }

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
      instrs += CALL(FuncIdx(funcName))
    }

    if (!receiverClassInfo.isAncestorOfHijackedClass) {
      // Standard dispatch codegen
      genReceiverNotNull()
      instrs += LOCAL_TEE(LocalIdx(receiverLocalForDispatch))
      genArgs(t.args, t.method.name)
      genTableDispatch(receiverClassInfo, t.method.name, receiverLocalForDispatch)
    } else {
      /* Hijacked class dispatch codegen
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
        def pushArgs(argsLocals: List[LocalIdx]): Unit =
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
            val argsLocals: List[LocalIdx] =
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
            CastFlags(false, false),
            labelNotOurObject,
            WasmImmediate.HeapType(Types.WasmHeapType.Simple.Any),
            WasmImmediate.HeapType(heapTypeForDispatch)
          )
          instrs += LOCAL_TEE(LocalIdx(receiverLocalForDispatch))
          pushArgs(argsLocals)
          genTableDispatch(receiverClassInfo, t.method.name, receiverLocalForDispatch)
          instrs += BR(labelDone)

          argsLocals
        } // end block labelNotOurObject

        // Now we have a value that is not one of our objects; the (ref any) is still on the stack

        if (t.method.name == toStringMethodName) {
          // By spec, toString() is special
          assert(argsLocals.isEmpty)
          instrs += CALL(FuncIdx(WasmFunctionName.jsValueToString))
        } else if (receiverClassName == JLNumberClass) {
          // the value must be a `number`, hence we can unbox to `double`
          genUnbox(IRTypes.DoubleType)
          pushArgs(argsLocals)
          genHijackedClassCall(IRNames.BoxedDoubleClass)
        } else if (receiverClassName == CharSequenceClass) {
          // the value must be a `string`; it already has the right type
          pushArgs(argsLocals)
          genHijackedClassCall(IRNames.BoxedStringClass)
        } else {
          /* It must be a method of j.l.Object and it can be any value.
           * hashCode() and equals() are overridden in all hijacked classes; we
           * use dedicated JavaScript helpers for those.
           * The other methods are never overridden and can be statically
           * resolved to j.l.Object.
           */
          pushArgs(argsLocals)
          t.method.name match {
            case `hashCodeMethodName` =>
              instrs += CALL(FuncIdx(WasmFunctionName.jsValueHashCode))
            case `equalsMethodName` =>
              instrs += CALL(FuncIdx(WasmFunctionName.is))
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
      val itables = ctx.calculateClassItables(clazz = receiverClassInfo.name)

      val (itableIdx, methodIdx) = itables.resolveMethod(methodName)
      val targetClass = itables.itables(itableIdx)
      val method = targetClass.methods(
        methodIdx
      ) // should be safe since resolveMethod should return valid value

      // val methodIdx = info.methods.indexWhere(i => i.name.methodName == t.method.name.nameString)
      // if (methodIdx < 0) { throw new Error(s"Cannot find method ${t.method}") }
      // val method = info.methods(methodIdx)

      // val rttReceiverClassName =
      //   TypeTransformer.transformType(t.receiver.tpe)(ctx) match {
      //     case Types.WasmRefNullType(Types.WasmHeapType.Type(name @ WasmStructTypeName(_))) => name
      //     case Types.WasmRefType(Types.WasmHeapType.Type(name @ WasmStructTypeName(_)))     => name
      //     case _ => throw new Error(s"Invalid receiver type ${t.receiver.tpe}")
      //   }

      instrs += LOCAL_GET(LocalIdx(receiverLocalForDispatch))
      instrs += STRUCT_GET(
        // receiver type should be upcasted into `Object` if it's interface
        // by TypeTransformer#transformType
        TypeIdx(WasmStructTypeName(IRNames.ObjectClass)),
        StructFieldIdx(1)
      )
      instrs += I32_CONST(I32(itableIdx))
      instrs += ARRAY_GET(
        TypeIdx(WasmArrayType.itables.name)
      )
      instrs += REF_CAST(
        HeapType(Types.WasmHeapType.Type(WasmITableTypeName(targetClass.name)))
      )
      instrs += STRUCT_GET(
        TypeIdx(WasmITableTypeName(targetClass.name)),
        StructFieldIdx(methodIdx)
      )
      instrs += CALL_REF(
        TypeIdx(method.toWasmFunctionType()(ctx).name)
      )
    }

    // Generates a vtable-based dispatch.
    def genVTableDispatch(): Unit = {
      val receiverClassName = receiverClassInfo.name

      val (methodIdx, info) = ctx
        .calculateVtableType(receiverClassName)
        .resolveWithIdx(WasmFunctionName(
          IRTrees.MemberNamespace.Public,
          receiverClassName,
          methodName
        ))

      // // push args to the stacks
      // local.get $this ;; for accessing funcref
      // local.get $this ;; for accessing vtable
      // struct.get $classType 0 ;; get vtable
      // struct.get $vtableType $methodIdx ;; get funcref
      // call.ref (type $funcType) ;; call funcref
      instrs += LOCAL_GET(LocalIdx(receiverLocalForDispatch))
      instrs += REF_CAST(
        HeapType(Types.WasmHeapType.Type(WasmStructTypeName(receiverClassName)))
      )
      instrs += STRUCT_GET(
        TypeIdx(WasmStructTypeName(receiverClassName)),
        StructFieldIdx(0)
      )
      instrs += STRUCT_GET(
        TypeIdx(WasmVTableTypeName(receiverClassName)),
        StructFieldIdx(WasmStructType.typeDataFieldCount + methodIdx)
      )
      instrs += CALL_REF(
        TypeIdx(info.toWasmFunctionType()(ctx).name)
      )
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
        IRTypes.BoxedClassToPrimType.get(t.className) match {
          case None =>
            genTree(t.receiver, IRTypes.ClassType(t.className))
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
        val namespace = IRTrees.MemberNamespace.forNonStaticCall(t.flags)
        val funcName = Names.WasmFunctionName(namespace, t.className, t.method.name)
        instrs += CALL(FuncIdx(funcName))
        if (t.tpe == IRTypes.NothingType)
          instrs += UNREACHABLE
        t.tpe
    }
  }

  private def genApplyStatic(tree: IRTrees.ApplyStatic): IRTypes.Type = {
    genArgs(tree.args, tree.method.name)
    val namespace = IRTrees.MemberNamespace.forStaticCall(tree.flags)
    val funcName = Names.WasmFunctionName(namespace, tree.className, tree.method.name)
    instrs += CALL(FuncIdx(funcName))
    if (tree.tpe == IRTypes.NothingType)
      instrs += UNREACHABLE
    tree.tpe
  }

  private def genArgs(args: List[IRTrees.Tree], methodName: IRNames.MethodName): Unit = {
    for ((arg, paramTypeRef) <- args.lazyZip(methodName.paramTypeRefs)) {
      val paramType = ctx.inferTypeFromTypeRef(paramTypeRef)
      genTree(arg, paramType)
    }
  }

  private def genLiteral(l: IRTrees.Literal): IRTypes.Type = {
    l match {
      case IRTrees.BooleanLiteral(v) => instrs += WasmInstr.I32_CONST(if (v) I32(1) else I32(0))
      case IRTrees.ByteLiteral(v)    => instrs += WasmInstr.I32_CONST(I32(v))
      case IRTrees.ShortLiteral(v)   => instrs += WasmInstr.I32_CONST(I32(v))
      case IRTrees.IntLiteral(v)     => instrs += WasmInstr.I32_CONST(I32(v))
      case IRTrees.CharLiteral(v)    => instrs += WasmInstr.I32_CONST(I32(v))
      case IRTrees.LongLiteral(v)    => instrs += WasmInstr.I64_CONST(I64(v))
      case IRTrees.FloatLiteral(v)   => instrs += WasmInstr.F32_CONST(F32(v))
      case IRTrees.DoubleLiteral(v)  => instrs += WasmInstr.F64_CONST(F64(v))

      case v: IRTrees.Undefined =>
        instrs += CALL(FuncIdx(WasmFunctionName.undef))
      case v: IRTrees.Null =>
        instrs += WasmInstr.REF_NULL(HeapType(Types.WasmHeapType.Simple.None))

      case v: IRTrees.StringLiteral =>
        instrs += ctx.getConstantStringInstr(v.value)

      case v: IRTrees.ClassOf =>
        v.typeRef match {
          case typeRef: IRTypes.NonArrayTypeRef =>
            genClassOfFromTypeData(getNonArrayTypeDataInstr(typeRef))

          case IRTypes.ArrayTypeRef(base, dimensions) =>
            val typeDataType =
              Types.WasmRefType(Types.WasmHeapType.Type(WasmStructTypeName.typeData))
            val typeDataLocal = fctx.addSyntheticLocal(typeDataType)

            instrs += getNonArrayTypeDataInstr(base)
            instrs += I32_CONST(I32(dimensions))
            instrs += CALL(FuncIdx(WasmFunctionName.arrayTypeData))
            instrs += LOCAL_SET(typeDataLocal)
            genClassOfFromTypeData(LOCAL_GET(typeDataLocal))
        }
    }

    l.tpe
  }

  private def getNonArrayTypeDataInstr(typeRef: IRTypes.NonArrayTypeRef): WasmInstr =
    GLOBAL_GET(GlobalIdx(WasmGlobalName.WasmGlobalVTableName(typeRef)))

  private def genClassOfFromTypeData(loadTypeDataInstr: WasmInstr): Unit = {
    fctx.block(Types.WasmRefType(Types.WasmHeapType.ClassType)) { nonNullLabel =>
      // fast path first
      instrs += loadTypeDataInstr
      instrs += STRUCT_GET(TypeIdx(WasmStructTypeName.typeData), WasmFieldName.typeData.classOfIdx)
      instrs += BR_ON_NON_NULL(nonNullLabel)
      // slow path
      instrs += loadTypeDataInstr
      instrs += CALL(FuncIdx(WasmFunctionName.createClassOf))
    }
  }

  private def genSelect(sel: IRTrees.Select): IRTypes.Type = {
    val className = sel.field.name.className
    val fieldName = WasmFieldName(sel.field.name)
    val idx = ctx.getClassInfo(className).getFieldIdx(sel.field.name)

    // For Select, the receiver can never be a hijacked class, so we can use genTreeAuto
    genTreeAuto(sel.qualifier)

    instrs += STRUCT_GET(TypeIdx(WasmStructTypeName(className)), idx)
    sel.tpe
  }

  private def genStoreModule(t: IRTrees.StoreModule): IRTypes.Type = {
    val className = fctx.enclosingClassName.getOrElse {
      throw new AssertionError(s"Cannot emit $t at ${t.pos} without enclosing class name")
    }
    val name = WasmGlobalName.WasmModuleInstanceName.fromIR(className)
    genTreeAuto(IRTrees.This()(IRTypes.ClassType(className))(t.pos))
    instrs += GLOBAL_SET(GlobalIdx(name))
    IRTypes.NoType
  }

  /** Push module class instance to the stack.
    *
    * see: WasmBuilder.genLoadModuleFunc
    */
  private def genLoadModule(t: IRTrees.LoadModule): IRTypes.Type = {
    instrs += CALL(FuncIdx(Names.WasmFunctionName.loadModule(t.className)))
    t.tpe
  }

  private def genUnaryOp(unary: IRTrees.UnaryOp): IRTypes.Type = {
    import IRTrees.UnaryOp._

    genTreeAuto(unary.lhs)

    (unary.op: @switch) match {
      case Boolean_! =>
        instrs += I32_CONST(I32(1))
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
        instrs += I32_CONST(I32(0xffff))
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
        instrs += CALL(FuncIdx(WasmFunctionName.stringLength))
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

      // New in 1.11
      case BinaryOp.String_charAt =>
        genTree(binary.lhs, IRTypes.StringType) // push the string
        genTree(binary.rhs, IRTypes.IntType) // push the index
        instrs += CALL(FuncIdx(WasmFunctionName.stringCharAt))
        IRTypes.CharType

      case _ => genElementaryBinaryOp(binary)
    }
  }

  private def genEq(binary: IRTrees.BinaryOp): IRTypes.Type = {
    // TODO Optimize this when the operands have a better type than `any`
    genTree(binary.lhs, IRTypes.AnyType)
    genTree(binary.rhs, IRTypes.AnyType)
    instrs += CALL(FuncIdx(WasmFunctionName.is))

    if (binary.op == IRTrees.BinaryOp.!==) {
      instrs += I32_CONST(I32(1))
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

      // TODO
      // get_local 0
      // get_local 1
      // f32.div
      // f32.trunc
      // get_local 1
      // f32.mul
      // f32.sub
      case BinaryOp.Float_% => ???

      case BinaryOp.Double_+ => F64_ADD
      case BinaryOp.Double_- => F64_SUB
      case BinaryOp.Double_* => F64_MUL
      case BinaryOp.Double_/ => F64_DIV
      case BinaryOp.Double_% => ??? // TODO same as Float_%

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
        val receiverLocalForDispatch: WasmLocalName = {
          val name = fctx.genSyntheticLocalName()
          val typ = Types.WasmRefType(Types.WasmHeapType.ObjectType)
          fctx.locals.define(WasmLocal(name, typ, isParameter = false))
          name
        }

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
              instrs += LOCAL_TEE(LocalIdx(receiverLocalForDispatch))
              genTableDispatch(objectClassInfo, toStringMethodName, receiverLocalForDispatch)
              instrs += BR_ON_NON_NULL(labelDone)
            }

            genLiteral(IRTrees.StringLiteral("null")(tree.pos))
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
            fctx.block(Types.WasmAnyRef) { labelNotOurObject =>
              // Load receiver
              genTreeAuto(tree)

              instrs += BR_ON_CAST_FAIL(
                CastFlags(true, false),
                labelNotOurObject,
                WasmImmediate.HeapType(Types.WasmHeapType.Simple.Any),
                WasmImmediate.HeapType(Types.WasmHeapType.ObjectType)
              )
              instrs += LOCAL_TEE(LocalIdx(receiverLocalForDispatch))
              genTableDispatch(objectClassInfo, toStringMethodName, receiverLocalForDispatch)
              instrs += BR_ON_NON_NULL(labelDone)
              instrs += REF_NULL(HeapType(Types.WasmHeapType.Simple.Any))
            } // end block labelNotOurObject

            // Now we have a value that is not one of our objects; the anyref is still on the stack
            instrs += CALL(FuncIdx(WasmFunctionName.jsValueToString))
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
              instrs += CALL(FuncIdx(WasmFunctionName.booleanToString))
            case IRTypes.CharType =>
              instrs += CALL(FuncIdx(WasmFunctionName.charToString))
            case IRTypes.ByteType | IRTypes.ShortType | IRTypes.IntType =>
              instrs += CALL(FuncIdx(WasmFunctionName.intToString))
            case IRTypes.LongType =>
              instrs += CALL(FuncIdx(WasmFunctionName.longToString))
            case IRTypes.FloatType =>
              instrs += F64_PROMOTE_F32
              instrs += CALL(FuncIdx(WasmFunctionName.doubleToString))
            case IRTypes.DoubleType =>
              instrs += CALL(FuncIdx(WasmFunctionName.doubleToString))
            case IRTypes.NullType | IRTypes.UndefType =>
              instrs += CALL(FuncIdx(WasmFunctionName.jsValueToString))
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
          instrs += CALL(FuncIdx(WasmFunctionName.jsValueToString)) // for `null`

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
        instrs += CALL(FuncIdx(WasmFunctionName.stringConcat))
    }

    IRTypes.StringType
  }

  private def genIsInstanceOf(tree: IRTrees.IsInstanceOf): IRTypes.Type = {
    genTree(tree.expr, IRTypes.AnyType)

    def genIsPrimType(testType: IRTypes.PrimType): Unit = {
      testType match {
        case IRTypes.UndefType =>
          instrs += CALL(FuncIdx(WasmFunctionName.isUndef))
        case PrimTypeWithBoxUnbox(testType) =>
          /* Calls the appromriate `tX` JS helper. It dynamically tests whether
           * the value fits in the given primitive type, according to
           * https://www.scala-js.org/doc/semantics.html
           * All the `tX` helpers have Wasm type `anyref -> i32` (interpreted as `boolean`).
           */
          instrs += CALL(FuncIdx(WasmFunctionName.typeTest(testType.primRef)))
        case IRTypes.StringType =>
          instrs += CALL(FuncIdx(WasmFunctionName.isString))
        case IRTypes.CharType =>
          val structTypeName = WasmStructTypeName(SpecialNames.CharBoxClass)
          instrs += REF_TEST(HeapType(Types.WasmHeapType.Type(structTypeName)))
        case IRTypes.LongType =>
          val structTypeName = WasmStructTypeName(SpecialNames.LongBoxClass)
          instrs += REF_TEST(HeapType(Types.WasmHeapType.Type(structTypeName)))
        case _ =>
          println(tree)
          ???
      }
    }

    tree.testType match {
      case testType: IRTypes.PrimType =>
        genIsPrimType(testType)

      case IRTypes.AnyType | IRTypes.ClassType(IRNames.ObjectClass) =>
        instrs += REF_IS_NULL
        instrs += I32_CONST(I32(1))
        instrs += I32_XOR

      case IRTypes.ClassType(testClassName) =>
        IRTypes.BoxedClassToPrimType.get(testClassName) match {
          case Some(primType) =>
            genIsPrimType(primType)
          case None =>
            val info = ctx.getClassInfo(testClassName)
            if (info.isInterface) {
              // TODO: run-time type test for interface
              println(tree)
              ???
            } else {
              instrs += REF_TEST(
                HeapType(Types.WasmHeapType.Type(WasmStructTypeName(testClassName)))
              )
            }
        }

      case IRTypes.ArrayType(_) =>
        println(tree)
        ???

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

        case IRTypes.AnyType | IRTypes.ClassType(IRNames.ObjectClass) =>
          ()

        case IRTypes.ClassType(targetClassName) =>
          val info = ctx.getClassInfo(targetClassName)
          if (info.kind.isClass) {
            instrs += REF_CAST_NULL(
              HeapType(Types.WasmHeapType.Type(WasmStructTypeName(targetClassName)))
            )
          } else if (info.kind == ClassKind.HijackedClass) {
            IRTypes.BoxedClassToPrimType(targetClassName) match {
              case IRTypes.UndefType | IRTypes.StringType =>
                ()
              case PrimTypeWithBoxUnbox(primType) =>
                instrs += CALL(
                  WasmImmediate.FuncIdx(WasmFunctionName.unboxOrNull(primType.primRef))
                )
              case IRTypes.CharType =>
                val structTypeName = WasmStructTypeName(SpecialNames.CharBoxClass)
                instrs += REF_CAST_NULL(HeapType(Types.WasmHeapType.Type(structTypeName)))
              case IRTypes.LongType =>
                val structTypeName = WasmStructTypeName(SpecialNames.LongBoxClass)
                instrs += REF_CAST_NULL(HeapType(Types.WasmHeapType.Type(structTypeName)))
              case _ =>
                println(tree)
                ???
            }
          }

        case IRTypes.ArrayType(_) =>
          println(tree)
          ???

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
        genLiteral(IRTrees.Undefined())
      case PrimTypeWithBoxUnbox(targetTpe) =>
        instrs += CALL(WasmImmediate.FuncIdx(WasmFunctionName.unbox(targetTpe.primRef)))
      case IRTypes.StringType =>
        instrs += REF_AS_NOT_NULL
      case IRTypes.CharType =>
        // Extract the `value` field (the only field) out of the box class.
        // TODO Handle null
        val structTypeName = WasmStructTypeName(SpecialNames.CharBoxClass)
        instrs += REF_CAST(HeapType(Types.WasmHeapType.Type(structTypeName)))
        instrs += STRUCT_GET(TypeIdx(structTypeName), StructFieldIdx(2))
      case IRTypes.LongType =>
        // TODO Handle null
        val structTypeName = WasmStructTypeName(SpecialNames.LongBoxClass)
        instrs += REF_CAST(HeapType(Types.WasmHeapType.Type(structTypeName)))
        instrs += STRUCT_GET(TypeIdx(structTypeName), StructFieldIdx(2))
      case IRTypes.NothingType | IRTypes.NullType | IRTypes.NoType =>
        throw new IllegalArgumentException(s"Illegal type in genUnbox: $targetTpe")
      case _ =>
        println(s"genUnbox($targetTpe)")
        ???
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
      val typeDataType = Types.WasmRefType(Types.WasmHeapType.Type(WasmStructTypeName.typeData))
      val objectTypeIdx = TypeIdx(WasmStructTypeName(IRNames.ObjectClass))

      val typeDataLocal = fctx.addSyntheticLocal(typeDataType)

      genTreeAuto(tree.expr)
      instrs += STRUCT_GET(objectTypeIdx, StructFieldIdx(0)) // implicit trap on null
      instrs += LOCAL_SET(typeDataLocal)
      genClassOfFromTypeData(LOCAL_GET(typeDataLocal))
    } else {
      genTree(tree.expr, IRTypes.AnyType)
      instrs += REF_AS_NOT_NULL
      instrs += CALL(FuncIdx(WasmFunctionName.anyGetClass))
    }

    tree.tpe
  }

  private def genVarRef(r: IRTrees.VarRef): IRTypes.Type = {
    val name = WasmLocalName.fromIR(r.ident.name)
    instrs += LOCAL_GET(LocalIdx(name))
    r.tpe
  }

  private def genThis(t: IRTrees.This): IRTypes.Type = {
    instrs += LOCAL_GET(LocalIdx(fctx.receiver.name))

    /* If the receiver is a Class/ModuleClass, its wasm type will be declared
     * as `(ref any)`, and therefore we must cast it down.
     */
    t.tpe match {
      case IRTypes.ClassType(className) if className != IRNames.ObjectClass =>
        val info = ctx.getClassInfo(className)
        if (info.kind.isClass) {
          instrs += REF_CAST(
            HeapType(Types.WasmHeapType.Type(WasmStructTypeName(className)))
          )
        } else if (info.isInterface) {
          instrs += REF_CAST(
            HeapType(Types.WasmHeapType.ObjectType)
          )
        }
      case _ =>
        ()
    }

    t.tpe
  }

  private def genVarDef(r: IRTrees.VarDef): IRTypes.Type = {
    val localIdx = fctx.addLocal(r.name.name, TypeTransformer.transformType(r.vtpe)(ctx))

    genTree(r.rhs, r.vtpe)
    instrs += LOCAL_SET(localIdx)

    IRTypes.NoType
  }

  private def genIf(t: IRTrees.If, expectedType: IRTypes.Type): IRTypes.Type = {
    val ty = TypeTransformer.transformType(expectedType)(ctx)
    genTree(t.cond, IRTypes.BooleanType)
    fctx.ifThenElse(ty) {
      genTree(t.thenp, expectedType)
    } {
      genTree(t.elsep, expectedType)
    }
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

  private def genBlock(t: IRTrees.Block, expectedType: IRTypes.Type): IRTypes.Type = {
    for (stat <- t.stats.init)
      genTree(stat, IRTypes.NoType)
    genTree(t.stats.last, expectedType)
    expectedType
  }

  private def genLabeled(t: IRTrees.Labeled, expectedType: IRTypes.Type): IRTypes.Type = {
    val label = fctx.registerLabel(t.label.name, expectedType)
    val ty = TypeTransformer.transformType(t.tpe)(ctx)

    // Manual BLOCK here because we have a specific `label`
    instrs += BLOCK(BlockType.ValueType(ty), Some(label))
    genTree(t.body, expectedType)
    instrs += END
    expectedType
  }

  private def genReturn(t: IRTrees.Return): IRTypes.Type = {
    val (label, expectedType) = fctx.getLabelFor(t.label.name)

    genTree(t.expr, expectedType)
    instrs += BR(label)
    IRTypes.NothingType
  }

  private def genNew(n: IRTrees.New): IRTypes.Type = {
    val localInstance = WasmLocal(
      fctx.genSyntheticLocalName(),
      TypeTransformer.transformType(n.tpe)(ctx),
      isParameter = false
    )
    fctx.locals.define(localInstance)

    // REF_NULL(HeapType(Types.WasmHeapType.Type(WasmTypeName.WasmStructTypeName(n.className)))),
    // LOCAL_TEE(LocalIdx(localInstance.name))
    instrs += CALL(FuncIdx(WasmFunctionName.newDefault(n.className)))
    instrs += LOCAL_TEE(LocalIdx(localInstance.name))
    genArgs(n.args, n.ctor.name)
    instrs += CALL(FuncIdx(WasmFunctionName(
      IRTrees.MemberNamespace.Constructor,
      n.className,
      n.ctor.name
    )))
    instrs += LOCAL_GET(LocalIdx(localInstance.name))
    n.tpe
  }

  /** Codegen to box a primitive `char`/`long` into a `CharacterBox`/`LongBox`. */
  private def genBox(
      primType: IRTypes.PrimTypeWithRef,
      boxClassName: IRNames.ClassName
  ): IRTypes.Type = {
    // `primTyp` is `i32` for `char` (containing a `u16` value) or `i64` for `long`.
    val primTyp = TypeTransformer.transformType(primType)(ctx)
    val primLocal = WasmLocal(fctx.genSyntheticLocalName(), primTyp, isParameter = false)
    fctx.locals.define(primLocal)

    val boxClassType = IRTypes.ClassType(boxClassName)
    val boxTyp = TypeTransformer.transformType(boxClassType)(ctx)
    val instanceLocal = WasmLocal(fctx.genSyntheticLocalName(), boxTyp, isParameter = false)
    fctx.locals.define(instanceLocal)

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

    instrs += LOCAL_SET(LocalIdx(primLocal.name))
    instrs += CALL(FuncIdx(WasmFunctionName.newDefault(boxClassName)))
    instrs += LOCAL_TEE(LocalIdx(instanceLocal.name))
    instrs += LOCAL_GET(LocalIdx(primLocal.name))
    instrs += STRUCT_SET(TypeIdx(WasmStructTypeName(boxClassName)), StructFieldIdx(2))
    instrs += LOCAL_GET(LocalIdx(instanceLocal.name))

    boxClassType
  }

  private def genIdentityHashCode(tree: IRTrees.IdentityHashCode): IRTypes.Type = {
    /* TODO We should allocate ID hash codes and store them. We will probably
     * have to store them as an additional field in all objects, together with
     * the vtable and itable pointers.
     */
    genTree(tree.expr, IRTypes.AnyType)
    instrs += DROP
    instrs += I32_CONST(I32(42))

    IRTypes.IntType
  }

  private def genJSNew(tree: IRTrees.JSNew): IRTypes.Type = {
    genTree(tree.ctor, IRTypes.AnyType)
    genJSArgsArray(tree.args)
    instrs += CALL(FuncIdx(WasmFunctionName.jsNew))
    IRTypes.AnyType
  }

  private def genJSSelect(tree: IRTrees.JSSelect): IRTypes.Type = {
    genTree(tree.qualifier, IRTypes.AnyType)
    genTree(tree.item, IRTypes.AnyType)
    instrs += CALL(FuncIdx(WasmFunctionName.jsSelect))
    IRTypes.AnyType
  }

  private def genJSFunctionApply(tree: IRTrees.JSFunctionApply): IRTypes.Type = {
    genTree(tree.fun, IRTypes.AnyType)
    genJSArgsArray(tree.args)
    instrs += CALL(FuncIdx(WasmFunctionName.jsFunctionApply))
    IRTypes.AnyType
  }

  private def genJSMethodApply(tree: IRTrees.JSMethodApply): IRTypes.Type = {
    genTree(tree.receiver, IRTypes.AnyType)
    genTree(tree.method, IRTypes.AnyType)
    genJSArgsArray(tree.args)
    instrs += CALL(FuncIdx(WasmFunctionName.jsMethodApply))
    IRTypes.AnyType
  }

  private def genLoadJSConstructor(tree: IRTrees.LoadJSConstructor): IRTypes.Type = {
    val info = ctx.getClassInfo(tree.className)
    val jsNativeLoadSpec = info.jsNativeLoadSpec.getOrElse {
      throw new AssertionError(s"Found $tree for class without jsNativeLoadSpec at ${tree.pos}")
    }
    genLoadJSNativeLoadSpec(jsNativeLoadSpec)(tree.pos)
  }

  private def genLoadJSModule(tree: IRTrees.LoadJSModule): IRTypes.Type = {
    val info = ctx.getClassInfo(tree.className)
    val jsNativeLoadSpec = info.jsNativeLoadSpec.getOrElse {
      throw new AssertionError(s"Found $tree for class without jsNativeLoadSpec at ${tree.pos}")
    }
    genLoadJSNativeLoadSpec(jsNativeLoadSpec)(tree.pos)
  }

  private def genSelectJSNativeMember(tree: IRTrees.SelectJSNativeMember): IRTypes.Type = {
    val info = ctx.getClassInfo(tree.className)
    val jsNativeLoadSpec = info.jsNativeMembers.getOrElse(
      tree.member.name, {
        throw new AssertionError(s"Found $tree for non-existing JS native member at ${tree.pos}")
      }
    )
    genLoadJSNativeLoadSpec(jsNativeLoadSpec)(tree.pos)
  }

  private def genLoadJSNativeLoadSpec(loadSpec: IRTrees.JSNativeLoadSpec)(implicit
      pos: Position
  ): IRTypes.Type = {
    import IRTrees.JSNativeLoadSpec._

    def genFollowPath(path: List[String]): Unit = {
      for (prop <- path) {
        genLiteral(IRTrees.StringLiteral(prop))
        instrs += CALL(FuncIdx(WasmFunctionName.jsSelect))
      }
    }

    loadSpec match {
      case Global(globalRef, path) =>
        genLiteral(IRTrees.StringLiteral(globalRef))
        instrs += CALL(FuncIdx(WasmFunctionName.jsGlobalRefGet))
        genFollowPath(path)
        IRTypes.AnyType
      case Import(module, path) =>
        ???
      case ImportWithGlobalFallback(importSpec, globalSpec) =>
        genLoadJSNativeLoadSpec(globalSpec)
    }
  }

  private def genJSDelete(tree: IRTrees.JSDelete): IRTypes.Type = {
    genTree(tree.qualifier, IRTypes.AnyType)
    genTree(tree.item, IRTypes.AnyType)
    instrs += CALL(FuncIdx(WasmFunctionName.jsDelete))
    IRTypes.NoType
  }

  private def genJSUnaryOp(tree: IRTrees.JSUnaryOp): IRTypes.Type = {
    genTree(tree.lhs, IRTypes.AnyType)
    instrs += CALL(FuncIdx(WasmFunctionName.jsUnaryOps(tree.op)))
    IRTypes.AnyType
  }

  private def genJSBinaryOp(tree: IRTrees.JSBinaryOp): IRTypes.Type = {
    import IRTrees.JSBinaryOp

    tree.op match {
      case JSBinaryOp.|| | JSBinaryOp.&& =>
        /* Here we need to implement the short-circuiting behavior, with a
         * condition based on the truthy value of the left-hand-side.
         */
        val lhsLocal = fctx.genSyntheticLocalName()
        fctx.locals.define(WasmLocal(lhsLocal, Types.WasmAnyRef, isParameter = false))
        genTree(tree.lhs, IRTypes.AnyType)
        instrs += LOCAL_TEE(LocalIdx(lhsLocal))
        instrs += CALL(FuncIdx(WasmFunctionName.jsIsTruthy))
        instrs += IF(BlockType.ValueType(Types.WasmAnyRef))
        if (tree.op == JSBinaryOp.||) {
          instrs += LOCAL_GET(LocalIdx(lhsLocal))
          instrs += ELSE
          genTree(tree.rhs, IRTypes.AnyType)
        } else {
          genTree(tree.rhs, IRTypes.AnyType)
          instrs += ELSE
          instrs += LOCAL_GET(LocalIdx(lhsLocal))
        }
        instrs += END

      case _ =>
        genTree(tree.lhs, IRTypes.AnyType)
        genTree(tree.rhs, IRTypes.AnyType)
        instrs += CALL(FuncIdx(WasmFunctionName.jsBinaryOps(tree.op)))
    }

    tree.tpe
  }

  private def genJSArrayConstr(tree: IRTrees.JSArrayConstr): IRTypes.Type = {
    genJSArgsArray(tree.items)
    IRTypes.AnyType
  }

  private def genJSObjectConstr(tree: IRTrees.JSObjectConstr): IRTypes.Type = {
    instrs += CALL(FuncIdx(WasmFunctionName.jsNewObject))
    for ((prop, value) <- tree.fields) {
      genTree(prop, IRTypes.AnyType)
      genTree(value, IRTypes.AnyType)
      instrs += CALL(FuncIdx(WasmFunctionName.jsObjectPush))
    }
    IRTypes.AnyType
  }

  private def genJSGlobalRef(tree: IRTrees.JSGlobalRef): IRTypes.Type = {
    genLiteral(IRTrees.StringLiteral(tree.name)(tree.pos))
    instrs += CALL(FuncIdx(WasmFunctionName.jsGlobalRefGet))
    IRTypes.AnyType
  }

  private def genJSTypeOfGlobalRef(tree: IRTrees.JSTypeOfGlobalRef): IRTypes.Type = {
    genLiteral(IRTrees.StringLiteral(tree.globalRef.name)(tree.pos))
    instrs += CALL(FuncIdx(WasmFunctionName.jsGlobalRefTypeof))
    IRTypes.AnyType
  }

  private def genJSArgsArray(args: List[IRTrees.TreeOrJSSpread]): Unit = {
    instrs += CALL(FuncIdx(WasmFunctionName.jsNewArray))
    for (arg <- args) {
      arg match {
        case arg: IRTrees.Tree =>
          genTree(arg, IRTypes.AnyType)
          instrs += CALL(FuncIdx(WasmFunctionName.jsArrayPush))
        case IRTrees.JSSpread(items) =>
          genTree(items, IRTypes.AnyType)
          instrs += CALL(FuncIdx(WasmFunctionName.jsArraySpreadPush))
      }
    }
  }

  private def genJSLinkingInfo(tree: IRTrees.JSLinkingInfo): IRTypes.Type = {
    instrs += CALL(FuncIdx(WasmFunctionName.jsLinkingInfo))
    IRTypes.AnyType
  }

  // ===============================================================================
  // array
  // ===============================================================================
  private def genArrayLength(t: IRTrees.ArrayLength): IRTypes.Type = {
    genTreeAuto(t.array)
    instrs += ARRAY_LEN
    IRTypes.IntType
  }

  private def genNewArray(t: IRTrees.NewArray): IRTypes.Type = {
    if (t.lengths.isEmpty || t.lengths.sizeIs > t.typeRef.dimensions)
      throw new AssertionError(
        s"invalid lengths ${t.lengths} for array type ${t.typeRef.displayName}"
      )
    if (t.lengths.size == 1) {
      val arrTy = ctx.getArrayType(t.typeRef)
      val zero = Defaults.defaultValue(arrTy.field.typ)
      ctx.inferTypeFromTypeRef(t.typeRef.base)
      instrs += zero
      genTree(t.lengths.head, IRTypes.IntType)
      instrs += ARRAY_NEW(TypeIdx(arrTy.name))
    } else ??? // TODO support multi dimensional arrays
    t.tpe
  }

  /** For getting element from an array, array.set should be generated by transformation of
    * `Assign(ArraySelect(...), ...)`
    */
  private def genArraySelect(t: IRTrees.ArraySelect): IRTypes.Type = {
    val irArrType = t.array.tpe match {
      case t: IRTypes.ArrayType => t
      case _ =>
        throw new IllegalArgumentException(
          s"ArraySelect.array must be an array type, but has type ${t.array.tpe}"
        )
    }
    genTreeAuto(t.array)
    genTree(t.index, IRTypes.IntType)
    instrs += ARRAY_GET(TypeIdx(ctx.getArrayType(irArrType.arrayTypeRef).name))

    val typeRef = irArrType.arrayTypeRef
    if (typeRef.dimensions > 1)
      IRTypes.ArrayType(typeRef.copy(dimensions = typeRef.dimensions - 1))
    else ctx.inferTypeFromTypeRef(typeRef.base)
  }

  private def genArrayValue(t: IRTrees.ArrayValue): IRTypes.Type = {
    val arrTy = ctx.getArrayType(t.typeRef)
    t.elems.foreach(genTreeAuto)
    instrs += ARRAY_NEW_FIXED(TypeIdx(arrTy.name), I32(t.elems.size))
    t.tpe
  }

  private def genClosure(tree: IRTrees.Closure): IRTypes.Type = {
    implicit val ctx = this.ctx

    val hasThis = !tree.arrow
    val hasRestParam = tree.restParam.isDefined
    val dataStructType = ctx.getClosureDataStructType(tree.captureParams.map(_.ptpe))

    // Define the function where captures are reified as a `__captureData` argument.
    val closureFuncName = fctx.genInnerFuncName()
    locally {
      val receiverParam =
        if (!hasThis) None
        else Some(WasmLocal(WasmLocalName.receiver, Types.WasmAnyRef, isParameter = true))

      val captureDataParam = WasmLocal(
        WasmLocalName("__captureData"),
        Types.WasmRefType(Types.WasmHeapType.Type(dataStructType.name)),
        isParameter = true
      )

      val paramLocals = (tree.params ::: tree.restParam.toList).map { param =>
        val typ = TypeTransformer.transformType(param.ptpe)
        WasmLocal(WasmLocalName.fromIR(param.name.name), typ, isParameter = true)
      }
      val resultTyps = TypeTransformer.transformResultType(IRTypes.AnyType)

      implicit val fctx = WasmFunctionContext(
        enclosingClassName = None,
        closureFuncName,
        receiverParam,
        captureDataParam :: paramLocals,
        resultTyps
      )

      val captureDataLocalIdx = fctx.paramIndices.head

      // Extract the fields of captureData in individual locals
      for ((captureParam, index) <- tree.captureParams.zipWithIndex) {
        val local = fctx.addLocal(
          captureParam.name.name,
          TypeTransformer.transformType(captureParam.ptpe)
        )
        fctx.instrs += LOCAL_GET(captureDataLocalIdx)
        fctx.instrs += STRUCT_GET(TypeIdx(dataStructType.name), StructFieldIdx(index))
        fctx.instrs += LOCAL_SET(local)
      }

      // Now transform the body - use AnyType as result type to box potential primitives
      WasmExpressionBuilder.generateIRBody(tree.body, IRTypes.AnyType)

      fctx.buildAndAddToContext()
    }

    // Put a reference to the function on the stack
    instrs += ctx.refFuncWithDeclaration(closureFuncName)

    // Evaluate the capture values and instantiate the capture data struct
    for ((param, value) <- tree.captureParams.zip(tree.captureValues))
      genTree(value, param.ptpe)
    instrs += STRUCT_NEW(TypeIdx(dataStructType.name))

    /* If there is a ...rest param, the helper requires as third argument the
     * number of regular arguments.
     */
    if (hasRestParam)
      instrs += I32_CONST(I32(tree.params.size))

    // Call the appropriate helper
    val helper = (hasThis, hasRestParam) match {
      case (false, false) => WasmFunctionName.closure
      case (true, false)  => WasmFunctionName.closureThis
      case (false, true)  => WasmFunctionName.closureRest
      case (true, true)   => WasmFunctionName.closureThisRest
    }
    instrs += CALL(FuncIdx(helper))

    IRTypes.AnyType
  }
}
