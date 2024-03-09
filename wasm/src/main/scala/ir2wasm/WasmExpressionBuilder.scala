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

object WasmExpressionBuilder {
  def transformBody(tree: IRTrees.Tree, resultType: IRTypes.Type)(implicit
      ctx: FunctionTypeWriterWasmContext,
      fctx: WasmFunctionContext
  ): WasmExpr = {
    val builder = new WasmExpressionBuilder(ctx, fctx)
    WasmExpr(builder.genBody(tree, resultType))
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
        case IRTypes.BooleanType | IRTypes.ByteType | IRTypes.ShortType |
            IRTypes.IntType | IRTypes.FloatType | IRTypes.DoubleType =>
          Some(primType)
        case _ =>
          None
      }
    }
  }
}

private class WasmExpressionBuilder private (
    ctx: FunctionTypeWriterWasmContext,
    fctx: WasmFunctionContext
) {
  import WasmExpressionBuilder._

  private val instrs = mutable.ListBuffer.empty[WasmInstr]

  def genBody(tree: IRTrees.Tree, expectedType: IRTypes.Type): List[WasmInstr] = {
    genTree(tree, expectedType)
    instrs.toList
  }

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
      case t: IRTrees.IsInstanceOf     => genIsInstanceOf(t)
      case t: IRTrees.AsInstanceOf     => genAsInstanceOf(t)
      case t: IRTrees.Block            => genBlock(t, expectedType)
      case t: IRTrees.Labeled          => genLabeled(t, expectedType)
      case t: IRTrees.Return           => genReturn(t)
      case t: IRTrees.Select           => genSelect(t)
      case t: IRTrees.Assign           => genAssign(t)
      case t: IRTrees.VarDef           => genVarDef(t)
      case t: IRTrees.New              => genNew(t)
      case t: IRTrees.If               => genIf(t, expectedType)
      case t: IRTrees.While            => genWhile(t)
      case t: IRTrees.Skip             => IRTypes.NoType
      case t: IRTrees.IdentityHashCode => genIdentityHashCode(t)
      case _ =>
        println(tree)
        ???

      // case unary: IRTrees.JSUnaryOp => ???
      // case select: IRTrees.JSPrivateSelect => ???
      // case nul: IRTrees.Null => ???
      // case v: IRTrees.UnwrapFromThrowable => ???
      // case IRTrees.RecordValue(pos) =>
      // case IRTrees.JSTypeOfGlobalRef(pos) =>
      // case IRTrees.JSMethodApply(pos) =>
      // case IRTrees.Debugger(pos) =>
      // case IRTrees.JSNewTarget(pos) =>
      // case IRTrees.SelectStatic(tpe) =>
      // case IRTrees.IsInstanceOf(pos) =>
      // case IRTrees.JSLinkingInfo(pos) =>
      // case IRTrees.Select(tpe) =>
      // case IRTrees.Return(pos) =>
      // case IRTrees.ArrayLength(pos) =>
      // case IRTrees.While(pos) =>
      // case IRTrees.LoadJSConstructor(pos) =>
      // case IRTrees.JSSuperMethodCall(pos) =>
      // case IRTrees.NewArray(pos) =>
      // case IRTrees.Match(tpe) =>
      // case IRTrees.Throw(pos) =>
      // case IRTrees.JSNew(pos) =>
      // case IRTrees.Closure(pos) =>
      // case IRTrees.JSGlobalRef(pos) =>
      // case IRTrees.JSBinaryOp(pos) =>
      // case IRTrees.JSObjectConstr(pos) =>
      // case IRTrees.RecordSelect(tpe) =>
      // case IRTrees.AsInstanceOf(pos) =>
      // case IRTrees.TryFinally(pos) =>
      // case IRTrees.Labeled(pos) =>
      // case IRTrees.SelectJSNativeMember(pos) =>
      // case IRTrees.ClassOf(pos) =>
      // case IRTrees.GetClass(pos) =>
      // case IRTrees.JSImportMeta(pos) =>
      // case IRTrees.JSSuperSelect(pos) =>
      // case IRTrees.ArraySelect(tpe) =>
      // case IRTrees.JSSelect(pos) =>
      // case IRTrees.LoadJSModule(pos) =>
      // case IRTrees.JSFunctionApply(pos) =>
      // case IRTrees.WrapAsThrowable(pos) =>
      // case IRTrees.JSSuperConstructorCall(pos) =>
      // case IRTrees.Clone(pos) =>
      // case IRTrees.CreateJSClass(pos) =>
      // case IRTrees.Transient(pos) =>
      // case IRTrees.ArrayValue(pos) =>
      // case IRTrees.JSDelete(pos) =>
      // case IRTrees.ForIn(pos) =>
      // case IRTrees.JSArrayConstr(pos) =>
      // case tc: IRTrees.TryCatch => ???
      // case IRTrees.JSImportCall(pos) =>
      // case IRTrees.IdentityHashCode(pos) =>
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
        val className = WasmStructTypeName(sel.className)
        val fieldName = WasmFieldName(sel.field.name)
        val idx = ctx.getClassInfo(sel.className).getFieldIdx(fieldName)

        // For Select, the receiver can never be a hijacked class, so we can use genTreeAuto
        genTreeAuto(sel.qualifier)

        genTree(t.rhs, t.lhs.tpe)
        instrs += STRUCT_SET(TypeIdx(className), idx)

      case sel: IRTrees.SelectStatic => // OK?
        val className = WasmStructTypeName(sel.className)
        val fieldName = WasmFieldName(sel.field.name)
        val idx = ctx.getClassInfo(sel.className).getFieldIdx(fieldName)
        instrs += GLOBAL_GET(
          GlobalIdx(Names.WasmGlobalName.WasmModuleInstanceName.fromIR(sel.className))
        )
        genTree(t.rhs, t.lhs.tpe)
        instrs += STRUCT_SET(TypeIdx(className), idx)

      case assign: IRTrees.ArraySelect     => ??? // array.set
      case assign: IRTrees.RecordSelect    => ??? // struct.set
      case assign: IRTrees.JSPrivateSelect => ???
      case assign: IRTrees.JSSelect        => ???
      case assign: IRTrees.JSSuperSelect   => ???
      case assign: IRTrees.JSGlobalRef     => ???

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
          IRTrees.ApplyStatically(t.flags, t.receiver, receiverClassName, t.method, t.args)(t.tpe)(t.pos)
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
      case ClassType(className)   => className
      case IRTypes.AnyType        => IRNames.ObjectClass
      case _                      => throw new Error(s"Invalid receiver type ${t.receiver.tpe}")
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
      val funcName = Names.WasmFunctionName(hijackedClass, t.method.name)
      instrs += CALL(FuncIdx(funcName))
    }

    /* Generates a vtable- or itable-based dispatch.
     * Before this code gen, the stack must contain the receiver and the args, and
     * the receiver is also available in the local `receiverLocalForDispatch`.
     * The two occurrences of the receiver must have the type for dispatch.
     * After this code gen, the stack contains the result.
     */
    def genTableDispatch(): Unit = {
      if (receiverClassInfo.isInterface)
        genITableDispatch()
      else
        genVTableDispatch()
    }

    // Generates an itable-based dispatch.
    def genITableDispatch(): Unit = {
      val itables = ctx.calculateClassItables(clazz = receiverClassName)

      val (itableIdx, methodIdx) = itables.resolveMethod(t.method.name)
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
      val (methodIdx, info) = ctx
        .calculateVtableType(receiverClassName)
        .resolveWithIdx(WasmFunctionName(receiverClassName, t.method.name))

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
        StructFieldIdx(methodIdx)
      )
      instrs += CALL_REF(
        TypeIdx(info.toWasmFunctionType()(ctx).name)
      )
    }

    if (!receiverClassInfo.isAncestorOfHijackedClass) {
      // Standard dispatch codegen
      genReceiverNotNull()
      instrs += LOCAL_TEE(LocalIdx(receiverLocalForDispatch))
      genArgs(t.args, t.method.name)
      genTableDispatch()
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

      val labelDone = fctx.genLabel()
      instrs += BLOCK(BlockType.ValueType(resultTyp.headOption), Some(labelDone))

      // First try the case where the value is one of our objects
      val labelNotOurObject = fctx.genLabel()
      instrs += BLOCK(BlockType.ValueType(Types.WasmRefType.any), Some(labelNotOurObject))

      // Load receiver and arguments and store them in temporary variables
      genReceiverNotNull()
      val argsLocals = if (t.args.isEmpty) {
        /* When there are no arguments, we can leave the receiver directly on
         * the stack instead of going through a local. We will still need a
         * local for the table-based dispatch, though.
         */
        Nil
      } else {
        val receiverLocal: WasmLocalName = fctx.genSyntheticLocalName()
        fctx.locals.define(WasmLocal(receiverLocal, Types.WasmRefType.any, isParameter = false))

        instrs += LOCAL_SET(LocalIdx(receiverLocal))
        val argsLocals: List[WasmLocalName] = for ((arg, typeRef) <- t.args.zip(t.method.name.paramTypeRefs)) yield {
          val typ = ctx.inferTypeFromTypeRef(typeRef)
          genTree(arg, typ)
          val localName = fctx.genSyntheticLocalName()
          fctx.locals.define(WasmLocal(localName, TypeTransformer.transformType(typ)(ctx), isParameter = false))
          instrs += LOCAL_SET(LocalIdx(localName))
          localName
        }
        instrs += LOCAL_GET(LocalIdx(receiverLocal))
        argsLocals
      }

      def pushArgs(): Unit =
        argsLocals.foreach(argLocal => instrs += LOCAL_GET(LocalIdx(argLocal)))

      instrs += BR_ON_CAST_FAIL(
        CastFlags(false, false),
        labelNotOurObject,
        WasmImmediate.HeapType(Types.WasmHeapType.Simple.Any),
        WasmImmediate.HeapType(heapTypeForDispatch)
      )
      instrs += LOCAL_TEE(LocalIdx(receiverLocalForDispatch))
      pushArgs()
      genTableDispatch()
      instrs += BR(labelDone)
      instrs += END // BLOCK labelNotOurObject

      // Now we have a value that is not one of our objects; the (ref any) is still on the stack

      if (t.method.name == toStringMethodName) {
        // By spec, toString() is special
        assert(argsLocals.isEmpty)
        instrs += CALL(FuncIdx(WasmFunctionName.jsValueToString))
      } else if (receiverClassName == JLNumberClass) {
        // the value must be a `number`, hence we can unbox to `double`
        genUnbox(IRTypes.DoubleType)
        pushArgs()
        genHijackedClassCall(IRNames.BoxedDoubleClass)
      } else if (receiverClassName == CharSequenceClass) {
        // the value must be a `string`; it already has the right type
        pushArgs()
        genHijackedClassCall(IRNames.BoxedStringClass)
      } else {
        /* It must be a method of j.l.Object and it can be any value.
         * hashCode() and equals() are overridden in all hijacked classes; we
         * use dedicated JavaScript helpers for those.
         * The other methods are never overridden and can be statically
         * resolved to j.l.Object.
         */
        pushArgs()
        t.method.name match {
          case `hashCodeMethodName` =>
            instrs += CALL(FuncIdx(WasmFunctionName.jsValueHashCode))
          case `equalsMethodName` =>
            instrs += CALL(FuncIdx(WasmFunctionName.is))
          case _ =>
            genHijackedClassCall(IRNames.ObjectClass)
        }
      }

      instrs += END // BLOCK labelDone
    }

    if (t.tpe == IRTypes.NothingType)
      instrs += UNREACHABLE

    t.tpe
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
        val funcName = Names.WasmFunctionName(t.className, t.method.name)
        instrs += CALL(FuncIdx(funcName))
        if (t.tpe == IRTypes.NothingType)
          instrs += UNREACHABLE
        t.tpe
    }
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
        // TODO We should allocate literal strings once and for all as globals
        // This is absolutely atrocious at the moment
        val str = v.value
        instrs += CALL(FuncIdx(WasmFunctionName.emptyString))
        for (c <- str) {
          instrs += WasmInstr.I32_CONST(I32(c.toInt))
          instrs += CALL(FuncIdx(WasmFunctionName.charToString))
          instrs += CALL(FuncIdx(WasmFunctionName.stringConcat))
        }

      case v: IRTrees.ClassOf => ???
    }

    l.tpe
  }

  private def genSelect(sel: IRTrees.Select): IRTypes.Type = {
    val className = WasmStructTypeName(sel.className)
    val fieldName = WasmFieldName(sel.field.name)
    val idx = ctx.getClassInfo(sel.className).getFieldIdx(fieldName)

    // For Select, the receiver can never be a hijacked class, so we can use genTreeAuto
    genTreeAuto(sel.qualifier)

    instrs += STRUCT_GET(TypeIdx(className), idx)
    sel.tpe
  }

  private def genStoreModule(t: IRTrees.StoreModule): IRTypes.Type = {
    val name = WasmGlobalName.WasmModuleInstanceName.fromIR(t.className)
    genTree(t.value, IRTypes.ClassType(t.className))
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
        instrs += I64_EXTEND32_S
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
      genTreeAuto(tree)
      tree.tpe match {
        case IRTypes.StringType =>
          () // no-op

        case IRTypes.BooleanType =>
          instrs += CALL(FuncIdx(WasmFunctionName.booleanToString))

        case IRTypes.CharType =>
          instrs += CALL(FuncIdx(WasmFunctionName.charToString))

        case IRTypes.ByteType | IRTypes.ShortType | IRTypes.IntType =>
          instrs += CALL(FuncIdx(WasmFunctionName.intToString))

        case IRTypes.LongType =>
          // TODO Write a correct implementation
          instrs += DROP
          genLiteral(IRTrees.StringLiteral("0")(tree.pos))

        case IRTypes.FloatType =>
          instrs += F64_PROMOTE_F32
          instrs += CALL(FuncIdx(WasmFunctionName.doubleToString))

        case IRTypes.DoubleType =>
          instrs += CALL(FuncIdx(WasmFunctionName.doubleToString))

        case IRTypes.UndefType =>
          instrs += CALL(FuncIdx(WasmFunctionName.jsValueToString))

        case IRTypes.ClassType(className) =>
          val info = ctx.getClassInfo(className)
          if (info.kind == ClassKind.HijackedClass) {
            instrs += CALL(FuncIdx(WasmFunctionName.jsValueToString))
          } else {
            println(tree)
            ???
          }

        case _ =>
          // TODO
          println(tree)
          ???
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
                instrs += CALL(WasmImmediate.FuncIdx(WasmFunctionName.unboxOrNull(primType.primRef)))
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
   *  `targetTpe` must not be `NothingType`, `NullType` nor `NoType`.
   *
   *  The type left on the stack is non-nullable.
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
    val local = WasmLocal(
      WasmLocalName.fromIR(r.name.name),
      TypeTransformer.transformType(r.vtpe)(ctx),
      isParameter = false
    )
    fctx.locals.define(local)

    genTree(r.rhs, r.vtpe)
    instrs += LOCAL_SET(LocalIdx(local.name))

    IRTypes.NoType
  }

  private def genIf(t: IRTrees.If, expectedType: IRTypes.Type): IRTypes.Type = {
    val ty = TypeTransformer.transformType(expectedType)(ctx)
    genTree(t.cond, IRTypes.BooleanType)
    instrs += IF(BlockType.ValueType(ty))
    genTree(t.thenp, expectedType)
    instrs += ELSE
    genTree(t.elsep, expectedType)
    instrs += END
    expectedType
  }

  private def genWhile(t: IRTrees.While): IRTypes.Type = {
    val label = fctx.genLabel()
    val noResultType = BlockType.ValueType(Types.WasmNoType)

    t.cond match {
      case IRTrees.BooleanLiteral(true) =>
        // infinite loop that must be typed as `nothing`, i.e., unreachable
        // loop $label
        //   body
        //   br $label
        // end
        // unreachable
        instrs += LOOP(noResultType, Some(label))
        genTree(t.body, IRTypes.NoType)
        instrs += BR(label)
        instrs += END
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

        instrs += LOOP(noResultType, Some(label))
        genTree(t.cond, IRTypes.BooleanType)
        instrs += IF(noResultType)
        genTree(t.body, IRTypes.NoType)
        instrs += BR(label)
        instrs += END // IF
        instrs += END // LOOP
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
    instrs += CALL(FuncIdx(WasmFunctionName(n.className, n.ctor.name)))
    instrs += LOCAL_GET(LocalIdx(localInstance.name))
    n.tpe
  }

  /** Codegen to box a primitive `char`/`long` into a `CharacterBox`/`LongBox`. */
  private def genBox(primType: IRTypes.PrimTypeWithRef, boxClassName: IRNames.ClassName): IRTypes.Type = {
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
}
