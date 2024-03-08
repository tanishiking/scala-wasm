package wasm
package ir2wasm

import scala.annotation.switch

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

class WasmExpressionBuilder(ctx: FunctionTypeWriterWasmContext, fctx: WasmFunctionContext) {

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

  def transformTree(tree: IRTrees.Tree): List[WasmInstr] = {
    tree match {
      case t: IRTrees.Literal    => transformLiteral(t)
      case t: IRTrees.UnaryOp    => transformUnaryOp(t)
      case t: IRTrees.BinaryOp   => transformBinaryOp(t)
      case t: IRTrees.VarRef     => List(transformVarRef(t))
      case t: IRTrees.LoadModule => transformLoadModule(t)
      case t: IRTrees.StoreModule =>
        transformStoreModule(t)
      case t: IRTrees.This => // push receiver to the stack
        List(LOCAL_GET(LocalIdx(fctx.receiver.name)))
      case t: IRTrees.ApplyStatic => ???
      case t: IRTrees.ApplyStatically =>
        transformApplyStatically(t)
      case t: IRTrees.Apply              => transformApply(t)
      case t: IRTrees.ApplyDynamicImport => ???
      case t: IRTrees.AsInstanceOf       => transformAsInstanceOf(t)
      case t: IRTrees.Block              => transformBlock(t)
      case t: IRTrees.Labeled            => transformLabeled(t)
      case t: IRTrees.Return             => transformReturn(t)
      case t: IRTrees.Select             => transformSelect(t)
      case t: IRTrees.Assign             => transformAssign(t)
      case t: IRTrees.VarDef             => transformVarDef(t)
      case t: IRTrees.New                => transformNew(t)
      case t: IRTrees.If                 => transformIf(t)
      case t: IRTrees.While              => transformWhile(t)
      case t: IRTrees.Skip               => Nil
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

  }

  private def transformAssign(t: IRTrees.Assign): List[WasmInstr] = {
    val wasmRHS = transformTree(t.rhs)
    t.lhs match {
      case sel: IRTrees.Select =>
        val className = WasmStructTypeName(sel.className)
        val fieldName = WasmFieldName(sel.field.name)
        val idx = ctx.getClassInfo(sel.className).getFieldIdx(fieldName)
        val castIfRequired = sel.qualifier match {
          case _: IRTrees.This =>
            List(
              // requires cast if the qualifier is `this`
              // because receiver type is Object in wasm
              REF_CAST(
                HeapType(Types.WasmHeapType.Type(WasmStructTypeName(sel.className)))
              )
            )
          case _ => Nil
        }
        transformTree(sel.qualifier) ++ castIfRequired ++ wasmRHS :+
          STRUCT_SET(TypeIdx(className), idx)

      case sel: IRTrees.SelectStatic => // OK?
        val className = WasmStructTypeName(sel.className)
        val fieldName = WasmFieldName(sel.field.name)
        val idx = ctx.getClassInfo(sel.className).getFieldIdx(fieldName)
        List(
          GLOBAL_GET(
            GlobalIdx(Names.WasmGlobalName.WasmModuleInstanceName.fromIR(sel.className))
          )
        ) ++ wasmRHS ++ List(
          STRUCT_SET(TypeIdx(className), idx)
        )
      case assign: IRTrees.ArraySelect     => ??? // array.set
      case assign: IRTrees.RecordSelect    => ??? // struct.set
      case assign: IRTrees.JSPrivateSelect => ???
      case assign: IRTrees.JSSelect        => ???
      case assign: IRTrees.JSSuperSelect   => ???
      case assign: IRTrees.JSGlobalRef     => ???
      case ref: IRTrees.VarRef =>
        wasmRHS :+ LOCAL_SET(LocalIdx(Names.WasmLocalName.fromIR(ref.ident.name)))
    }
  }

  private def transformApply(t: IRTrees.Apply): List[WasmInstr] = {
    val pushReceiver = transformTree(t.receiver)
    val wasmArgs = t.args.flatMap(transformTree)

    val receiverClassName = t.receiver.tpe match {
      case ClassType(className)   => className
      case prim: IRTypes.PrimType => IRTypes.PrimTypeToBoxedClass(prim)
      case _                      => throw new Error(s"Invalid receiver type ${t.receiver.tpe}")
    }
    val receiverClassInfo = ctx.getClassInfo(receiverClassName)

    if (receiverClassInfo.isInterface) { // interface dispatch
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

      pushReceiver ++ wasmArgs ++ pushReceiver ++
        List(
          STRUCT_GET(
            // receiver type should be upcasted into `Object` if it's interface
            // by TypeTransformer#transformType
            TypeIdx(WasmStructTypeName(IRNames.ObjectClass)),
            StructFieldIdx(1)
          ),
          I32_CONST(I32(itableIdx)),
          ARRAY_GET(
            TypeIdx(WasmArrayType.itables.name)
          ),
          REF_CAST(
            HeapType(Types.WasmHeapType.Type(WasmITableTypeName(targetClass.name)))
          ),
          STRUCT_GET(
            TypeIdx(WasmITableTypeName(targetClass.name)),
            StructFieldIdx(methodIdx)
          ),
          CALL_REF(
            TypeIdx(method.toWasmFunctionType()(ctx).name)
          )
        )
    } else if (receiverClassInfo.kind == ClassKind.HijackedClass) {
      // statically resolved call
      val info = receiverClassInfo.getMethodInfo(t.method.name)
      val castIfNeeded =
        if (receiverClassName == IRNames.BoxedStringClass && t.receiver.tpe == ClassType(IRNames.BoxedStringClass))
          List(REF_CAST(HeapType(Types.WasmHeapType.Type(WasmStructTypeName.string))))
        else
          Nil
      pushReceiver ++ castIfNeeded ++ wasmArgs ++
        List(
          CALL(FuncIdx(info.name))
        )
    } else { // virtual dispatch
      val (methodIdx, info) = ctx
        .calculateVtable(receiverClassName)
        .resolveWithIdx(WasmFunctionName(receiverClassName, t.method.name))

      // // push args to the stacks
      // local.get $this ;; for accessing funcref
      // local.get $this ;; for accessing vtable
      // struct.get $classType 0 ;; get vtable
      // struct.get $vtableType $methodIdx ;; get funcref
      // call.ref (type $funcType) ;; call funcref
      pushReceiver ++ wasmArgs ++ pushReceiver ++
        List(
          REF_CAST(
            HeapType(Types.WasmHeapType.Type(WasmStructTypeName(receiverClassName)))
          ),
          STRUCT_GET(
            TypeIdx(WasmStructTypeName(receiverClassName)),
            StructFieldIdx(0)
          ),
          STRUCT_GET(
            TypeIdx(WasmVTableTypeName.fromIR(receiverClassName)),
            StructFieldIdx(methodIdx)
          ),
          CALL_REF(
            TypeIdx(info.toWasmFunctionType()(ctx).name)
          )
        )
    }
  }

  private def transformApplyStatically(t: IRTrees.ApplyStatically): List[WasmInstr] = {
    val wasmArgs = transformTree(t.receiver) ++ t.args.flatMap(transformTree)
    val funcName = Names.WasmFunctionName(t.className, t.method.name)
    wasmArgs :+ CALL(FuncIdx(funcName))
  }

  private def transformLiteral(l: IRTrees.Literal): List[WasmInstr] = l match {
    case IRTrees.BooleanLiteral(v) => WasmInstr.I32_CONST(if (v) I32(1) else I32(0)) :: Nil
    case IRTrees.ByteLiteral(v)    => WasmInstr.I32_CONST(I32(v)) :: Nil
    case IRTrees.ShortLiteral(v)   => WasmInstr.I32_CONST(I32(v)) :: Nil
    case IRTrees.IntLiteral(v)     => WasmInstr.I32_CONST(I32(v)) :: Nil
    case IRTrees.CharLiteral(v)    => WasmInstr.I32_CONST(I32(v)) :: Nil
    case IRTrees.LongLiteral(v)    => WasmInstr.I64_CONST(I64(v)) :: Nil
    case IRTrees.FloatLiteral(v)   => WasmInstr.F32_CONST(F32(v)) :: Nil
    case IRTrees.DoubleLiteral(v)  => WasmInstr.F64_CONST(F64(v)) :: Nil

    case v: IRTrees.Undefined =>
      WasmInstr.GLOBAL_GET(GlobalIdx(WasmGlobalName.WasmUndefName)) :: Nil
    case v: IRTrees.Null => ???

    case v: IRTrees.StringLiteral =>
      // TODO We should allocate literal strings once and for all as globals
      val str = v.value
      str.toList.map(c => WasmInstr.I32_CONST(I32(c.toInt))) :::
        List(
          WasmInstr
            .ARRAY_NEW_FIXED(TypeIdx(WasmTypeName.WasmArrayTypeName.stringData), I32(str.length())),
          WasmInstr.STRUCT_NEW(TypeIdx(WasmTypeName.WasmStructTypeName.string))
        )

    case v: IRTrees.ClassOf => ???
  }

  private def transformSelect(sel: IRTrees.Select): List[WasmInstr] = {
    val className = WasmStructTypeName(sel.className)
    val fieldName = WasmFieldName(sel.field.name)
    val idx = ctx.getClassInfo(sel.className).getFieldIdx(fieldName)

    val select = sel.qualifier match {
      case _: IRTrees.This =>
        List(
          // requires cast if the qualifier is `this`
          // because receiver type is Object in wasm
          REF_CAST(
            HeapType(Types.WasmHeapType.Type(WasmStructTypeName(sel.className)))
          ),
          STRUCT_GET(TypeIdx(className), idx)
        )
      case _ =>
        List(STRUCT_GET(TypeIdx(className), idx))
    }
    transformTree(sel.qualifier) ++ select
  }

  private def transformStoreModule(t: IRTrees.StoreModule): List[WasmInstr] = {
    val name = WasmGlobalName.WasmModuleInstanceName.fromIR(t.className)
    transformTree(t.value) :+ GLOBAL_SET(GlobalIdx(name))
  }

  /** Push module class instance to the stack.
    *
    * see: WasmBuilder.genLoadModuleFunc
    */
  private def transformLoadModule(t: IRTrees.LoadModule): List[WasmInstr] =
    List(CALL(FuncIdx(Names.WasmFunctionName.loadModule(t.className))))

  private def transformUnaryOp(unary: IRTrees.UnaryOp): List[WasmInstr] = {
    import IRTrees.UnaryOp._

    val lhsInstrs = transformTree(unary.lhs)

    (unary.op: @switch) match {
      case Boolean_! =>
        lhsInstrs ++
          List(
            I32_CONST(I32(1)),
            I32_XOR
          )

      // Widening conversions
      case CharToInt | ByteToInt | ShortToInt =>
        lhsInstrs // these are no-ops because they are all represented as i32's with the right mathematical value
      case IntToLong =>
        lhsInstrs :+ I64_EXTEND32_S
      case IntToDouble =>
        lhsInstrs :+ F64_CONVERT_I32_S
      case FloatToDouble =>
        lhsInstrs :+ F64_PROMOTE_F32

      // Narrowing conversions
      case IntToChar =>
        lhsInstrs ++ List(I32_CONST(I32(0xffff)), I32_AND)
      case IntToByte =>
        lhsInstrs :+ I32_EXTEND8_S
      case IntToShort =>
        lhsInstrs :+ I32_EXTEND16_S
      case LongToInt =>
        lhsInstrs :+ I32_WRAP_I64
      case DoubleToInt =>
        lhsInstrs :+ I32_TRUNC_SAT_F64_S
      case DoubleToFloat =>
        lhsInstrs :+ F32_DEMOTE_F64

      // Long <-> Double (neither widening nor narrowing)
      case LongToDouble =>
        lhsInstrs :+ F64_CONVERT_I64_S
      case DoubleToLong =>
        lhsInstrs :+ I64_TRUNC_SAT_F64_S

      // Long -> Float (neither widening nor narrowing), introduced in 1.6
      case LongToFloat =>
        lhsInstrs :+ F32_CONVERT_I64_S

      // String.length, introduced in 1.11
      case String_length =>
        lhsInstrs ++
          List(
            STRUCT_GET(TypeIdx(WasmStructTypeName.string), StructFieldIdx(0)), // get the array
            ARRAY_LEN
          )
    }
  }

  private def transformBinaryOp(binary: IRTrees.BinaryOp): List[WasmInstr] = {
    import IRTrees.BinaryOp

    def longShiftOp(shiftInstr: WasmInstr): List[WasmInstr] = {
      transformTree(binary.lhs) ++
        transformTree(binary.rhs) ++
        List(
          I64_EXTEND_I32_S,
          shiftInstr
        )
    }

    binary.op match {
      case BinaryOp.String_+ => transformStringConcat(binary.lhs, binary.rhs)

      case BinaryOp.Long_<<  => longShiftOp(I64_SHL)
      case BinaryOp.Long_>>> => longShiftOp(I64_SHR_U)
      case BinaryOp.Long_>>  => longShiftOp(I64_SHR_S)

      // New in 1.11
      case BinaryOp.String_charAt =>
        transformTree(binary.lhs) ++ // push the string
          List(
            STRUCT_GET(TypeIdx(WasmStructTypeName.string), StructFieldIdx(0)), // get the array
          ) ++
          transformTree(binary.rhs) ++ // push the index
          List(
            ARRAY_GET_U(TypeIdx(WasmArrayTypeName.stringData)) // access the element of the array
          )

      case _ => transformElementaryBinaryOp(binary)
    }
  }

  private def transformElementaryBinaryOp(binary: IRTrees.BinaryOp): List[WasmInstr] = {
    import IRTrees.BinaryOp
    val lhsInstrs = transformTree(binary.lhs)
    val rhsInstrs = transformTree(binary.rhs)
    val operation = binary.op match {
      case BinaryOp.=== => ???
      case BinaryOp.!== => ???

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

      case BinaryOp.Long_+   => I64_ADD
      case BinaryOp.Long_-   => I64_SUB
      case BinaryOp.Long_*   => I64_MUL
      case BinaryOp.Long_/   => I64_DIV_S
      case BinaryOp.Long_%   => I64_REM_S
      case BinaryOp.Long_|   => I64_OR
      case BinaryOp.Long_&   => I64_AND
      case BinaryOp.Long_^   => I64_XOR

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
    lhsInstrs ++ rhsInstrs :+ operation
  }

  private def transformStringConcat(lhs: IRTrees.Tree, rhs: IRTrees.Tree): List[WasmInstr] = {
    val wasmStringType = Types.WasmRefType(Types.WasmHeapType.Type(WasmStructTypeName.string))

    def transformToString(tree: IRTrees.Tree): List[WasmInstr] = {
      val valueInstrs = transformTree(tree)
      tree.tpe match {
        case IRTypes.StringType =>
          valueInstrs

        case IRTypes.BooleanType =>
          valueInstrs ++
            List(IF(BlockType.ValueType(wasmStringType))) ++
            transformLiteral(IRTrees.StringLiteral("true")(tree.pos)) ++
            List(ELSE) ++
            transformLiteral(IRTrees.StringLiteral("false")(tree.pos)) ++
            List(END)

        case IRTypes.CharType =>
          valueInstrs ++
            List(
              WasmInstr.ARRAY_NEW_FIXED(TypeIdx(WasmTypeName.WasmArrayTypeName.stringData), I32(1)),
              WasmInstr.STRUCT_NEW(TypeIdx(WasmTypeName.WasmStructTypeName.string))
            )

        case IRTypes.ByteType | IRTypes.ShortType | IRTypes.IntType =>
          // TODO Write a correct implementation
          valueInstrs ++ (DROP +: transformLiteral(IRTrees.StringLiteral("0")(tree.pos)))

        case IRTypes.LongType =>
          // TODO Write a correct implementation
          valueInstrs ++ (DROP +: transformLiteral(IRTrees.StringLiteral("0")(tree.pos)))

        case IRTypes.FloatType | IRTypes.DoubleType =>
          // TODO Write a correct implementation
          valueInstrs ++ (DROP +: transformLiteral(IRTrees.StringLiteral("0.0")(tree.pos)))

        case _ =>
          // TODO
          ???
      }
    }

    lhs match {
      case IRTrees.StringLiteral("") =>
        // Common case where we don't actually need a concatenation
        transformToString(rhs)

      case _ =>
        // TODO: transformToString(lhs) ::: transformToString(rhs) :: callHelperConcat() :: Nil
        ???
    }
  }

  private def transformAsInstanceOf(tree: IRTrees.AsInstanceOf): List[WasmInstr] = {
    val exprInstrs = transformTree(tree.expr)

    val sourceTpe = tree.expr.tpe
    val targetTpe = tree.tpe

    if (IRTypes.isSubtype(sourceTpe, targetTpe)(isSubclass(_, _))) {
      // Common case where no cast is necessary
      exprInstrs
    } else {
      println(tree)
      ???
    }
  }

  private def isSubclass(subClass: IRNames.ClassName, superClass: IRNames.ClassName): Boolean =
    ctx.getClassInfo(subClass).ancestors.contains(superClass)

  private def transformVarRef(r: IRTrees.VarRef): LOCAL_GET = {
    val name = WasmLocalName.fromIR(r.ident.name)
    LOCAL_GET(LocalIdx(name))
  }

  private def transformVarDef(r: IRTrees.VarDef): List[WasmInstr] = {
    r.vtpe match {
      // val _: Unit = rhs
      case ClassType(className) if className == IRNames.BoxedUnitClass =>
        transformTree(r.rhs) :+ DROP
      case _ =>
        val local = WasmLocal(
          WasmLocalName.fromIR(r.name.name),
          TypeTransformer.transformType(r.vtpe)(ctx),
          isParameter = false
        )
        fctx.locals.define(local)

        transformTree(r.rhs) :+ LOCAL_SET(LocalIdx(local.name))
    }
  }

  private def transformIf(t: IRTrees.If): List[WasmInstr] = {
    val ty = TypeTransformer.transformType(t.tpe)(ctx)
    transformTree(t.cond) ++
      List(IF(BlockType.ValueType(ty))) ++
      transformTree(t.thenp) ++
      List(ELSE) ++
      transformTree(t.elsep) ++
      List(END)
  }

  private def transformWhile(t: IRTrees.While): List[WasmInstr] = {
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
        List(
          LOOP(noResultType, Some(label))
        ) ++ transformTree(t.body) ++
          List(
            BR(label),
            END,
            UNREACHABLE
          )

      case _ =>
        // loop $label
        //   cond
        //   if
        //     body
        //     br $label
        //   end
        // end

        List(
          LOOP(noResultType, Some(label))
        ) ++ transformTree(t.cond) ++
          List(
            IF(noResultType)
          ) ++
          transformTree(t.body) ++
          List(
            BR(label),
            END, // IF
            END // LOOP
          )
    }
  }

  private def transformBlock(t: IRTrees.Block): List[WasmInstr] =
    t.stats.flatMap(transformTree)

  private def transformLabeled(t: IRTrees.Labeled): List[WasmInstr] = {
    val label = fctx.getLabelFor(t.label.name)
    val ty = TypeTransformer.transformType(t.tpe)(ctx)
    BLOCK(BlockType.ValueType(ty), Some(label)) +:
      transformTree(t.body) :+
      END
  }

  private def transformReturn(t: IRTrees.Return): List[WasmInstr] = {
    val label = fctx.getLabelFor(t.label.name)
    transformTree(t.expr) :+
      BR(label)
  }

  private def transformNew(n: IRTrees.New): List[WasmInstr] = {
    val localInstance = WasmLocal(
      fctx.genSyntheticLocalName(),
      TypeTransformer.transformType(n.tpe)(ctx),
      isParameter = false
    )
    fctx.locals.define(localInstance)

    List(
      // REF_NULL(HeapType(Types.WasmHeapType.Type(WasmTypeName.WasmStructTypeName(n.className)))),
      // LOCAL_TEE(LocalIdx(localInstance.name))
      CALL(FuncIdx(WasmFunctionName.newDefault(n.className))),
      LOCAL_TEE(LocalIdx(localInstance.name))
    ) ++ n.args.flatMap(transformTree) ++
      List(
        CALL(FuncIdx(WasmFunctionName(n.className, n.ctor.name))),
        LOCAL_GET(LocalIdx(localInstance.name))
      )
  }
}
