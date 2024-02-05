package ir2wasm

import org.scalajs.ir.{Trees => IRTrees}
import wasm4s.Names
import wasm4s.WasmExpr
import wasm4s.WasmInstr
import wasm4s.WasmInstr._
import wasm4s.WasmImmediate._
import org.scalajs.ir.{Types => IRTypes}
import wasm4s.WasmFunctionContext
import wasm4s.WasmFunction

class WasmExpressionBuilder(fctx: WasmFunctionContext) {
  def transformMethod(
      method: IRTrees.MethodDef
  ): List[WasmInstr] = {
    val body = method.body.getOrElse(throw new Exception("abstract method cannot be transformed"))
    val instrs = transformTree(body)
    method.resultType match {
      case IRTypes.NoType => instrs
      case _              => instrs :+ RETURN
    }
  }
  def transformTree(tree: IRTrees.Tree): List[WasmInstr] = {
    tree match {
      case l: IRTrees.Literal  => List(transformLiteral(l))
      case u: IRTrees.UnaryOp  => transformUnaryOp(u)
      case b: IRTrees.BinaryOp => transformBinaryOp(b)
      case r: IRTrees.VarRef   => List(transformVarRef(r))
      case _                   => ???

      // case undef: IRTrees.Undefined => ???
      // case unary: IRTrees.JSUnaryOp => ???
      // case select: IRTrees.JSPrivateSelect => ???
      // case nul: IRTrees.Null => ???
      // case v: IRTrees.UnwrapFromThrowable => ???
      // case v: IRTrees.New => ???
      // case IRTrees.Assign(pos) =>
      // case IRTrees.ApplyStatic(tpe) =>
      // case IRTrees.RecordValue(pos) =>
      // case IRTrees.JSTypeOfGlobalRef(pos) =>
      // case IRTrees.JSMethodApply(pos) =>
      // case IRTrees.Debugger(pos) =>
      // case IRTrees.JSNewTarget(pos) =>
      // case IRTrees.SelectStatic(tpe) =>
      // case IRTrees.LoadModule(pos) =>
      // case IRTrees.IsInstanceOf(pos) =>
      // case IRTrees.JSLinkingInfo(pos) =>
      // case IRTrees.Select(tpe) =>
      // case IRTrees.Return(pos) =>
      // case IRTrees.ArrayLength(pos) =>
      // case IRTrees.ApplyStatically(tpe) =>
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
      // case IRTrees.If(tpe) =>
      // case IRTrees.TryFinally(pos) =>
      // case IRTrees.Block =>
      // case IRTrees.Labeled(pos) =>
      // case IRTrees.SelectJSNativeMember(pos) =>
      // case IRTrees.ClassOf(pos) =>
      // case IRTrees.StoreModule(pos) =>
      // case IRTrees.GetClass(pos) =>
      // case IRTrees.This(tpe) =>
      // case IRTrees.JSImportMeta(pos) =>
      // case IRTrees.JSSuperSelect(pos) =>
      // case IRTrees.ArraySelect(tpe) =>
      // case IRTrees.JSSelect(pos) =>
      // case IRTrees.Skip(pos) =>
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
      // case IRTrees.Apply(tpe) =>
      // case IRTrees.ApplyDynamicImport(pos) =>
      // case vd: IRTrees.VarDef => ???
      // case tc: IRTrees.TryCatch => ???
      // case IRTrees.JSImportCall(pos) =>
      // case IRTrees.IdentityHashCode(pos) =>
    }

  }

  def transformLiteral(l: IRTrees.Literal): WasmInstr = l match {
    case IRTrees.BooleanLiteral(v) => WasmInstr.I32_CONST(if (v) I32(1) else I32(0))
    case IRTrees.ByteLiteral(v)    => WasmInstr.I32_CONST(I32(v))
    case IRTrees.ShortLiteral(v)   => WasmInstr.I32_CONST(I32(v))
    case IRTrees.IntLiteral(v)     => WasmInstr.I32_CONST(I32(v))
    case IRTrees.CharLiteral(v)    => WasmInstr.I32_CONST(I32(v))
    case IRTrees.LongLiteral(v)    => WasmInstr.I64_CONST(I64(v))
    case IRTrees.FloatLiteral(v)   => WasmInstr.F32_CONST(F32(v))
    case IRTrees.DoubleLiteral(v)  => WasmInstr.F64_CONST(F64(v))

    case v: IRTrees.Undefined     => ???
    case v: IRTrees.Null          => ???
    case v: IRTrees.StringLiteral => ???
    case v: IRTrees.ClassOf       => ???
  }

  def transformUnaryOp(unary: IRTrees.UnaryOp): List[WasmInstr] = {
    ???
  }

  def transformBinaryOp(binary: IRTrees.BinaryOp): List[WasmInstr] = {
    import IRTrees.BinaryOp
    val lhsInstrs = transformTree(binary.lhs)
    val rhsInstrs = transformTree(binary.rhs)
    val operation = binary.op match {
      case BinaryOp.===      => ???
      case BinaryOp.!==      => ???
      case BinaryOp.String_+ => ???

      case BinaryOp.Boolean_== => I32_EQ
      case BinaryOp.Boolean_!= => I32_NE
      case BinaryOp.Boolean_|  => I32_OR
      case BinaryOp.Boolean_&  => I32_AND

      case BinaryOp.Int_+   => I32_ADD
      case BinaryOp.Int_-   => I32_SUB
      case BinaryOp.Int_*   => I32_MUL
      case BinaryOp.Int_/   => I32_DIV_S // signed division
      case BinaryOp.Int_%   => I32_REM_S // signed remainder
      case BinaryOp.Int_|   => ???
      case BinaryOp.Int_&   => ???
      case BinaryOp.Int_^   => ???
      case BinaryOp.Int_<<  => ???
      case BinaryOp.Int_>>> => ???
      case BinaryOp.Int_>>  => ???
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
      case BinaryOp.Long_|   => ???
      case BinaryOp.Long_&   => ???
      case BinaryOp.Long_^   => ???
      case BinaryOp.Long_<<  => ???
      case BinaryOp.Long_>>> => ???
      case BinaryOp.Long_>>  => ???

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

      // // New in 1.11
      case BinaryOp.String_charAt => ??? // TODO
    }
    lhsInstrs ++ rhsInstrs :+ operation
  }

  def transformVarRef(r: IRTrees.VarRef): LOCAL_GET = {
    val name = Names.WasmLocalName.fromIR(r.ident.name)
    LOCAL_GET(LocalIdx(name))
  }

}
