package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.JSNativeLoadSpec
import org.scalajs.ir.Types._

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.WasmInstr._

import VarGen._

/** Scala.js-specific Wasm generators that are used across the board. */
object SWasmGen {

  def genZeroOf(tpe: Type)(implicit ctx: WasmContext): WasmInstr = {
    tpe match {
      case BooleanType | CharType | ByteType | ShortType | IntType =>
        I32_CONST(0)

      case LongType   => I64_CONST(0L)
      case FloatType  => F32_CONST(0.0f)
      case DoubleType => F64_CONST(0.0)
      case StringType => GLOBAL_GET(genGlobalName.emptyString)
      case UndefType  => GLOBAL_GET(genGlobalName.undef)

      case AnyType | ClassType(_) | ArrayType(_) | NullType =>
        REF_NULL(Types.WasmHeapType.None)

      case NoType | NothingType | _: RecordType =>
        throw new AssertionError(s"Unexpected type for field: ${tpe.show()}")
    }
  }

  def genBoxedZeroOf(tpe: Type)(implicit ctx: WasmContext): WasmInstr = {
    tpe match {
      case BooleanType =>
        GLOBAL_GET(genGlobalName.bFalse)
      case CharType =>
        GLOBAL_GET(genGlobalName.bZeroChar)
      case ByteType | ShortType | IntType | FloatType | DoubleType =>
        GLOBAL_GET(genGlobalName.bZero)
      case LongType =>
        GLOBAL_GET(genGlobalName.bZeroLong)
      case AnyType | ClassType(_) | ArrayType(_) | StringType | UndefType | NullType =>
        REF_NULL(Types.WasmHeapType.None)

      case NoType | NothingType | _: RecordType =>
        throw new AssertionError(s"Unexpected type for field: ${tpe.show()}")
    }
  }

  def genLoadJSConstructor(fb: FunctionBuilder, className: ClassName)(implicit
      ctx: WasmContext
  ): Unit = {
    val info = ctx.getClassInfo(className)

    info.jsNativeLoadSpec match {
      case None =>
        // This is a non-native JS class
        fb += CALL(genFunctionName.loadJSClass(className))

      case Some(loadSpec) =>
        genLoadJSFromSpec(fb, loadSpec)
    }
  }

  def genLoadJSFromSpec(fb: FunctionBuilder, loadSpec: JSNativeLoadSpec)(implicit
      ctx: WasmContext
  ): Unit = {
    def genFollowPath(path: List[String]): Unit = {
      for (prop <- path) {
        fb ++= ctx.getConstantStringInstr(prop)
        fb += CALL(genFunctionName.jsSelect)
      }
    }

    loadSpec match {
      case JSNativeLoadSpec.Global(globalRef, path) =>
        fb ++= ctx.getConstantStringInstr(globalRef)
        fb += CALL(genFunctionName.jsGlobalRefGet)
        genFollowPath(path)
      case JSNativeLoadSpec.Import(module, path) =>
        fb += GLOBAL_GET(ctx.getImportedModuleGlobal(module))
        genFollowPath(path)
      case JSNativeLoadSpec.ImportWithGlobalFallback(importSpec, globalSpec) =>
        genLoadJSFromSpec(fb, importSpec)
    }
  }

}
