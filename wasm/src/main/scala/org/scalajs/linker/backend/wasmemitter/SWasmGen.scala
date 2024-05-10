package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.JSNativeLoadSpec

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.WasmInstr._

import VarGen._

/** Scala.js-specific Wasm generators that are used across the board. */
object SWasmGen {

  def genLoadJSConstructor(fb: FunctionBuilder, className: ClassName)(implicit
      ctx: TypeDefinableWasmContext
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
      ctx: TypeDefinableWasmContext
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
