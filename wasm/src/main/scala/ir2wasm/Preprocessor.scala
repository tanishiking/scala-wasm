package ir2wasm

import wasm.wasm4s._

import org.scalajs.ir.{Trees => IRTrees}
import wasm.ir2wasm.TypeTransformer._
import WasmContext._
import org.scalajs.ir.Trees.FieldDef

object Preprocessor {
  def collectMethods(clazz: IRTrees.ClassDef)(implicit ctx: WasmContext): Unit = {
    val infos = clazz.methods.collect {
      case method if !method.flags.namespace.isConstructor =>
        val name = Names.WasmFunctionName(clazz.name.name, method.name.name)
        val receiverType = makeReceiverType(clazz.name.name)
        val sig = WasmFunctionSignature(
            receiverType +: method.args.map(a => transformType(a.ptpe)),
            transformResultType(method.resultType)
        )
        val typeName = ctx.addFunctionType(sig)
        WasmFunctionInfo(
          Names.WasmFunctionName(clazz.name.name, method.name.name),
          WasmFunctionType(typeName, sig)
        )
    }
    ctx.classInfo.put(clazz.name.name, WasmClassInfo(
        infos,
        clazz.fields.collect { case f: FieldDef => Names.WasmFieldName(f.name.name) },
        clazz.superClass.map(_.name)))
  }
}
