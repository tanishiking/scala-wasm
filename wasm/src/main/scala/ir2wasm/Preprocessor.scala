package ir2wasm

import wasm.wasm4s._

import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Names => IRNames}
import WasmContext._
import org.scalajs.ir.ClassKind
object Preprocessor {
  // def preprocess(clazz: List[IRTrees.ClassDef])(implicit ctx: WasmContext)
  def preprocess(clazz: IRTrees.ClassDef)(implicit ctx: WasmContext): Unit = {
    clazz.kind match {
      case ClassKind.ModuleClass | ClassKind.Class | ClassKind.Interface =>
        collectMethods(clazz)
      case ClassKind.JSClass | ClassKind.JSModuleClass | ClassKind.NativeJSModuleClass |
          ClassKind.AbstractJSType | ClassKind.NativeJSClass | ClassKind.HijackedClass =>
        ???
    }
  }
  private def collectMethods(clazz: IRTrees.ClassDef)(implicit ctx: WasmContext): Unit = {
    val infos =
      if (clazz.name.name == IRNames.ObjectClass)
        clazz.methods.filter(_.name.name == IRNames.NoArgConstructorName).map { method =>
          makeWasmFunctionInfo(clazz, method)
        }
      else
        clazz.methods.map { method =>
          makeWasmFunctionInfo(clazz, method)
        }
    ctx.putClassInfo(
      clazz.name.name,
      WasmClassInfo(
        clazz.name.name,
        clazz.kind,
        infos,
        clazz.fields.collect { case f: IRTrees.FieldDef => Names.WasmFieldName(f.name.name) },
        clazz.superClass.map(_.name),
        clazz.interfaces.map(_.name)
      )
    )
  }

  private def makeWasmFunctionInfo(
      clazz: IRTrees.ClassDef,
      method: IRTrees.MethodDef
  ): WasmFunctionInfo = {
    WasmFunctionInfo(
      Names.WasmFunctionName(clazz.name.name, method.name.name),
      method.args.map(_.ptpe),
      method.resultType,
      isAbstract = method.body.isEmpty
    )
  }
}
