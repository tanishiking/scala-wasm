package wasm.ir2wasm

import scala.concurrent.{ExecutionContext, Future}

import org.scalajs.ir.ClassKind._
import org.scalajs.ir.Names._
import org.scalajs.ir.Position
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.interface.unstable.IRFileImpl

import wasm.utils.TestIRBuilder._
import wasm.utils.MemClassDefIRFile

/** Patches that we apply to the standard library classes to make them wasm-friendly. */
object LibraryPatches {
  def patchIRFiles(irFiles: Seq[IRFile])(implicit ec: ExecutionContext): Future[Seq[IRFile]] = {
    val patched1: Future[Seq[IRFile]] = Future.traverse(irFiles) { irFile =>
      val irFileImpl = IRFileImpl.fromIRFile(irFile)
      irFileImpl.entryPointsInfo.flatMap { entryPointsInfo =>
        MethodPatches.get(entryPointsInfo.className) match {
          case None =>
            Future.successful(irFile)
          case Some(patches) =>
            irFileImpl.tree.map(classDef => MemClassDefIRFile(applyMethodPatches(classDef, patches)))
        }
      }
    }

    patched1.map(FloatingPointBitsIRFile +: _)
  }

  private val FloatingPointBitsIRFile: IRFile = {
    val classDef = ClassDef(
      ClassIdent("java.lang.FloatingPointBits$"),
      NON,
      ModuleClass,
      None,
      Some(ClassIdent(ObjectClass)),
      Nil,
      None,
      None,
      Nil,
      List(
        trivialCtor("java.lang.FloatingPointBits$"),
        MethodDef(
          EMF, MethodIdent(m("numberHashCode", List(D), I)), NON,
          List(paramDef("value", DoubleType)), IntType,
          Some(Block(
            // TODO This is not a compliant but it's good enough for now
            UnaryOp(UnaryOp.DoubleToInt, VarRef("value")(DoubleType))
          ))
        )(EOH, NOV)
      ),
      None,
      Nil,
      Nil,
      Nil
    )(EOH)

    MemClassDefIRFile(classDef)
  }

  private val MethodPatches: Map[ClassName, List[MethodDef]] = {
    Map(
      ObjectClass -> List(
        // TODO Remove this patch when we support getClass() and full string concatenation
        MethodDef(
          EMF, m("toString", Nil, T), NON,
          Nil, ClassType(BoxedStringClass),
          Some(StringLiteral("[object]"))
        )(EOH, NOV)
      ),

      BoxedCharacterClass.withSuffix("$") -> List(
        MethodDef(
          EMF, m("toString", List(C), T), NON,
          List(paramDef("c", CharType)), ClassType(BoxedStringClass),
          Some(BinaryOp(BinaryOp.String_+, StringLiteral(""), VarRef("c")(CharType)))
        )(EOH, NOV)
      ),

      BoxedIntegerClass.withSuffix("$") -> List(
        MethodDef(
          EMF, m("toHexString", List(I), T), NON,
          List(paramDef("i", IntType)), ClassType(BoxedStringClass),
          Some(
            // TODO Write a compliant implementation
            BinaryOp(BinaryOp.String_+, StringLiteral(""), VarRef("i")(IntType))
          )
        )(EOH, NOV)
      )
    )
  }

  private def applyMethodPatches(classDef: ClassDef, patches: List[MethodDef]): ClassDef = {
    val patchesMap = patches.map(m => m.name.name -> m).toMap
    val patchedMethods = classDef.methods.map(m => patchesMap.getOrElse(m.name.name, m))

    import classDef._
    ClassDef(
      name,
      originalName,
      kind,
      jsClassCaptures,
      superClass,
      interfaces,
      jsSuperClass,
      jsNativeLoadSpec,
      fields,
      patchedMethods,
      jsConstructor,
      jsMethodProps,
      jsNativeMembers,
      topLevelExportDefs
    )(EOH)(pos)
  }
}
