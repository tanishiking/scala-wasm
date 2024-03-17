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
    val derivedCharBox = new java.util.concurrent.atomic.AtomicReference[IRFile](null)
    val derivedLongBox = new java.util.concurrent.atomic.AtomicReference[IRFile](null)

    val patched1: Future[Seq[IRFile]] = Future.traverse(irFiles) { irFile =>
      val irFileImpl = IRFileImpl.fromIRFile(irFile)
      irFileImpl.entryPointsInfo.flatMap { entryPointsInfo =>
        MethodPatches.get(entryPointsInfo.className) match {
          case None =>
            entryPointsInfo.className match {
              case BoxedCharacterClass | BoxedLongClass =>
                irFileImpl.tree.map { classDef =>
                  val derivedBox = MemClassDefIRFile(deriveBoxClass(classDef))
                  if (classDef.className == BoxedCharacterClass)
                    derivedCharBox.set(derivedBox)
                  else
                    derivedLongBox.set(derivedBox)
                  irFile
                }
              case _ =>
                Future.successful(irFile)
            }
          case Some(patches) =>
            irFileImpl.tree.map(classDef => MemClassDefIRFile(applyMethodPatches(classDef, patches)))
        }
      }
    }

    patched1.map { irFiles1 =>
      val extra = List(FloatingPointBitsIRFile, derivedCharBox.get(), derivedLongBox.get())
      extra ++ irFiles1
    }
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

  /** Generates the accompanying Box class of `Character` or `Long`.
   *
   *  These box classes will be used as the generic representation of `char`s
   *  and `long`s when they are upcast to `java.lang.Character`/`java.lang.Long`
   *  or any of their supertypes.
   *
   *  The generated Box classes mimic the public structure of the corresponding
   *  hijacked classes. Whereas the hijacked classes instances *are* the
   *  primitives (conceptually), the box classes contain an explicit `value`
   *  field of the primitive type. They delegate all their instance methods to
   *  the corresponding methods of the hijacked class, applied on the `value`
   *  primitive.
   *
   *  For example, given the hijacked class
   *
   *  {{{
   *  hijacked class Long extends java.lang.Number with Comparable {
   *    def longValue;J(): long = this.asInstanceOf[long]
   *    def toString;T(): string = Long$.toString(this.longValue;J())
   *    def compareTo;jlLong;Z(that: java.lang.Long): boolean =
   *      Long$.compare(this.longValue;J(), that.longValue;J())
   *  }
   *  }}}
   *
   *  we generate
   *
   *  {{{
   *  class LongBox extends java.lang.Number with Comparable {
   *    val value: long
   *    def <init>(value: long) = { this.value = value }
   *    def longValue;J(): long = this.value.longValue;J()
   *    def toString;T(): string = this.value.toString;J()
   *    def compareTo;jlLong;Z(that: jlLong): boolean =
   *      this.value.compareTo;jlLong;Z(that)
   *  }
   *  }}}
   */
  private def deriveBoxClass(classDef: ClassDef): ClassDef = {
    val className = classDef.className
    val derivedClassName = className.withSuffix("Box")
    val primType = BoxedClassToPrimType(className).asInstanceOf[PrimTypeWithRef]
    val derivedClassType = ClassType(derivedClassName)

    val fieldName = FieldName(derivedClassName, SimpleFieldName("value"))
    val fieldIdent = FieldIdent(fieldName)

    val derivedFields: List[FieldDef] = List(
      FieldDef(EMF, fieldIdent, NON, primType)
    )

    val selectField = Select(This()(derivedClassType), fieldIdent)(primType)

    val ctorParamDef = ParamDef(LocalIdent(fieldName.simpleName.toLocalName), NON, primType, mutable = false)
    val derivedCtor = MethodDef(
      EMF.withNamespace(MemberNamespace.Constructor),
      MethodIdent(MethodName.constructor(List(primType.primRef))),
      NON,
      List(ctorParamDef),
      NoType,
      Some(
        Assign(selectField, ctorParamDef.ref)
      )
    )(EOH, NOV)

    val derivedMethods: List[MethodDef] = for {
      method <- classDef.methods if method.flags.namespace == MemberNamespace.Public
    } yield {
      MethodDef(
        method.flags, method.name, method.originalName, method.args, method.resultType,
        Some(
          Apply(
            EAF,
            selectField,
            method.name,
            method.args.map(_.ref)
          )(method.resultType)
        )
      )(method.optimizerHints, method.version)
    }

    locally {
      import classDef.{pos => _, _}

      ClassDef(
        ClassIdent(derivedClassName),
        NON,
        Class,
        None,
        superClass,
        interfaces,
        None,
        None,
        derivedFields,
        derivedCtor :: derivedMethods,
        None,
        Nil,
        Nil,
        Nil
      )(EOH)
    }
  }
}
