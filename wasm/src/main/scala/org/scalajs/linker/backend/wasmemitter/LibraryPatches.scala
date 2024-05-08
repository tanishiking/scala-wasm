package org.scalajs.linker.backend.wasmemitter

import scala.concurrent.{ExecutionContext, Future}

import org.scalajs.ir.ClassKind._
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Position
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.{EntryPointsInfo, Version}

import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.interface.unstable.IRFileImpl

/** Patches that we apply to the standard library classes to make them wasm-friendly. */
object LibraryPatches {
  def patchIRFiles(irFiles: Seq[IRFile])(implicit ec: ExecutionContext): Future[Seq[IRFile]] = {
    val derivedIRFiles: Future[Seq[Option[IRFile]]] = Future.traverse(irFiles) { irFile =>
      val irFileImpl = IRFileImpl.fromIRFile(irFile)
      irFileImpl.entryPointsInfo.flatMap { entryPointsInfo =>
        entryPointsInfo.className match {
          case BoxedCharacterClass | BoxedLongClass =>
            irFileImpl.tree.map { classDef =>
              Some(new MemClassDefIRFile(deriveBoxClass(classDef)))
            }
          case _ =>
            Future.successful(None)
        }
      }
    }

    val leanerJSExceptionIRFile =
      org.scalajs.linker.backend.emitter.PrivateLibHolder.files.find { irFile =>
        IRFileImpl.fromIRFile(irFile).path.contains("JavaScriptException")
      }.get

    derivedIRFiles.map { derived =>
      derived.flatten ++ Seq(StackTraceIRFile) ++ irFiles ++ Seq(leanerJSExceptionIRFile)
    }
  }

  private val EAF = ApplyFlags.empty
  private val EMF = MemberFlags.empty
  private val EOH = OptimizerHints.empty
  private val NON = NoOriginalName
  private val NOV = Version.Unversioned

  private def trivialCtor(enclosingClassName: ClassName)(implicit pos: Position): MethodDef = {
    val flags = MemberFlags.empty.withNamespace(MemberNamespace.Constructor)
    MethodDef(
      flags,
      MethodIdent(NoArgConstructorName),
      NON,
      Nil,
      NoType,
      Some(
        ApplyStatically(
          EAF.withConstructor(true),
          This()(ClassType(enclosingClassName)),
          ObjectClass,
          MethodIdent(NoArgConstructorName),
          Nil
        )(NoType)
      )
    )(EOH, NOV)
  }

  private val StackTraceIRFile: IRFile = {
    implicit val noPosition: Position = Position.NoPosition

    val StackTraceModuleClass = ClassName("java.lang.StackTrace$")
    val StackTraceElementClass = ClassName("java.lang.StackTraceElement")

    val arrayOfSTERef = ArrayTypeRef(ClassRef(StackTraceElementClass), 1)

    val O = ClassRef(ObjectClass)

    val classDef = ClassDef(
      ClassIdent(StackTraceModuleClass),
      NON,
      ModuleClass,
      None,
      Some(ClassIdent(ObjectClass)),
      Nil,
      None,
      None,
      Nil,
      List(
        trivialCtor(StackTraceModuleClass),
        MethodDef(
          EMF,
          MethodIdent(MethodName("captureJSError", List(ClassRef(ThrowableClass)), O)),
          NON,
          List(
            ParamDef(LocalIdent(LocalName("th")), NON, ClassType(ThrowableClass), mutable = false)
          ),
          AnyType,
          Some(
            JSNew(JSGlobalRef("Error"), Nil)
          )
        )(EOH.withInline(true), NOV),
        MethodDef(
          EMF,
          MethodIdent(MethodName("extract", List(O), arrayOfSTERef)),
          NON,
          List(ParamDef(LocalIdent(LocalName("jsError")), NON, AnyType, mutable = false)),
          ArrayType(arrayOfSTERef),
          Some(
            ArrayValue(arrayOfSTERef, Nil)
          )
        )(EOH.withInline(true), NOV),
        MethodDef(
          EMF,
          MethodIdent(MethodName("getCurrentStackTrace", Nil, arrayOfSTERef)),
          NON,
          Nil,
          ArrayType(arrayOfSTERef),
          Some(
            ArrayValue(arrayOfSTERef, Nil)
          )
        )(EOH.withInline(true), NOV)
      ),
      None,
      Nil,
      Nil,
      Nil
    )(EOH)

    new MemClassDefIRFile(classDef)
  }

  /** Generates the accompanying Box class of `Character` or `Long`.
    *
    * These box classes will be used as the generic representation of `char`s and `long`s when they
    * are upcast to `java.lang.Character`/`java.lang.Long` or any of their supertypes.
    *
    * The generated Box classes mimic the public structure of the corresponding hijacked classes.
    * Whereas the hijacked classes instances *are* the primitives (conceptually), the box classes
    * contain an explicit `value` field of the primitive type. They delegate all their instance
    * methods to the corresponding methods of the hijacked class, applied on the `value` primitive.
    *
    * For example, given the hijacked class
    *
    * {{{
    *  hijacked class Long extends java.lang.Number with Comparable {
    *    def longValue;J(): long = this.asInstanceOf[long]
    *    def toString;T(): string = Long$.toString(this.longValue;J())
    *    def compareTo;jlLong;Z(that: java.lang.Long): boolean =
    *      Long$.compare(this.longValue;J(), that.longValue;J())
    *  }
    * }}}
    *
    * we generate
    *
    * {{{
    *  class LongBox extends java.lang.Number with Comparable {
    *    val value: long
    *    def <init>(value: long) = { this.value = value }
    *    def longValue;J(): long = this.value.longValue;J()
    *    def toString;T(): string = this.value.toString;J()
    *    def compareTo;jlLong;Z(that: jlLong): boolean =
    *      this.value.compareTo;jlLong;Z(that)
    *  }
    * }}}
    */
  private def deriveBoxClass(classDef: ClassDef): ClassDef = {
    implicit val pos: Position = classDef.pos

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

    val ctorParamDef =
      ParamDef(LocalIdent(fieldName.simpleName.toLocalName), NON, primType, mutable = false)
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
        method.flags,
        method.name,
        method.originalName,
        method.args,
        method.resultType,
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

  /** An in-memory IRFile for a ClassDef.
    *
    * Adapted from Scala.js upstream.
    */
  private final class MemClassDefIRFile(classDef: ClassDef)
      extends IRFileImpl("mem://" + classDef.name.name + ".sjsir", Version.Unversioned) {

    def tree(implicit ec: ExecutionContext): Future[ClassDef] =
      Future(classDef)

    def entryPointsInfo(implicit ec: ExecutionContext): Future[EntryPointsInfo] =
      tree.map(EntryPointsInfo.forClassDef)
  }
}
