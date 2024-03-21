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
    val derivedIRFiles: Future[Seq[Option[IRFile]]] = Future.traverse(irFiles) { irFile =>
      val irFileImpl = IRFileImpl.fromIRFile(irFile)
      irFileImpl.entryPointsInfo.flatMap { entryPointsInfo =>
        entryPointsInfo.className match {
          case BoxedCharacterClass | BoxedLongClass =>
            irFileImpl.tree.map { classDef =>
              Some(MemClassDefIRFile(deriveBoxClass(classDef)))
            }
          case _ =>
            Future.successful(None)
        }
      }
    }

    derivedIRFiles.map { derived =>
      derived.flatten ++ Seq(StackTraceIRFile) ++ irFiles
    }
  }

  private val StackTraceIRFile: IRFile = {
    val arrayOfSTERef = ArrayTypeRef(ClassRef("java.lang.StackTraceElement"), 1)

    val classDef = ClassDef(
      ClassIdent("java.lang.StackTrace$"),
      NON,
      ModuleClass,
      None,
      Some(ClassIdent(ObjectClass)),
      Nil,
      None,
      None,
      Nil,
      List(
        trivialCtor("java.lang.StackTrace$"),
        MethodDef(
          EMF,
          MethodIdent(m("captureJSError", List(ClassRef(ThrowableClass)), O)),
          NON,
          List(paramDef("throwable", ClassType(ThrowableClass))),
          AnyType,
          Some(
            JSNew(JSGlobalRef("Error"), Nil)
          )
        )(EOH.withInline(true), NOV),
        MethodDef(
          EMF,
          MethodIdent(m("extract", List(O), arrayOfSTERef)),
          NON,
          List(paramDef("jsError", AnyType)),
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

    MemClassDefIRFile(classDef)
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
}
