package wasm.ir2wasm

import wasm.wasm4s._

import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.ClassKind
import org.scalajs.ir.Traversers

import org.scalajs.linker.standard.{LinkedClass, LinkedTopLevelExport}

import EmbeddedConstants._
import WasmContext._

object Preprocessor {
  def preprocess(classes: List[LinkedClass], tles: List[LinkedTopLevelExport])(implicit
      ctx: WasmContext
  ): Unit = {
    for (clazz <- classes)
      preprocess(clazz)

    val collector = new AbstractMethodCallCollector(ctx)
    for (clazz <- classes)
      collector.collectAbstractMethodCalls(clazz)
    for (tle <- tles)
      collector.collectAbstractMethodCalls(tle)

    for (clazz <- classes) {
      ctx.getClassInfo(clazz.className).buildMethodTable()

      if (clazz.kind == ClassKind.Interface && clazz.hasInstanceTests)
        HelperFunctions.genInstanceTest(clazz)
      HelperFunctions.genCloneFunction(clazz)
    }
  }

  private def preprocess(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    val kind = clazz.kind

    val allFieldDefs: List[IRTrees.FieldDef] =
      if (kind.isClass) {
        val inheritedFields = clazz.superClass match {
          case None      => Nil
          case Some(sup) => ctx.getClassInfo(sup.name).allFieldDefs
        }
        val myFieldDefs = clazz.fields.collect {
          case fd: IRTrees.FieldDef if !fd.flags.namespace.isStatic =>
            fd
          case fd: IRTrees.JSFieldDef =>
            throw new AssertionError(s"Illegal $fd in Scala class ${clazz.className}")
        }
        inheritedFields ::: myFieldDefs
      } else {
        Nil
      }

    val classConcretePublicMethodNames = {
      if (kind.isClass || kind == ClassKind.HijackedClass) {
        for {
          m <- clazz.methods
          if m.body.isDefined && m.flags.namespace == IRTrees.MemberNamespace.Public
        } yield {
          m.methodName
        }
      } else {
        Nil
      }
    }

    /* Should we emit a vtable/typeData global for this class?
     *
     * There are essentially three reasons for which we need them:
     *
     * - Because there is a `classOf[C]` somewhere in the program; if that is
     *   true, then `clazz.hasRuntimeTypeInfo` is true.
     * - Because it is the vtable of a class with direct instances; in that
     *   case `clazz.hasRuntimeTypeInfo` is also true, as guaranteed by the
     *   Scala.js frontend analysis.
     * - Because we generate a test of the form `isInstanceOf[Array[C]]`. In
     *   that case, `clazz.hasInstanceTests` is true.
     *
     * `clazz.hasInstanceTests` is also true if there is only `isInstanceOf[C]`,
     * in the program, so that is not *optimal*, but it is correct.
     */
    val hasRuntimeTypeInfo = clazz.hasRuntimeTypeInfo || clazz.hasInstanceTests

    ctx.putClassInfo(
      clazz.name.name,
      new WasmClassInfo(
        ctx,
        clazz.name.name,
        kind,
        clazz.jsClassCaptures,
        classConcretePublicMethodNames,
        allFieldDefs,
        clazz.superClass.map(_.name),
        clazz.interfaces.map(_.name),
        clazz.ancestors,
        clazz.hasInstances,
        !clazz.hasDirectInstances,
        hasRuntimeTypeInfo,
        clazz.jsNativeLoadSpec,
        clazz.jsNativeMembers.map(m => m.name.name -> m.jsNativeLoadSpec).toMap
      )
    )

    // Update specialInstanceTypes for ancestors of hijacked classes
    if (clazz.kind == ClassKind.HijackedClass) {
      def addSpecialInstanceTypeOnAllAncestors(jsValueType: Int): Unit =
        clazz.ancestors.foreach(ctx.getClassInfo(_).addSpecialInstanceType(jsValueType))

      clazz.className match {
        case IRNames.BoxedBooleanClass =>
          addSpecialInstanceTypeOnAllAncestors(JSValueTypeFalse)
          addSpecialInstanceTypeOnAllAncestors(JSValueTypeTrue)
        case IRNames.BoxedStringClass =>
          addSpecialInstanceTypeOnAllAncestors(JSValueTypeString)
        case IRNames.BoxedDoubleClass =>
          addSpecialInstanceTypeOnAllAncestors(JSValueTypeNumber)
        case IRNames.BoxedUnitClass =>
          addSpecialInstanceTypeOnAllAncestors(JSValueTypeUndefined)
        case _ =>
          ()
      }
    }

    /* Work around https://github.com/scala-js/scala-js/issues/4972
     * Manually mark all ancestors of instantiated classes as having instances.
     */
    if (clazz.hasDirectInstances && !kind.isJSType)
      clazz.ancestors.foreach(ancestor => ctx.getClassInfo(ancestor).setHasInstances())
  }

  /** Collect WasmFunctionInfo based on the abstract method call
    *
    * ```
    * class A extends B:
    *   def a = 1
    *
    * class B extends C:
    *   def b: Int = 1
    *   override def c: Int = 1
    *
    * abstract class C:
    *   def c: Int
    * ```
    *
    * why we need this? - The problem is that the frontend linker gets rid of abstract method
    * entirely.
    *
    * It keeps B.c because it's concrete and used. But because `C.c` isn't there at all anymore, if
    * we have val `x: C` and we call `x.c`, we don't find the method at all.
    */
  private class AbstractMethodCallCollector(ctx: WasmContext) extends Traversers.Traverser {
    import IRTrees._

    def collectAbstractMethodCalls(clazz: LinkedClass): Unit = {
      for (method <- clazz.methods)
        traverseMethodDef(method)
      for (jsConstructor <- clazz.jsConstructorDef)
        traverseJSConstructorDef(jsConstructor)
      for (export <- clazz.exportedMembers)
        traverseJSMethodPropDef(export)
    }

    def collectAbstractMethodCalls(tle: LinkedTopLevelExport): Unit = {
      tle.tree match {
        case IRTrees.TopLevelMethodExportDef(_, jsMethodDef) =>
          traverseJSMethodPropDef(jsMethodDef)
        case _ =>
          ()
      }
    }

    override def traverse(tree: Tree): Unit = {
      super.traverse(tree)

      tree match {
        case Apply(flags, receiver, methodName, _) if !methodName.name.isReflectiveProxy =>
          receiver.tpe match {
            case IRTypes.ClassType(className) =>
              val classInfo = ctx.getClassInfo(className)
              if (classInfo.hasInstances)
                classInfo.registerDynamicCall(methodName.name)
            case IRTypes.AnyType | IRTypes.ArrayType(_) =>
              ctx.getClassInfo(IRNames.ObjectClass).registerDynamicCall(methodName.name)
            case _ =>
              // For all other cases, we will always perform a static dispatch
              ()
          }

        case _ =>
          ()
      }
    }
  }
}
