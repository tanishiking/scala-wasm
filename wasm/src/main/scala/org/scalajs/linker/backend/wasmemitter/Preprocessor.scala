package org.scalajs.linker.backend.wasmemitter

import scala.collection.mutable

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.{ClassKind, Traversers}

import org.scalajs.linker.standard.{LinkedClass, LinkedTopLevelExport}

import EmbeddedConstants._
import WasmContext._

object Preprocessor {
  def preprocess(classes: List[LinkedClass], tles: List[LinkedTopLevelExport])(implicit
      ctx: WasmContext
  ): Unit = {
    val staticFieldMirrors = computeStaticFieldMirrors(tles)

    val definedReflectiveProxyNames = mutable.HashSet.empty[MethodName]

    for (clazz <- classes) {
      preprocess(clazz, staticFieldMirrors.getOrElse(clazz.className, Map.empty))

      // For Scala classes, collect the reflective proxy method names that it defines
      if (clazz.kind.isClass || clazz.kind == ClassKind.HijackedClass) {
        for (method <- clazz.methods if method.methodName.isReflectiveProxy)
          definedReflectiveProxyNames += method.methodName
      }
    }

    // sort for stability
    ctx.setReflectiveProxyIDs(definedReflectiveProxyNames.toList.sorted.zipWithIndex.toMap)

    val collector = new AbstractMethodCallCollector(ctx)
    for (clazz <- classes)
      collector.collectAbstractMethodCalls(clazz)
    for (tle <- tles)
      collector.collectAbstractMethodCalls(tle)

    for (clazz <- classes) {
      ctx.getClassInfo(clazz.className).buildMethodTable()
    }
    assignBuckets(classes)
  }

  private def computeStaticFieldMirrors(
      tles: List[LinkedTopLevelExport]
  ): Map[ClassName, Map[FieldName, List[String]]] = {
    var result = Map.empty[ClassName, Map[FieldName, List[String]]]
    for (tle <- tles) {
      tle.tree match {
        case TopLevelFieldExportDef(_, exportName, FieldIdent(fieldName)) =>
          val className = tle.owningClass
          val mirrors = result.getOrElse(className, Map.empty)
          val newExportNames = exportName :: mirrors.getOrElse(fieldName, Nil)
          val newMirrors = mirrors.updated(fieldName, newExportNames)
          result = result.updated(className, newMirrors)

        case _ =>
      }
    }
    result
  }

  private def preprocess(clazz: LinkedClass, staticFieldMirrors: Map[FieldName, List[String]])(
      implicit ctx: WasmContext
  ): Unit = {
    val kind = clazz.kind

    val allFieldDefs: List[FieldDef] =
      if (kind.isClass) {
        val inheritedFields = clazz.superClass match {
          case None      => Nil
          case Some(sup) => ctx.getClassInfo(sup.name).allFieldDefs
        }
        val myFieldDefs = clazz.fields.collect {
          case fd: FieldDef if !fd.flags.namespace.isStatic =>
            fd
          case fd: JSFieldDef =>
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
          if m.body.isDefined && m.flags.namespace == MemberNamespace.Public
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
      new ClassInfo(
        ctx,
        clazz.name.name,
        kind,
        clazz.jsClassCaptures,
        classConcretePublicMethodNames,
        allFieldDefs,
        clazz.superClass.map(_.name),
        clazz.ancestors,
        clazz.hasInstances,
        !clazz.hasDirectInstances,
        hasRuntimeTypeInfo,
        clazz.jsNativeLoadSpec,
        clazz.jsNativeMembers.map(m => m.name.name -> m.jsNativeLoadSpec).toMap,
        staticFieldMirrors,
        _itableIdx = -1
      )
    )

    // Update specialInstanceTypes for ancestors of hijacked classes
    if (clazz.kind == ClassKind.HijackedClass) {
      def addSpecialInstanceTypeOnAllAncestors(jsValueType: Int): Unit =
        clazz.ancestors.foreach(ctx.getClassInfo(_).addSpecialInstanceType(jsValueType))

      clazz.className match {
        case BoxedBooleanClass =>
          addSpecialInstanceTypeOnAllAncestors(JSValueTypeFalse)
          addSpecialInstanceTypeOnAllAncestors(JSValueTypeTrue)
        case BoxedStringClass =>
          addSpecialInstanceTypeOnAllAncestors(JSValueTypeString)
        case BoxedDoubleClass =>
          addSpecialInstanceTypeOnAllAncestors(JSValueTypeNumber)
        case BoxedUnitClass =>
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

  /** Collect FunctionInfo based on the abstract method call
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
        case TopLevelMethodExportDef(_, jsMethodDef) =>
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
            case ClassType(className) =>
              val classInfo = ctx.getClassInfo(className)
              if (classInfo.hasInstances)
                classInfo.registerDynamicCall(methodName.name)
            case AnyType =>
              ctx.getClassInfo(ObjectClass).registerDynamicCall(methodName.name)
            case _ =>
              // For all other cases, including arrays, we will always perform a static dispatch
              ()
          }

        case _ =>
          ()
      }
    }
  }

  /** Group interface types + types that implements any interfaces into buckets, where no two types
    * in the same bucket can have common subtypes.
    *
    * It allows compressing the itable by reusing itable's index (buckets) for unrelated types,
    * instead of having a 1-1 mapping from type to index. As a result, the itables' length will be
    * the same as the number of buckets).
    *
    * The algorithm separates the type hierarchy into three disjoint subsets,
    *
    *   - join types: types with multiple parents (direct supertypes) that have only single
    *     subtyping descendants: `join(T) = {x ∈ multis(T) | ∄ y ∈ multis(T) : y <: x}` where
    *     multis(T) means types with multiple direct supertypes.
    *   - spine types: all ancestors of join types: `spine(T) = {x ∈ T | ∃ y ∈ join(T) : x ∈
    *     ancestors(y)}`
    *   - plain types: types that are neither join nor spine types
    *
    * The bucket assignment process consists of two parts:
    *
    * **1. Assign buckets to spine types**
    *
    * Two spine types can share the same bucket only if they do not have any common join type
    * descendants.
    *
    * Visit spine types in reverse topological order because (from leaves to root) when assigning a
    * a spine type to bucket, the algorithm already has the complete information about the
    * join/spine type descendants of that spine type.
    *
    * Assign a bucket to a spine type if adding it doesn't violate the bucket assignment rule: two
    * spine types can share a bucket only if they don't have any common join type descendants. If no
    * existing bucket satisfies the rule, create a new bucket.
    *
    * **2. Assign buckets to non-spine types (plain and join types)**
    *
    * Visit these types in level order (from root to leaves) For each type, compute the set of
    * buckets already used by its ancestors. Assign the type to any available bucket not in this
    * set. If no available bucket exists, create a new one.
    *
    * To test if type A is a subtype of type B: load the bucket index of type B (we do this by
    * `getItableIdx`), load the itable at that index from A, and check if the itable is an itable
    * for B.
    *
    * @see
    *   This algorithm is based on the "packed encoding" presented in the paper "Efficient Type
    *   Inclusion Tests"
    *   [[https://www.researchgate.net/publication/2438441_Efficient_Type_Inclusion_Tests]]
    */
  private def assignBuckets(allClasses: List[LinkedClass])(implicit ctx: WasmContext): Unit = {
    val classes = allClasses.filterNot(_.kind.isJSType)

    var nextIdx = 0
    def newBucket(): Bucket = {
      val idx = nextIdx
      nextIdx += 1
      new Bucket(idx)
    }
    def getAllInterfaces(clazz: LinkedClass): List[ClassName] =
      clazz.ancestors.filter(ctx.getClassInfo(_).isInterface)

    val buckets = new mutable.ListBuffer[Bucket]()

    /** All join type descendants of the class */
    val joinsOf =
      new mutable.HashMap[ClassName, mutable.HashSet[ClassName]]()

    /** the buckets that have been assigned to any of the ancestors of the class */
    val usedOf = new mutable.HashMap[ClassName, mutable.HashSet[Bucket]]()
    val spines = new mutable.HashSet[ClassName]()

    for (clazz <- classes.reverseIterator) {
      val info = ctx.getClassInfo(clazz.name.name)
      val ifaces = getAllInterfaces(clazz)
      if (ifaces.nonEmpty) {
        val joins = joinsOf.getOrElse(clazz.name.name, new mutable.HashSet())

        if (joins.nonEmpty) { // spine type
          var found = false
          val bs = buckets.iterator
          // look for an existing bucket to add the spine type to
          while (!found && bs.hasNext) {
            val b = bs.next()
            // two spine types can share a bucket only if they don't have any common join type descendants
            if (!b.joins.exists(joins)) {
              found = true
              b.add(info)
              b.joins ++= joins
            }
          }
          if (!found) { // there's no bucket to add, create new bucket
            val b = newBucket()
            b.add(info)
            buckets.append(b)
            b.joins ++= joins
          }
          for (iface <- ifaces) {
            joinsOf.getOrElseUpdate(iface, new mutable.HashSet()) ++= joins
          }
          spines.add(clazz.name.name)
        } else if (ifaces.length > 1) { // join type, add to joins map, bucket assignment is done later
          ifaces.foreach { iface =>
            joinsOf.getOrElseUpdate(iface, new mutable.HashSet()) += clazz.name.name
          }
        }
        // else: plain, do nothing
      }

    }

    for (clazz <- classes) {
      val info = ctx.getClassInfo(clazz.name.name)
      val ifaces = getAllInterfaces(clazz)
      if (ifaces.nonEmpty && !spines.contains(clazz.name.name)) {
        val used = usedOf.getOrElse(clazz.name.name, new mutable.HashSet())
        for {
          iface <- ifaces
          parentUsed <- usedOf.get(iface)
        } { used ++= parentUsed }

        var found = false
        val bs = buckets.iterator
        while (!found && bs.hasNext) {
          val b = bs.next()
          if (!used.contains(b)) {
            found = true
            b.add(info)
            used.add(b)
          }
        }
        if (!found) {
          val b = newBucket()
          buckets.append(b)
          b.add(info)
          used.add(b)
        }
      }
    }

    ctx.setItablesLength(buckets.length)
  }

  private final class Bucket(idx: Int) {
    def add(clazz: ClassInfo) = clazz.setItableIdx((idx))

    /** A set of join types that are descendants of the types assigned to that bucket */
    val joins = new mutable.HashSet[ClassName]()
  }

}
