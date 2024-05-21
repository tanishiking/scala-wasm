package org.scalajs.linker.backend.wasmemitter

import scala.annotation.tailrec

import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees.{FieldDef, ParamDef, JSNativeLoadSpec}
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
import org.scalajs.linker.standard.LinkedTopLevelExport
import org.scalajs.linker.standard.LinkedClass

import org.scalajs.linker.backend.webassembly.ModuleBuilder
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Modules => wamod}
import org.scalajs.linker.backend.webassembly.{Identitities => wanme}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

import VarGen._
import org.scalajs.ir.OriginalName

final class WasmContext {
  import WasmContext._

  private val classInfo = mutable.Map[ClassName, ClassInfo]()
  private var reflectiveProxies: Map[MethodName, Int] = null

  private var _itablesLength: Int = 0
  def itablesLength = _itablesLength

  private val functionTypes = LinkedHashMap.empty[watpe.FunctionType, wanme.TypeID]
  private val tableFunctionTypes = mutable.HashMap.empty[MethodName, wanme.TypeID]
  private val constantStringGlobals = LinkedHashMap.empty[String, StringData]
  private val classItableGlobals = mutable.ListBuffer.empty[ClassName]
  private val closureDataTypes = LinkedHashMap.empty[List[Type], wanme.TypeID]

  val moduleBuilder: ModuleBuilder = {
    new ModuleBuilder(new ModuleBuilder.FunctionTypeProvider {
      def functionTypeToTypeName(sig: watpe.FunctionType): wanme.TypeID = {
        functionTypes.getOrElseUpdate(
          sig, {
            val typeName = genTypeID.forFunction(functionTypes.size)
            moduleBuilder.addRecType(typeName, NoOriginalName, sig)
            typeName
          }
        )
      }
    })
  }

  private var stringPool = new mutable.ArrayBuffer[Byte]()
  private var nextConstantStringIndex: Int = 0
  private var nextConstatnStringOffset: Int = 0
  private var nextClosureDataTypeIndex: Int = 1

  private val _importedModules: mutable.LinkedHashSet[String] =
    new mutable.LinkedHashSet()

  private val _jsPrivateFieldNames: mutable.ListBuffer[FieldName] =
    new mutable.ListBuffer()
  private val _funcDeclarations: mutable.LinkedHashSet[wanme.FunctionID] =
    new mutable.LinkedHashSet()

  /** The main `rectype` containing the object model types. */
  val mainRecType: ModuleBuilder.RecTypeBuilder = new ModuleBuilder.RecTypeBuilder

  /** Get an index of the itable for the given interface. The itable instance must be placed at the
    * index in the array of itables (whose size is `itablesLength`).
    */
  def getItableIdx(iface: ClassInfo): Int = {
    val idx = iface.itableIdx
    if (idx < 0) throw new IllegalArgumentException(s"Interface $iface is not registed.")
    idx
  }

  def getClassInfoOption(name: ClassName): Option[ClassInfo] =
    classInfo.get(name)

  def getClassInfo(name: ClassName): ClassInfo =
    classInfo.getOrElse(name, throw new Error(s"Class not found: $name"))

  def inferTypeFromTypeRef(typeRef: TypeRef): Type = typeRef match {
    case PrimRef(tpe) =>
      tpe
    case ClassRef(className) =>
      if (className == ObjectClass || getClassInfo(className).kind.isJSType)
        AnyType
      else
        ClassType(className)
    case typeRef: ArrayTypeRef =>
      ArrayType(typeRef)
  }

  /** Sets the map of reflexity proxy IDs, only for use by `Preprocessor`. */
  def setReflectiveProxyIDs(proxyIDs: Map[MethodName, Int]): Unit =
    reflectiveProxies = proxyIDs

  /** Retrieves a unique identifier for a reflective proxy with the given name.
    *
    * If no class defines a reflective proxy with the given name, returns `-1`.
    */
  def getReflectiveProxyId(name: MethodName): Int =
    reflectiveProxies.getOrElse(name, -1)

  /** Adds or reuses a function type for a table function.
    *
    * Table function types are part of the main `rectype`, and have names derived from the
    * `methodName`.
    */
  def tableFunctionType(methodName: MethodName): wanme.TypeID = {
    // Project all the names with the same *signatures* onto a normalized `MethodName`
    val normalizedName = MethodName(
      SpecialNames.normalizedSimpleMethodName,
      methodName.paramTypeRefs,
      methodName.resultTypeRef,
      methodName.isReflectiveProxy
    )

    tableFunctionTypes.getOrElseUpdate(
      normalizedName, {
        val typeName = genTypeID.forTableFunctionType(normalizedName)
        val regularParamTyps = normalizedName.paramTypeRefs.map { typeRef =>
          TypeTransformer.transformType(inferTypeFromTypeRef(typeRef))(this)
        }
        val resultTyp =
          TypeTransformer.transformResultType(inferTypeFromTypeRef(normalizedName.resultTypeRef))(
            this
          )
        mainRecType.addSubType(
          typeName,
          NoOriginalName,
          watpe.FunctionType(watpe.RefType.any :: regularParamTyps, resultTyp)
        )
        typeName
      }
    )
  }

  def addConstantStringGlobal(str: String): StringData = {
    constantStringGlobals.get(str) match {
      case Some(data) =>
        data

      case None =>
        val bytes = str.toCharArray.flatMap { char =>
          Array((char & 0xFF).toByte, (char >> 8).toByte)
        }
        val offset = nextConstatnStringOffset
        val data = StringData(nextConstantStringIndex, offset)
        constantStringGlobals(str) = data

        stringPool ++= bytes
        nextConstantStringIndex += 1
        nextConstatnStringOffset += bytes.length
        data
    }
  }

  def getConstantStringInstr(str: String): List[wa.Instr] =
    getConstantStringDataInstr(str) :+ wa.Call(genFunctionID.stringLiteral)

  def getConstantStringDataInstr(str: String): List[wa.I32Const] = {
    val data = addConstantStringGlobal(str)
    List(
      wa.I32Const(data.offset),
      // Assuming that the stringLiteral method will instantiate the
      // constant string from the data section using "array.newData $i16Array ..."
      // The length of the array should be equal to the length of the WTF-16 encoded string
      wa.I32Const(str.length()),
      wa.I32Const(data.constantStringIndex)
    )
  }

  def getClosureDataStructType(captureParamTypes: List[Type]): wanme.TypeID = {
    closureDataTypes.getOrElseUpdate(
      captureParamTypes, {
        val fields: List[watpe.StructField] =
          for ((tpe, i) <- captureParamTypes.zipWithIndex)
            yield watpe.StructField(
              genFieldID.captureParam(i),
              NoOriginalName,
              TypeTransformer.transformType(tpe)(this),
              isMutable = false
            )
        val structTypeName = genTypeID.captureData(nextClosureDataTypeIndex)
        nextClosureDataTypeIndex += 1
        val structType = watpe.StructType(fields)
        moduleBuilder.addRecType(structTypeName, NoOriginalName, structType)
        structTypeName
      }
    )
  }

  def refFuncWithDeclaration(name: wanme.FunctionID): wa.RefFunc = {
    addFuncDeclaration(name)
    wa.RefFunc(name)
  }

  def assignBuckets(classes: List[LinkedClass]): Unit =
    _itablesLength = assignBuckets0(classes.filterNot(_.kind.isJSType))

  def addExport(exprt: wamod.Export): Unit =
    moduleBuilder.addExport(exprt)

  def addFunction(fun: wamod.Function): Unit =
    moduleBuilder.addFunction(fun)

  def addGlobal(g: wamod.Global): Unit =
    moduleBuilder.addGlobal(g)

  def addGlobalITable(name: ClassName, g: wamod.Global): Unit = {
    classItableGlobals += name
    addGlobal(g)
  }

  def getAllClassesWithITableGlobal(): List[ClassName] =
    classItableGlobals.toList

  def getImportedModuleGlobal(moduleName: String): wanme.GlobalID = {
    val name = genGlobalID.forImportedModule(moduleName)
    if (_importedModules.add(moduleName)) {
      val origName = OriginalName("import." + moduleName)
      moduleBuilder.addImport(
        wamod.Import(
          "__scalaJSImports",
          moduleName,
          wamod.ImportDesc.Global(name, origName, watpe.RefType.anyref, isMutable = false)
        )
      )
    }
    name
  }

  def allImportedModules: List[String] = _importedModules.toList

  def addFuncDeclaration(name: wanme.FunctionID): Unit =
    _funcDeclarations += name

  def putClassInfo(name: ClassName, info: ClassInfo): Unit =
    classInfo.put(name, info)

  def addJSPrivateFieldName(fieldName: FieldName): Unit =
    _jsPrivateFieldNames += fieldName

  def getFinalStringPool(): (Array[Byte], Int) =
    (stringPool.toArray, nextConstantStringIndex)

  def getAllJSPrivateFieldNames(): List[FieldName] =
    _jsPrivateFieldNames.toList

  def getAllFuncDeclarations(): List[wanme.FunctionID] =
    _funcDeclarations.toList

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
  private def assignBuckets0(classes: List[LinkedClass]): Int = {
    var nextIdx = 0
    def newBucket(): Bucket = {
      val idx = nextIdx
      nextIdx += 1
      new Bucket(idx)
    }
    def getAllInterfaces(info: ClassInfo): List[ClassName] =
      info.ancestors.filter(getClassInfo(_).isInterface)

    val buckets = new mutable.ListBuffer[Bucket]()

    /** All join type descendants of the class */
    val joinsOf =
      new mutable.HashMap[ClassName, mutable.HashSet[ClassName]]()

    /** the buckets that have been assigned to any of the ancestors of the class */
    val usedOf = new mutable.HashMap[ClassName, mutable.HashSet[Bucket]]()
    val spines = new mutable.HashSet[ClassName]()

    for (clazz <- classes.reverseIterator) {
      val info = getClassInfo(clazz.name.name)
      val ifaces = getAllInterfaces(info)
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
      val info = getClassInfo(clazz.name.name)
      val ifaces = getAllInterfaces(info)
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
    buckets.length
  }
}

object WasmContext {
  final case class StringData(constantStringIndex: Int, offset: Int)

  final class ClassInfo(
      ctx: WasmContext,
      val name: ClassName,
      val kind: ClassKind,
      val jsClassCaptures: Option[List[ParamDef]],
      classConcretePublicMethodNames: List[MethodName],
      val allFieldDefs: List[FieldDef],
      val superClass: Option[ClassName],
      val interfaces: List[ClassName],
      val ancestors: List[ClassName],
      private var _hasInstances: Boolean,
      val isAbstract: Boolean,
      val hasRuntimeTypeInfo: Boolean,
      val jsNativeLoadSpec: Option[JSNativeLoadSpec],
      val jsNativeMembers: Map[MethodName, JSNativeLoadSpec],
      val staticFieldMirrors: Map[FieldName, List[String]],
      private var _itableIdx: Int
  ) {
    val resolvedMethodInfos: Map[MethodName, ConcreteMethodInfo] = {
      if (kind.isClass || kind == ClassKind.HijackedClass) {
        val inherited: Map[MethodName, ConcreteMethodInfo] = superClass match {
          case Some(superClass) => ctx.getClassInfo(superClass).resolvedMethodInfos
          case None             => Map.empty
        }

        for (methodName <- classConcretePublicMethodNames)
          inherited.get(methodName).foreach(_.markOverridden())

        classConcretePublicMethodNames.foldLeft(inherited) { (prev, methodName) =>
          prev.updated(methodName, new ConcreteMethodInfo(name, methodName))
        }
      } else {
        Map.empty
      }
    }

    private val methodsCalledDynamically = mutable.HashSet.empty[MethodName]

    /** For a class or interface, its table entries in definition order. */
    private var _tableEntries: List[MethodName] = null

    // See caller in Preprocessor.preprocess
    def setHasInstances(): Unit =
      _hasInstances = true

    def hasInstances: Boolean = _hasInstances

    def setItableIdx(idx: Int): Unit = _itableIdx = idx

    /** Returns the index of this interface's itable in the classes' interface tables.
      */
    def itableIdx: Int = _itableIdx

    private var _specialInstanceTypes: Int = 0

    def addSpecialInstanceType(jsValueType: Int): Unit =
      _specialInstanceTypes |= (1 << jsValueType)

    /** A bitset of the `jsValueType`s corresponding to hijacked classes that extend this class.
      *
      * This value is used for instance tests against this class. A JS value `x` is an instance of
      * this type iff `jsValueType(x)` is a member of this bitset. Because of how a bitset works,
      * this means testing the following formula:
      *
      * {{{
      * ((1 << jsValueType(x)) & specialInstanceTypes) != 0
      * }}}
      *
      * For example, if this class is `Comparable`, we want the bitset to contain the values for
      * `boolean`, `string` and `number` (but not `undefined`), because `jl.Boolean`, `jl.String`
      * and `jl.Double` implement `Comparable`.
      *
      * This field is initialized with 0, and augmented during preprocessing by calls to
      * `addSpecialInstanceType`.
      *
      * This technique is used both for static `isInstanceOf` tests as well as reflective tests
      * through `Class.isInstance`. For the latter, this value is stored in
      * `typeData.specialInstanceTypes`. For the former, it is embedded as a constant in the
      * generated code.
      *
      * See the `isInstance` and `genInstanceTest` helpers.
      *
      * Special cases: this value remains 0 for all the numeric hijacked classes except `jl.Double`,
      * since `jsValueType(x) == JSValueTypeNumber` is not enough to deduce that
      * `x.isInstanceOf[Int]`, for example.
      */
    def specialInstanceTypes: Int = _specialInstanceTypes

    /** Is this class an ancestor of any hijacked class?
      *
      * This includes but is not limited to the hijacked classes themselves, as well as `jl.Object`.
      */
    def isAncestorOfHijackedClass: Boolean =
      specialInstanceTypes != 0 || kind == ClassKind.HijackedClass

    def isInterface = kind == ClassKind.Interface

    def registerDynamicCall(methodName: MethodName): Unit =
      methodsCalledDynamically += methodName

    def buildMethodTable(): Unit = {
      if (_tableEntries != null)
        throw new IllegalStateException(s"Duplicate call to buildMethodTable() for $name")

      kind match {
        case ClassKind.Class | ClassKind.ModuleClass | ClassKind.HijackedClass =>
          val superTableEntries =
            superClass.fold[List[MethodName]](Nil)(sup => ctx.getClassInfo(sup).tableEntries)
          val superTableEntrySet = superTableEntries.toSet

          /* When computing the table entries to add for this class, exclude:
           * - methods that are already in the super class' table entries, and
           * - methods that are effectively final, since they will always be
           *   statically resolved instead of using the table dispatch.
           */
          val newTableEntries = methodsCalledDynamically.toList
            .filter(!superTableEntrySet.contains(_))
            .filterNot(m => resolvedMethodInfos.get(m).exists(_.isEffectivelyFinal))
            .sorted // for stability

          _tableEntries = superTableEntries ::: newTableEntries

        case ClassKind.Interface =>
          _tableEntries = methodsCalledDynamically.toList.sorted // for stability

        case _ =>
          _tableEntries = Nil
      }

      methodsCalledDynamically.clear() // gc
    }

    def tableEntries: List[MethodName] = {
      if (_tableEntries == null)
        throw new IllegalStateException(s"Table not yet built for $name")
      _tableEntries
    }
  }

  final class ConcreteMethodInfo(
      val ownerClass: ClassName,
      val methodName: MethodName
  ) {
    val tableEntryName = genFunctionID.forTableEntry(ownerClass, methodName)

    private var effectivelyFinal: Boolean = true

    private[WasmContext] def markOverridden(): Unit =
      effectivelyFinal = false

    def isEffectivelyFinal: Boolean = effectivelyFinal
  }

  private[WasmContext] class Bucket(idx: Int) {
    def add(clazz: ClassInfo) = clazz.setItableIdx((idx))

    /** A set of join types that are descendants of the types assigned to that bucket */
    val joins = new mutable.HashSet[ClassName]()
  }
}
