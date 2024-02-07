package wasm.utils

import scala.language.implicitConversions

import org.scalajs.ir
import org.scalajs.ir.ClassKind
import org.scalajs.ir.Hashers
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.Version

import org.scalajs.linker.interface.ModuleInitializer

/** Utilities to concisely build IR snippets for tests.
  *
  * Adapted from Scala.js upstream.
  */
object TestIRBuilder {
  implicit val noPosition: ir.Position = ir.Position.NoPosition

  val EAF = ApplyFlags.empty
  val EMF = MemberFlags.empty
  val EOH = OptimizerHints.empty
  val NON = NoOriginalName
  val NOV = Version.Unversioned

  val JSCtorFlags = EMF.withNamespace(MemberNamespace.Public)

  val V = VoidRef
  val I = IntRef
  val Z = BooleanRef
  val O = ClassRef(ObjectClass)
  val T = ClassRef(BoxedStringClass)
  val AT = ArrayTypeRef(ClassRef(BoxedStringClass), 1)

  def m(name: String, paramTypeRefs: List[TypeRef], resultTypeRef: TypeRef): MethodName =
    MethodName(name, paramTypeRefs, resultTypeRef)

  def classDef(
      className: ClassName,
      kind: ClassKind = ClassKind.Class,
      jsClassCaptures: Option[List[ParamDef]] = None,
      superClass: Option[ClassName] = None,
      interfaces: List[ClassName] = Nil,
      jsSuperClass: Option[Tree] = None,
      jsNativeLoadSpec: Option[JSNativeLoadSpec] = None,
      fields: List[AnyFieldDef] = Nil,
      methods: List[MethodDef] = Nil,
      jsConstructor: Option[JSConstructorDef] = None,
      jsMethodProps: List[JSMethodPropDef] = Nil,
      jsNativeMembers: List[JSNativeMemberDef] = Nil,
      topLevelExportDefs: List[TopLevelExportDef] = Nil,
      optimizerHints: OptimizerHints = EOH
  ): ClassDef = {
    val notHashed = ClassDef(
      ClassIdent(className),
      NON,
      kind,
      jsClassCaptures,
      superClass.map(ClassIdent(_)),
      interfaces.map(ClassIdent(_)),
      jsSuperClass,
      jsNativeLoadSpec,
      fields,
      methods,
      jsConstructor,
      jsMethodProps,
      jsNativeMembers,
      topLevelExportDefs
    )(optimizerHints)
    Hashers.hashClassDef(notHashed)
  }

  final val MainTestClassName = ClassName("Test")

  val MainTestModuleInitializers = mainModuleInitializers("Test")

  def mainTestClassDef(mainBody: Tree): ClassDef = {
    classDef(
      MainTestClassName,
      kind = ClassKind.ModuleClass,
      superClass = Some(ObjectClass),
      methods = List(
        // trivialCtor(MainTestClassName),
        mainMethodDef(mainBody)
      )
    )
  }

  def trivialCtor(enclosingClassName: ClassName): MethodDef = {
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

  def trivialJSCtor: JSMethodDef = {
    JSMethodDef(
      JSCtorFlags,
      str("constructor"),
      Nil,
      None,
      Block(JSSuperConstructorCall(Nil) :: Undefined() :: Nil)
    )(EOH, NOV)
  }

  val MainMethodName: MethodName = m("main", List(I), I)

  def mainMethodDef(body: Tree): MethodDef = {
    val argsParamDef = paramDef("args", IntType)
    MethodDef(
      MemberFlags.empty.withNamespace(MemberNamespace.PublicStatic),
      MainMethodName,
      NON,
      List(argsParamDef),
      IntType,
      Some(body)
    )(EOH, NOV)
  }

  def methodDef(
      name: String,
      paramDefs: List[ParamDef],
      paramTypeRefs: List[TypeRef],
      resultTypeRef: TypeRef,
      resultType: Type,
      body: Tree
  ): MethodDef = {
    val methodName = m(name, paramTypeRefs, resultTypeRef)
    MethodDef(
      flags = MemberFlags.empty.withNamespace(MemberNamespace.PublicStatic),
      name = MainMethodName,
      originalName = NON,
      args = paramDefs,
      resultType = resultType,
      body = Some(body)
    )(EOH, NOV)
  }

  def consoleLog(expr: Tree): Tree =
    JSMethodApply(JSGlobalRef("console"), str("log"), List(expr))

  def paramDef(name: LocalName, ptpe: Type): ParamDef =
    ParamDef(LocalIdent(name), NON, ptpe, mutable = false)

  def mainModuleInitializers(moduleClassName: String): List[ModuleInitializer] =
    ModuleInitializer.mainMethodWithArgs(moduleClassName, "main") :: Nil

  val JSObjectLikeClass = ClassName("JSObject")

  val JSObjectLikeClassDef: ClassDef = {
    classDef(
      JSObjectLikeClass,
      kind = ClassKind.NativeJSClass,
      superClass = Some(ObjectClass),
      jsNativeLoadSpec = Some(JSNativeLoadSpec.Global("Object", Nil))
    )
  }

  // def requiredMemberDefs(className: ClassName, classKind: ClassKind): List[MemberDef] = {
  //   if (classKind == ClassKind.ModuleClass) List(trivialCtor(className))
  //   else if (classKind.isJSClass) List(trivialJSCtor)
  //   else Nil
  // }

  implicit def string2LocalName(name: String): LocalName =
    LocalName(name)
  implicit def string2LabelName(name: String): LabelName =
    LabelName(name)
  implicit def string2FieldName(name: String): FieldName =
    FieldName(name)
  implicit def string2ClassName(name: String): ClassName =
    ClassName(name)

  implicit def string2LocalIdent(name: String): LocalIdent =
    LocalIdent(LocalName(name))
  implicit def string2LabelIdent(name: String): LabelIdent =
    LabelIdent(LabelName(name))
  implicit def string2FieldIdent(name: String): FieldIdent =
    FieldIdent(FieldName(name))
  implicit def string2ClassIdent(name: String): ClassIdent =
    ClassIdent(ClassName(name))

  implicit def localName2LocalIdent(name: LocalName): LocalIdent =
    LocalIdent(name)
  implicit def methodName2MethodIdent(name: MethodName): MethodIdent =
    MethodIdent(name)

  def int(x: Int): IntLiteral = IntLiteral(x)
  def str(x: String): StringLiteral = StringLiteral(x)
}
