package wasm
package ir2wasm

import wasm4s._
import wasm4s.WasmContext._
import wasm4s.Names._

import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.ClassKind

import collection.mutable

class WasmBuilder(module: WasmModule) {
  // val module = new WasmModule()

  def transformClassDef(clazz: IRTrees.ClassDef)(implicit ctx: WasmContext) = {
    clazz.kind match {
      case ClassKind.ModuleClass => transformModuleClass(clazz)
      case ClassKind.Class       => transformClass(clazz)
      case _                     =>
    }
  }

  private def transformClassCommon(
      clazz: IRTrees.ClassDef
  )(implicit ctx: WasmContext): WasmStructType = {
    val (vtableType, vtable) = generateVTable(clazz)
    val vtableField = WasmStructField(
      Names.WasmFieldName.vtable,
      Types.WasmRefType(Types.WasmHeapType.Type(vtableType.name)),
      isMutable = false
    )

    val superType = clazz.superClass.flatMap(s =>
      if (s.name.nameString == "java.lang.Object") None
      else
        Some(Names.WasmTypeName.WasmStructTypeName(s.name))
    )

    // type definition
    val fields = clazz.fields.map(transformField)
    val structType = WasmStructType(
      Names.WasmTypeName.WasmStructTypeName(clazz.name.name),
      vtableField +: fields,
      superType
    )
    ctx.gcTypes.define(structType)
    module.addRecGroupType(structType)

    val receiver = WasmLocal(
      Names.WasmLocalName.fromStr("<this>"),
      Types.WasmRefNullType(Types.WasmHeapType.Type(structType.name)),
      isParameter = true
    )

    val functions = clazz.methods.map { method =>
      val paramTys = receiver.typ +: method.args.map(arg => TypeTransformer.transformType(arg.ptpe))
      val resultTy = TypeTransformer.transformResultType(method.resultType)
      val sig = WasmFunctionSignature(paramTys, resultTy)
      val typeName = ctx.addFunctionType(sig)
      val functionType = WasmFunctionType(typeName, sig)

      implicit val fctx = WasmFunctionContext(receiver)
      (receiver +: method.args.map { arg =>
        WasmLocal(
          Names.WasmLocalName.fromIR(arg.name.name),
          TypeTransformer.transformType(arg.ptpe),
          isParameter = true
        )
      }).foreach(fctx.locals.define)

      val body = transformMethod(method)
      val func = WasmFunction(
        Names.WasmFunctionName(clazz.name.name, method.name.name),
        functionType,
        fctx.locals.all,
        WasmExpr(body)
      )
      ctx.functions.define(func)
      module.addFunction(func)
      func
    }
    structType
  }

  private def genLoadModuleFunc(clazz: IRTrees.ClassDef)(implicit ctx: WasmContext): Unit = {
    import WasmInstr._
    import WasmImmediate._
    assert(clazz.kind == ClassKind.ModuleClass)
    val ctor = clazz.methods
      .find(_.methodName.isConstructor)
      .getOrElse(throw new Error(s"Module class should have a constructor, ${clazz.name}"))
    val typeName = WasmTypeName.WasmStructTypeName(clazz.name.name)
    val globalInstanceName = WasmGlobalName.WasmModuleInstanceName.fromIR(clazz.name.name)
    val ctorName = WasmFunctionName(clazz.name.name, ctor.name.name)
    val body = List(
      // global.get $module_name
      // ref.if_null
      //   ref.null $module_type
      //   call $module_init ;; should set to global
      // end
      // global.get $module_name
      GLOBAL_GET(GlobalIdx(globalInstanceName)), // [rt]
      REF_IS_NULL, // [rt] -> [i32] (bool)
      IF(WasmImmediate.BlockType.ValueType(None)),
      GLOBAL_GET(
        GlobalIdx(globalInstanceName)
      ), // [rt] // REF_NULL(HeapType(Types.WasmHeapType.Type(tyName))),
      CALL(FuncIdx(ctorName)),
      // ELSE,
      END,
      GLOBAL_GET(GlobalIdx(globalInstanceName)) // [rt]
    )

    val sig =
      WasmFunctionSignature(Nil, List(Types.WasmRefNullType(Types.WasmHeapType.Type(typeName))))
    val loadModuleTypeName = ctx.addFunctionType(sig)
    val func = WasmFunction(
      WasmFunctionName.loadModule(clazz.name.name),
      WasmFunctionType(loadModuleTypeName, sig),
      Nil,
      WasmExpr(body)
    )
    println(func)
    ctx.functions.define(func)
    module.addFunction(func)
  }

  private def generateVTable(
      clazz: IRTrees.ClassDef
  )(implicit ctx: WasmContext): (WasmStructType, WasmGlobal) = {
    def genVTableType(vtable: WasmVTable): WasmStructType = {
      val vtableFields = vtable.map { method =>
        WasmStructField(
          Names.WasmFieldName.fromFunction(method.name),
          Types.WasmRefType(Types.WasmHeapType.Func(method.tpe.name)),
          isMutable = false
        )
      }
      WasmStructType(
        Names.WasmTypeName.WasmVTableTypeName.fromIR(clazz.name.name),
        vtableFields,
        clazz.superClass.flatMap(s =>
          if (s.name.nameString == "java.lang.Object") None
          else Some(Names.WasmTypeName.WasmVTableTypeName.fromIR(s.name))
        )
      )
    }

    def genGlobalVTable(vtable: WasmVTable, vtableType: WasmStructType): WasmGlobal = {
      // construct global vtable instance
      val init = vtable.map { method =>
        WasmInstr.REF_FUNC(method.name)
      } :+ WasmInstr.STRUCT_NEW(vtableType.name)
      WasmGlobal(
        Names.WasmGlobalName.WasmGlobalVTableName.fromIR(clazz.name.name),
        Types.WasmRefType(Types.WasmHeapType.Type(vtableType.name)),
        Some(WasmExpr(init))
      )
    }

    val vtable = ctx.getVtable(clazz.name.name)

    val vtableType = genVTableType(vtable)
    ctx.gcTypes.define(vtableType)
    module.addRecGroupType(vtableType)

    val globalVTable = genGlobalVTable(vtable, vtableType)
    ctx.globals.define(globalVTable)
    module.addGlobal(globalVTable)

    (vtableType, globalVTable)
  }

  private def transformClass(clazz: IRTrees.ClassDef)(implicit ctx: WasmContext): Unit = {
    assert(clazz.kind == ClassKind.Class)
    transformClassCommon(clazz)
  }

  private def transformModuleClass(clazz: IRTrees.ClassDef)(implicit ctx: WasmContext) = {
    assert(clazz.kind == ClassKind.ModuleClass)

    val structType = transformClassCommon(clazz)

    // global instance
    // (global name (ref null type))
    val global = WasmGlobal(
      Names.WasmGlobalName.WasmModuleInstanceName.fromIR(clazz.name.name),
      Types.WasmRefNullType(Types.WasmHeapType.Type(structType.name)),
      None
    )
    ctx.globals.define(global)
    module.addGlobal(global)

    genLoadModuleFunc(clazz)

    clazz.topLevelExportDefs.foreach { i =>
      implicit val fctx = WasmFunctionContext()
      val expressionBuilder = new WasmExpressionBuilder(ctx, fctx)
      i match {
        case d: IRTrees.TopLevelFieldExportDef   => ???
        case d: IRTrees.TopLevelJSClassExportDef => ???
        case d: IRTrees.TopLevelMethodExportDef  => transformToplevelMethodExportDef(d)
        case d: IRTrees.TopLevelModuleExportDef  => ???
      }
    }
  }

  private def transformToplevelMethodExportDef(
      exportDef: IRTrees.TopLevelMethodExportDef
  )(implicit ctx: WasmContext, fctx: WasmFunctionContext) = {
    val builder = new WasmExpressionBuilder(ctx, fctx)
    val method = exportDef.methodDef
    val methodName = method.name match {
      case lit: IRTrees.StringLiteral => lit
      case _                          => ???
    }

    // hack
    // export top[moduleID="main"] static def "foo"(arg: any): any = {
    //   val prep0: int = arg.asInstanceOf[int];
    //   mod:sample.Main$.foo;I;I(prep0)
    // }
    // ->
    // export top[moduleID="main"] static def "foo"(arg: int): int = {
    //   val prep0: int = arg;
    //   mod:sample.Main$.foo;I;I(arg)
    // }
    val paramTypeMap = mutable.Map[IRTrees.LocalIdent, IRTypes.Type]()
    val nameMap = mutable.Map[IRTrees.LocalIdent, IRTrees.LocalIdent]()
    val resultType: IRTypes.Type = method.body.tpe
    def collectMapping(t: IRTrees.Tree): Unit = {
      t match {
        case IRTrees.Block(stats) => stats.foreach(collectMapping)
        case IRTrees.VarDef(lhs, _, _, _, IRTrees.AsInstanceOf(IRTrees.VarRef(ident), tpe)) =>
          paramTypeMap.update(ident, tpe) // arg -> int
          nameMap.update(lhs, ident) // prep0 -> arg
        case _ =>
      }
    }
    def mutateTree(t: IRTrees.Tree): IRTrees.Tree = {
      t match {
        case b: IRTrees.Block => IRTrees.Block(b.stats.map(mutateTree))(b.pos)
        case vdef @ IRTrees.VarDef(_, _, _, _, IRTrees.AsInstanceOf(vref, tpe)) =>
          vdef.copy(rhs = vref)(vdef.pos)
        case app: IRTrees.Apply =>
          app.copy(args = app.args.map(a => mutateTree(a)))(app.tpe)(app.pos)
        case vref: IRTrees.VarRef =>
          val newName = nameMap.getOrElse(vref.ident, throw new Error("Invalid name"))
          vref.copy(ident = newName)(vref.tpe)(vref.pos)
        case t => t
      }
    }

    collectMapping(method.body)
    val newBody = mutateTree(method.body)
    val newParams = method.args.map { arg =>
      paramTypeMap.get(arg.name) match {
        case None         => arg
        case Some(newTpe) => arg.copy(ptpe = newTpe)(arg.pos)
      }
    }

    val sig = WasmFunctionSignature(
      newParams.map(arg => TypeTransformer.transformType(arg.ptpe)),
      TypeTransformer.transformResultType(resultType)
    )
    val typeName = ctx.addFunctionType(sig)
    val functionType = WasmFunctionType(typeName, sig)

    val params = newParams.map { arg =>
      WasmLocal(
        Names.WasmLocalName.fromIR(arg.name.name),
        TypeTransformer.transformType(arg.ptpe),
        isParameter = true
      )
    }
    params.foreach(fctx.locals.define)

    val instrs = newBody match {
      case t: IRTrees.Block => t.stats.flatMap(builder.transformTree)
      case _                => builder.transformTree(newBody)
    }
    val func = WasmFunction(
      Names.WasmFunctionName(methodName),
      functionType,
      fctx.locals.all,
      WasmExpr(instrs)
    )
    ctx.functions.define(func)
    module.addFunction(func)

    val export = new WasmExport.Function(
      methodName.value,
      func
    )
    module.addExport(export)
  }

  private def transformMethod(
      method: IRTrees.MethodDef
  )(implicit ctx: WasmContext, fctx: WasmFunctionContext): List[WasmInstr] = {
    val builder = new WasmExpressionBuilder(ctx, fctx)
    val body = method.body.getOrElse(throw new Exception("abstract method cannot be transformed"))
    val prefix =
      if (method.flags.namespace.isConstructor) builder.objectCreationPrefix(method) else Nil
    val instrs = body match {
      case t: IRTrees.Block => t.stats.flatMap(builder.transformTree)
      case _                => builder.transformTree(body)
    }
    method.resultType match {
      case IRTypes.NoType => prefix ++ instrs
      case _              => prefix ++ instrs :+ WasmInstr.RETURN
    }
  }

  private def transformField(
      field: IRTrees.AnyFieldDef
  )(implicit ctx: WasmContext): WasmStructField = {
    val fieldName =
      field match {
        case f: IRTrees.FieldDef =>
          Names.WasmFieldName(f.name.name)
        // TODO
        case js: IRTrees.JSFieldDef => ???
      }
    WasmStructField(
      fieldName,
      TypeTransformer.transformType(field.ftpe),
      field.flags.isMutable
    )
  }
}
