package wasm
package ir2wasm

import wasm4s._
import wasm4s.WasmContext._
import wasm4s.Names._
import wasm4s.Types._
import wasm4s.WasmInstr._
import TypeTransformer._

import org.scalajs.ir.{Trees => IRTrees}
import org.scalajs.ir.{Types => IRTypes}
import org.scalajs.ir.{Names => IRNames}
import org.scalajs.ir.ClassKind

import org.scalajs.linker.standard.{LinkedClass, LinkedTopLevelExport}

import collection.mutable
import java.awt.Window.Type
import _root_.wasm4s.Defaults

class WasmBuilder {
  // val module = new WasmModule()

  def transformClassDef(clazz: LinkedClass)(implicit ctx: WasmContext) = {
    clazz.kind match {
      case ClassKind.ModuleClass => transformModuleClass(clazz)
      case ClassKind.Class       => transformClass(clazz)
      case ClassKind.Interface   => transformInterface(clazz)
      case _                     =>
    }
  }

  def transformTopLevelExport(export: LinkedTopLevelExport)(implicit ctx: WasmContext): Unit = {
    implicit val fctx = WasmFunctionContext()
    val expressionBuilder = new WasmExpressionBuilder(ctx, fctx)
    export.tree match {
      case d: IRTrees.TopLevelFieldExportDef   => ???
      case d: IRTrees.TopLevelJSClassExportDef => ???
      case d: IRTrees.TopLevelMethodExportDef  => transformToplevelMethodExportDef(d)
      case d: IRTrees.TopLevelModuleExportDef  => ???
    }
  }

  private def transformClassCommon(
      clazz: LinkedClass
  )(implicit ctx: WasmContext): WasmStructType = {
    val (vtableType, vtableName) = genVTable(clazz)
    val vtableField = WasmStructField(
      Names.WasmFieldName.vtable,
      WasmRefNullType(WasmHeapType.Type(vtableType.name)),
      isMutable = false
    )
    calculateClassITable(clazz) match {
      case None =>
        genStructNewDefault(clazz, vtableName, None)
      case Some(globalITable) =>
        ctx.addGlobal(globalITable)
        genStructNewDefault(clazz, vtableName, Some(globalITable))
    }

    // type definition
    val fields = clazz.fields.map(transformField)
    val structType = WasmStructType(
      Names.WasmTypeName.WasmStructTypeName(clazz.name.name),
      vtableField +: WasmStructField.itables +: fields,
      clazz.superClass.map(s => Names.WasmTypeName.WasmStructTypeName(s.name))
    )
    ctx.addGCType(structType)

    // Do not generate methods in Object for now
    if (clazz.name.name == IRNames.ObjectClass)
      clazz.methods.filter(_.name.name == IRNames.NoArgConstructorName).foreach { method =>
        genFunction(clazz, method)
      }
    else
      clazz.methods.foreach { method =>
        genFunction(clazz, method)
      }

    structType
  }

  private def genLoadModuleFunc(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
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
      CALL(FuncIdx(WasmFunctionName.newDefault(clazz.name.name))),
      GLOBAL_SET(GlobalIdx(globalInstanceName)),
      GLOBAL_GET(GlobalIdx(globalInstanceName)),
      CALL(FuncIdx(ctorName)),
      // ELSE,
      END,
      GLOBAL_GET(GlobalIdx(globalInstanceName)) // [rt]
    )

    val sig =
      WasmFunctionSignature(Nil, List(WasmRefNullType(WasmHeapType.Type(typeName))))
    val loadModuleTypeName = ctx.addFunctionType(sig)
    val func = WasmFunction(
      WasmFunctionName.loadModule(clazz.name.name),
      WasmFunctionType(loadModuleTypeName, sig),
      Nil,
      WasmExpr(body)
    )
    ctx.addFunction(func)
  }

  private def genStructNewDefault(
      clazz: LinkedClass,
      vtable: WasmGlobalName.WasmGlobalVTableName,
      itable: Option[WasmGlobal]
  )(implicit ctx: WasmContext): Unit = {
    val getVTable = GLOBAL_GET(WasmImmediate.GlobalIdx(vtable))
    val getITable = itable match {
      case None => REF_NULL(WasmImmediate.HeapType(WasmHeapType.Type(WasmArrayType.itables.name)))
      case Some(i) => GLOBAL_GET(WasmImmediate.GlobalIdx(i.name))
    }
    val defaultFields =
      getVTable +: getITable +:
        clazz.fields.collect { case f: IRTrees.FieldDef =>
          val ty = transformType(f.ftpe)
          Defaults.defaultValue(ty)
        }

    val className = WasmTypeName.WasmStructTypeName(clazz.name.name)
    val body =
      defaultFields :+ STRUCT_NEW(WasmImmediate.TypeIdx(className))
    val sig =
      WasmFunctionSignature(Nil, List(WasmRefType(WasmHeapType.Type(className))))
    val newDefaultTypeName = ctx.addFunctionType(sig)
    val func = WasmFunction(
      WasmFunctionName.newDefault(clazz.name.name),
      WasmFunctionType(newDefaultTypeName, sig),
      Nil,
      WasmExpr(body)
    )
    ctx.addFunction(func)
  }

  /** @return
    *   global instance of the class itable
    */
  private def calculateClassITable(
      clazz: LinkedClass
  )(implicit ctx: ReadOnlyWasmContext): Option[WasmGlobal] = {
    val classItables = ctx.calculateClassItables(clazz.name.name)
    if (!classItables.isEmpty) {
      // val classITableTypeName = WasmTypeName.WasmITableTypeName(clazz.name.name)
      // val classITableType = WasmStructType(
      //   classITableTypeName,
      //   interfaceInfos.map { info =>
      //     val itableTypeName = WasmTypeName.WasmITableTypeName(info.name)
      //     WasmStructField(
      //       Names.WasmFieldName(itableTypeName),
      //       WasmRefType(WasmHeapType.Type(itableTypeName)),
      //       isMutable = false
      //     )
      //   },
      //   None
      // )

      val vtable = ctx.calculateVtable(clazz.name.name)

      val itablesInit: List[WasmInstr] = classItables.itables.flatMap { iface =>
        iface.methods.map { method =>
          val func = vtable.resolve(method.name)
          REF_FUNC(WasmImmediate.FuncIdx(func.name))
        } :+ STRUCT_NEW(WasmTypeName.WasmITableTypeName(iface.name))
      } ++ List(
        ARRAY_NEW_FIXED(
          WasmImmediate.TypeIdx(WasmArrayType.itables.name),
          WasmImmediate.I32(classItables.itables.size)
        )
      )

      val globalITable = WasmGlobal(
        WasmGlobalName.WasmGlobalITableName(clazz.name.name),
        WasmRefType(WasmHeapType.Type(WasmArrayType.itables.name)),
        init = WasmExpr(itablesInit),
        isMutable = false
      )
      Some(globalITable)
    } else None
  }

  private def genVTable(
      clazz: LinkedClass
  )(implicit ctx: WasmContext): (WasmStructType, WasmGlobalName.WasmGlobalVTableName) = {
    val className = clazz.name.name
    def genVTableType(vtable: WasmVTable): WasmStructType = {
      val vtableFields =
        vtable.functions.map { method =>
          WasmStructField(
            Names.WasmFieldName(method.name),
            WasmRefNullType(WasmHeapType.Func(method.toWasmFunctionType().name)),
            isMutable = false
          )
        }
      WasmStructType(
        Names.WasmTypeName.WasmVTableTypeName.fromIR(clazz.name.name),
        vtableFields,
        clazz.superClass.map(s => Names.WasmTypeName.WasmVTableTypeName.fromIR(s.name))
      )
    }

    val vtableName = Names.WasmGlobalName.WasmGlobalVTableName(clazz.name.name)

    val vtable = ctx.calculateVtable(className)
    val vtableType = genVTableType(vtable)
    ctx.addGCType(vtableType)

    val globalVTable =
      WasmGlobal(
        vtableName,
        WasmRefNullType(WasmHeapType.Type(vtableType.name)),
        WasmExpr(vtable.toVTableEntries(vtableType.name)),
        isMutable = false
      )
    ctx.addGlobal(globalVTable)

    (vtableType, vtableName)
  }

  private def transformClass(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    assert(clazz.kind == ClassKind.Class)
    transformClassCommon(clazz)
  }

  private def transformInterface(clazz: LinkedClass)(implicit ctx: WasmContext): Unit = {
    assert(clazz.kind == ClassKind.Interface)
    // gen itable type
    val className = clazz.name.name
    // val typeName = WasmTypeName.WasmITableTypeName(className)
    val classInfo = ctx.getClassInfo(clazz.className)
    val itableType = WasmStructType(
      Names.WasmTypeName.WasmITableTypeName(className),
      classInfo.methods.map { m =>
        WasmStructField(
          Names.WasmFieldName(m.name.methodName),
          WasmRefNullType(WasmHeapType.Func(m.toWasmFunctionType().name)),
          isMutable = false
        )
      },
      None
    )
    ctx.addGCType(itableType)
    // typeName
    // genITable
    // generateVTable()

    // Do we need receivers?
    clazz.methods.collect {
      case method if method.body.isDefined =>
        genFunction(clazz, method)
    }
  }

  private def transformModuleClass(clazz: LinkedClass)(implicit ctx: WasmContext) = {
    assert(clazz.kind == ClassKind.ModuleClass)

    val structType = transformClassCommon(clazz)
    val heapType = WasmHeapType.Type(structType.name)

    // global instance
    // (global name (ref null type))
    val global = WasmGlobal(
      Names.WasmGlobalName.WasmModuleInstanceName.fromIR(clazz.name.name),
      WasmRefNullType(heapType),
      WasmExpr(List(REF_NULL(WasmImmediate.HeapType(heapType)))),
      isMutable = true
    )
    ctx.addGlobal(global)

    genLoadModuleFunc(clazz)
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
      newParams.map(arg => transformType(arg.ptpe)),
      transformResultType(resultType)
    )
    val typeName = ctx.addFunctionType(sig)
    val functionType = WasmFunctionType(typeName, sig)

    val params = newParams.map { arg =>
      WasmLocal(
        Names.WasmLocalName.fromIR(arg.name.name),
        transformType(arg.ptpe),
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
    ctx.addFunction(func)

    val export = new WasmExport.Function(
      methodName.value,
      func
    )
    ctx.addExport(export)
  }

  private def genFunction(
      clazz: LinkedClass,
      method: IRTrees.MethodDef
  )(implicit ctx: WasmContext): WasmFunction = {
    val receiver = WasmLocal(
      Names.WasmLocalName.receiver,
      // Receiver type for non-constructor methods needs to be Object type because params are invariant
      // Otherwise, vtable can't be a subtype of the supertype's subtype
      // Constructor can use the exact type because it won't be registered to vtables.
      if (method.flags.namespace.isConstructor)
        WasmRefNullType(WasmHeapType.Type(WasmTypeName.WasmStructTypeName(clazz.name.name)))
      else
        WasmRefNullType(WasmHeapType.ObjectType),
      isParameter = true
    )
    val paramTys = receiver.typ +:
      method.args.map(arg => transformType(arg.ptpe))
    val resultTy = transformResultType(method.resultType)
    val sig = WasmFunctionSignature(paramTys, resultTy)
    val typeName = ctx.addFunctionType(sig)
    val functionType = WasmFunctionType(typeName, sig)

    // Prepare for function context, set receiver and parameters
    implicit val fctx = WasmFunctionContext(receiver)
    (receiver +: method.args.map { arg =>
      WasmLocal(
        Names.WasmLocalName.fromIR(arg.name.name),
        transformType(arg.ptpe),
        isParameter = true
      )
    }).foreach(fctx.locals.define)

    // build function body
    val builder = new WasmExpressionBuilder(ctx, fctx)
    val body = method.body.getOrElse(throw new Exception("abstract method cannot be transformed"))
    // val prefix =
    //   if (method.flags.namespace.isConstructor) builder.objectCreationPrefix(clazz, method) else Nil
    val instrs = body match {
      case t: IRTrees.Block => t.stats.flatMap(builder.transformTree)
      case _                => builder.transformTree(body)
    }
    val expr = method.resultType match {
      case IRTypes.NoType => WasmExpr(instrs)
      case _              => WasmExpr(instrs :+ RETURN)
    }

    val func = WasmFunction(
      Names.WasmFunctionName(clazz.name.name, method.name.name),
      functionType,
      fctx.locals.all,
      expr
    )
    ctx.addFunction(func)
    func
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
      transformType(field.ftpe),
      // needs to be mutable even if it's flags.isMutable = false
      // because it's initialized by constructor
      isMutable = true // field.flags.isMutable
    )
  }
}
