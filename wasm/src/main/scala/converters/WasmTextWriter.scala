package wasm
package converters

import wasm.wasm4s._
import wasm.wasm4s.Names._
import wasm.wasm4s.Types._
import wasm.wasm4s.WasmInstr._

class WasmTextWriter {
  import WasmTextWriter._

  def write(module: WasmModule): String = {
    implicit val b = new WatBuilder()
    writeModule(module)
    b.toString()
  }

  private def writeModule(module: WasmModule)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "module", {
        b.newLineList(
          "rec", {
            module.recGroupTypes.foreach(writeGCTypeDefinition)
            module.functionTypes.foreach(writeFunctionType)
            module.arrayTypes.foreach(writeGCTypeDefinition)
          }
        )
        module.imports.foreach(writeImport)
        module.definedFunctions.foreach(writeFunction)
        module.globals.foreach(writeGlobal)
        module.exports.foreach(writeExport)
        module.startFunction.foreach(writeStart)
        module.elements.foreach(writeElement)
      }
    )
    // context.gcTypes
  }

  private def writeGCTypeDefinition(
      t: WasmGCTypeDefinition
  )(implicit b: WatBuilder): Unit = {
    def writeField(field: WasmStructField): Unit = {
      b.sameLineList(
        "field", {
          b.appendElement(field.name.show)
          if (field.isMutable)
            b.sameLineList(
              "mut", {
                b.appendElement(field.typ.show)
              }
            )
          else b.appendElement(field.typ.show)
        }
      )
    }

    b.newLineList(
      "type", {
        b.appendElement(t.name.show)
        t match {
          case WasmArrayType(name, field) =>
            b.sameLineList(
              "array", {
                if (field.isMutable)
                  b.sameLineList(
                    "mut", {
                      b.appendElement(field.typ.show)
                    }
                  )
                else b.appendElement(field.typ.show)

              }
            )
          // (type $kotlin.Any___type_13 (struct (field (ref $kotlin.Any.vtable___type_12)) (field (ref null struct)) (field (mut i32)) (field (mut i32))))
          case WasmStructType(name, fields, superType) =>
            b.sameLineList(
              "sub", {
                superType.foreach(s => b.appendElement(s.show))
                b.sameLineList(
                  "struct", {
                    fields.foreach(writeField)
                  }
                )
              }
            )
        }
      }
    )
  }

  private def writeFunctionType(
      functionType: WasmFunctionType
  )(implicit b: WatBuilder): Unit =
    // (type type-name (func (param ty) (param ty) (result ty)))
    b.newLineList(
      "type", {
        b.appendElement(functionType.name.show)
        b.sameLineList(
          "func", {
            functionType.params.foreach { ty =>
              b.sameLineListOne("param", ty.show)
            }
            if (functionType.results.nonEmpty)
              functionType.results.foreach { ty =>
                b.sameLineListOne("result", ty.show)
              }
          }
        )
      }
    )

  private def writeImport(i: WasmImport)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "import", {
        b.appendElement(s"\"${i.module}\"")
        b.appendElement(s"\"${i.name}\"")

        i.desc match {
          case WasmImportDesc.Func(id, typ) =>
            b.sameLineList(
              "func", {
                b.appendElement(id.show)
                writeSig(typ.params, typ.results)
              }
            )
        }
      }
    )
  }

  private def writeSig(params: List[WasmType], results: List[WasmType])(implicit
      b: WatBuilder
  ): Unit = {
    params.foreach(typ => b.sameLineListOne("param", typ.show))
    results.foreach(typ => b.sameLineListOne("result", typ.show))
  }

  private def writeFunction(f: WasmFunction)(implicit b: WatBuilder): Unit = {
    def writeParam(l: WasmLocal)(implicit b: WatBuilder): Unit = {
      b.sameLineList(
        "param", {
          b.appendElement(l.name.show)
          b.appendElement(l.typ.show)
        }
      )
    }

    def writeLocal(l: WasmLocal)(implicit b: WatBuilder): Unit = {
      b.sameLineList(
        "local", {
          b.appendElement(l.name.show)
          b.appendElement(l.typ.show)
        }
      )
    }

    b.newLineList(
      "func", {
        val (params, nonParams) = f.locals.partition(_.isParameter)
        b.appendElement(f.name.show)
        b.sameLineListOne("type", f.typ.name.show)

        b.newLine()
        params.foreach(writeParam)
        f.typ.results.foreach(r => { b.sameLineListOne("result", r.show) })

        b.newLine()
        if (nonParams.nonEmpty) {
          nonParams.foreach(writeLocal)
        }
        f.body.instr.foreach(writeInstr)
      }
    )
  }

  private def writeGlobal(g: WasmGlobal)(implicit b: WatBuilder) =
    b.newLineList(
      "global", {
        b.appendElement(g.name.show)
        if (g.isMutable)
          b.sameLineList("mut", { b.appendElement(g.typ.show) })
        else b.appendElement(g.typ.show)
        g.init.instr.foreach(writeInstr)
      }
    )

  private def writeExport(e: WasmExport[_])(implicit
      b: WatBuilder
  ) = b.newLineList(
    "export", {
      b.appendElement(s"\"${e.name}\"")
      e match {
        case fun: WasmExport.Function =>
          b.sameLineList(
            "func",
            { b.appendElement(fun.field.name.show) }
          )
        case _: WasmExport.Global => ???
      }
    }
  )

  private def writeStart(startFunction: WasmFunctionName)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "start", {
        b.appendElement(startFunction.show)
      }
    )
  }

  private def writeElement(element: WasmElement)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "elem", {
        element.mode match {
          case WasmElement.Mode.Passive     => ()
          case WasmElement.Mode.Declarative => b.appendElement("declare")
        }
        b.appendElement(element.typ.show)
        element.init.foreach { item =>
          b.newLineList(
            "item",
            item.instr.foreach(writeInstr(_))
          )
        }
      }
    )
  }

  private def writeImmediate(i: WasmImmediate, instr: WasmInstr)(implicit b: WatBuilder): Unit = {
    def floatString(v: Double): String = {
      if (v.isNaN()) "nan"
      else if (v == Double.PositiveInfinity) "inf"
      else if (v == Double.NegativeInfinity) "-inf"
      else if (v.equals(-0.0)) "-0.0"
      else v.toString()
    }

    val str = i match {
      case WasmImmediate.I64(v)          => v.toString()
      case WasmImmediate.I32(v)          => v.toString()
      case WasmImmediate.F64(v)          => floatString(v)
      case WasmImmediate.F32(v)          => floatString(v.toDouble)
      case WasmImmediate.LocalIdx(name)  => name.show
      case WasmImmediate.GlobalIdx(name) => name.show
      case WasmImmediate.HeapType(ht) =>
        instr match {
          case _: REF_CAST      => s"(ref ${ht.show})"
          case _: REF_CAST_NULL => s"(ref null ${ht.show})"
          case _: REF_TEST      => s"(ref ${ht.show})"
          case _: REF_TEST_NULL => s"(ref null ${ht.show})"
          case _                => ht.show
        }
      case WasmImmediate.FuncIdx(name)                => name.show
      case WasmImmediate.TypeIdx(name)                => name.show
      case WasmImmediate.StructFieldIdx(v)            => v.toString
      case WasmImmediate.BlockType.FunctionType(name) => name.show
      case WasmImmediate.BlockType.ValueType(optTy) =>
        optTy.fold("") { ty => s"(result ${ty.show})" }
      case WasmImmediate.LabelIdx(i) => s"$$${i.toString}" // `loop 0` seems to be invalid
      case WasmImmediate.LabelIdxVector(indices) =>
        indices.map(i => "$" + i.value).mkString(" ")
      case i: WasmImmediate.CastFlags =>
        throw new UnsupportedOperationException(
          s"CastFlags $i must be handled directly in the instruction $instr"
        )
      case _ =>
        println(i)
        ???
    }
    b.appendElement(str)
  }

  private def writeRefTypeImmediate(i: WasmImmediate.HeapType, nullable: Boolean)(implicit
      b: WatBuilder
  ): Unit = {
    if (nullable)
      b.appendElement(s"(ref null ${i.value.show})")
    else
      b.appendElement(s"(ref ${i.value.show})")
  }

  private def writeInstr(instr: WasmInstr)(implicit b: WatBuilder): Unit = {
    instr match {
      case END | ELSE | _: CATCH => b.deindent()
      case _                     => ()
    }
    b.newLine()
    b.appendElement(instr.mnemonic)
    instr match {
      case instr: StructuredLabeledInstr =>
        instr.label.foreach(writeImmediate(_, instr))
      case _ =>
        ()
    }

    def writeBrOnCastImmediates(
        castFlags: WasmImmediate.CastFlags,
        label: WasmImmediate.LabelIdx,
        from: WasmImmediate.HeapType,
        to: WasmImmediate.HeapType
    ): Unit = {
      writeImmediate(label, instr)
      writeRefTypeImmediate(from, castFlags.nullable1)
      writeRefTypeImmediate(to, castFlags.nullable2)
    }

    instr match {
      case BR_ON_CAST(castFlags, label, from, to) =>
        writeBrOnCastImmediates(castFlags, label, from, to)
      case BR_ON_CAST_FAIL(castFlags, label, from, to) =>
        writeBrOnCastImmediates(castFlags, label, from, to)
      case _ =>
        instr.immediates.foreach { i => writeImmediate(i, instr) }
    }

    instr match {
      case _: BLOCK | _: LOOP | _: IF | ELSE | _: CATCH | _: TRY => b.indent()
      case _                                                     => ()
    }
  }

}

object WasmTextWriter {
  private class WatBuilder {
    private val builder = new StringBuilder
    private var level = 0
    private val indentStr = "  "

    private def indented(body: => Unit): Unit = {
      level += 1
      body
      level -= 1
    }

    def indent(): Unit = level += 1
    def deindent(): Unit = level -= 1

    def newLine(): Unit = {
      builder.append("\n")
      builder.append(indentStr * level)
    }

    def newLineList(name: String, body: => Unit): Unit = {
      newLine()
      builder.append(s"($name")
      indented(body)
      builder.append(")")
    }

    def sameLineList(name: String, body: => Unit): Unit = {
      builder.append(s" ($name")
      body
      builder.append(")")
    }

    def sameLineListOne(name: String, value: String) =
      sameLineList(name, { appendElement(value) })

    def appendElement(value: String): Unit = {
      builder.append(" ")
      builder.append(value)
    }

    override def toString: String =
      builder.toString()
  }

}
