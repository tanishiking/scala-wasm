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
        module.tags.foreach(writeTag)
        module.globals.foreach(writeGlobal)
        module.exports.foreach(writeExport)
        module.startFunction.foreach(writeStart)
        module.elements.foreach(writeElement)
        module.data.foreach(writeData)
      }
    )
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
                writeType(field.typ)
              }
            )
          else writeType(field.typ)
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
                      writeType(field.typ)
                    }
                  )
                else writeType(field.typ)

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
              b.sameLineList("param", writeType(ty))
            }
            if (functionType.results.nonEmpty)
              functionType.results.foreach { ty =>
                b.sameLineList("result", writeType(ty))
              }
          }
        )
      }
    )

  private def writeImport(i: WasmImport)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "import", {
        b.appendElement("\"" + i.module + "\"")
        b.appendElement("\"" + i.name + "\"")

        i.desc match {
          case WasmImportDesc.Func(id, typ) =>
            b.sameLineList(
              "func", {
                b.appendElement(id.show)
                writeSig(typ.params, typ.results)
              }
            )
          case WasmImportDesc.Global(id, typ, isMutable) =>
            b.sameLineList(
              "global", {
                b.appendElement(id.show)
                if (isMutable)
                  b.sameLineList("mut", writeType(typ))
                else
                  writeType(typ)
              }
            )
          case WasmImportDesc.Tag(id, typ) =>
            b.sameLineList(
              "tag", {
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
    params.foreach(typ => b.sameLineList("param", writeType(typ)))
    results.foreach(typ => b.sameLineList("result", writeType(typ)))
  }

  private def writeFunction(f: WasmFunction)(implicit b: WatBuilder): Unit = {
    def writeParam(l: WasmLocal)(implicit b: WatBuilder): Unit = {
      b.sameLineList(
        "param", {
          b.appendElement(l.name.show)
          writeType(l.typ)
        }
      )
    }

    def writeLocal(l: WasmLocal)(implicit b: WatBuilder): Unit = {
      b.sameLineList(
        "local", {
          b.appendElement(l.name.show)
          writeType(l.typ)
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
        f.typ.results.foreach(r => { b.sameLineList("result", writeType(r)) })

        b.newLine()
        if (nonParams.nonEmpty) {
          nonParams.foreach(writeLocal)
        }
        f.body.instr.foreach(writeInstr)
      }
    )
  }

  private def writeTag(tag: WasmTag)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "tag", {
        b.appendElement(tag.name.show)
        b.sameLineListOne("type", tag.typ.show)
      }
    )
  }

  private def writeGlobal(g: WasmGlobal)(implicit b: WatBuilder) =
    b.newLineList(
      "global", {
        b.appendElement(g.name.show)
        if (g.isMutable)
          b.sameLineList("mut", writeType(g.typ))
        else writeType(g.typ)
        g.init.instr.foreach(writeInstr)
      }
    )

  private def writeExport(e: WasmExport)(implicit
      b: WatBuilder
  ) = b.newLineList(
    "export", {
      b.appendElement("\"" + e.exportName + "\"")
      e match {
        case WasmExport.Function(_, funcName) =>
          b.sameLineList(
            "func",
            { b.appendElement(funcName.show) }
          )
        case WasmExport.Global(_, globalName) =>
          b.sameLineList(
            "global",
            { b.appendElement(globalName.show) }
          )
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
        writeType(element.typ)
        element.init.foreach { item =>
          b.newLineList(
            "item",
            item.instr.foreach(writeInstr(_))
          )
        }
      }
    )
  }

  private def writeData(data: WasmData)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "data", {
        b.appendElement(data.name.show)
        data.mode match {
          case WasmData.Mode.Passive => ()
        }
        b.appendElement("\"" + data.bytes.map("\\%02x".format(_)).mkString + "\"")
      }
    )
  }

  private def writeType(typ: WasmStorageType)(implicit b: WatBuilder): Unit = {
    typ match {
      case typ: WasmSimpleType => b.appendElement(typ.textName)
      case typ: WasmPackedType => b.appendElement(typ.textName)

      case WasmRefType(true, heapType: WasmHeapType.AbsHeapType) =>
        b.appendElement(heapType.nullableRefTextName)

      case WasmRefType(nullable, heapType) =>
        b.sameLineList(
          "ref", {
            if (nullable)
              b.appendElement("null")
            writeHeapType(heapType)
          }
        )
    }
  }

  private def writeHeapType(heapType: WasmHeapType)(implicit b: WatBuilder): Unit = {
    heapType match {
      case WasmHeapType.Type(typeName)        => b.appendElement(typeName.show)
      case heapType: WasmHeapType.AbsHeapType => b.appendElement(heapType.textName)
    }
  }

  private def floatString(v: Double): String = {
    if (v.isNaN()) "nan"
    else if (v == Double.PositiveInfinity) "inf"
    else if (v == Double.NegativeInfinity) "-inf"
    else if (v.equals(-0.0)) "-0.0"
    else v.toString()
  }

  private def writeBlockType(blockType: BlockType)(implicit b: WatBuilder): Unit = {
    blockType match {
      case BlockType.FunctionType(name) =>
        b.appendElement(s"(type ${name.show})")
      case BlockType.ValueType(optTy) =>
        for (ty <- optTy)
          b.sameLineList("result", writeType(ty))
    }
  }

  private def writeLabelIdx(labelIdx: WasmLabelName)(implicit b: WatBuilder): Unit =
    b.appendElement(labelIdx.show)

  private def writeInstr(instr: WasmInstr)(implicit b: WatBuilder): Unit = {
    instr match {
      case END | ELSE | _: CATCH | CATCH_ALL => b.deindent()
      case _                                 => ()
    }
    b.newLine()
    b.appendElement(instr.mnemonic)
    instr match {
      case instr: StructuredLabeledInstr =>
        instr.label.foreach(writeLabelIdx(_))
      case _ =>
        ()
    }

    writeInstrImmediates(instr)

    instr match {
      case _: StructuredLabeledInstr | ELSE | _: CATCH | CATCH_ALL => b.indent()
      case _                                                       => ()
    }
  }

  private def writeInstrImmediates(instr: WasmInstr)(implicit b: WatBuilder): Unit = {
    instr match {
      // Convenience categories

      case instr: WasmSimpleInstr =>
        ()
      case instr: WasmBlockTypeLabeledInstr =>
        writeBlockType(instr.blockTypeArgument)
      case instr: WasmLabelInstr =>
        writeLabelIdx(instr.labelArgument)
      case instr: WasmFuncInstr =>
        b.appendElement(instr.funcArgument.show)
      case instr: WasmTypeInstr =>
        b.appendElement(instr.typeArgument.show)
      case instr: WasmTagInstr =>
        b.appendElement(instr.tagArgument.show)
      case instr: WasmLocalInstr =>
        b.appendElement(instr.localArgument.show)
      case instr: WasmGlobalInstr =>
        b.appendElement(instr.globalArgument.show)
      case instr: WasmHeapTypeInstr =>
        writeHeapType(instr.heapTypeArgument)
      case instr: WasmRefTypeInstr =>
        writeType(instr.refTypeArgument)
      case instr: WasmStructFieldInstr =>
        b.appendElement(instr.structTypeName.show)
        b.appendElement(instr.fieldIdx.value.toString())

      // Specific instructions with unique-ish shapes

      case I32_CONST(v) => b.appendElement(v.toString())
      case I64_CONST(v) => b.appendElement(v.toString())
      case F32_CONST(v) => b.appendElement(floatString(v.toDouble))
      case F64_CONST(v) => b.appendElement(floatString(v))

      case BR_TABLE(labelIdxVector, defaultLabelIdx) =>
        labelIdxVector.foreach(writeLabelIdx(_))
        writeLabelIdx(defaultLabelIdx)

      case TRY_TABLE(blockType, clauses, _) =>
        writeBlockType(blockType)
        for (clause <- clauses) {
          b.sameLineList(
            clause.mnemonic, {
              clause.tag.foreach(tag => b.appendElement(tag.show))
              writeLabelIdx(clause.label)
            }
          )
        }

      case ARRAY_NEW_DATA(typeIdx, dataIdx) =>
        b.appendElement(typeIdx.show)
        b.appendElement(dataIdx.show)

      case ARRAY_NEW_FIXED(typeIdx, length) =>
        b.appendElement(typeIdx.show)
        b.appendElement(Integer.toUnsignedString(length))

      case ARRAY_COPY(destType, srcType) =>
        b.appendElement(destType.show)
        b.appendElement(srcType.show)

      case BR_ON_CAST(labelIdx, from, to) =>
        writeLabelIdx(labelIdx)
        writeType(from)
        writeType(to)
      case BR_ON_CAST_FAIL(labelIdx, from, to) =>
        writeLabelIdx(labelIdx)
        writeType(from)
        writeType(to)
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
