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
        module.types.foreach(writeRecType)
        module.imports.foreach(writeImport)
        module.funcs.foreach(writeFunction)
        module.tags.foreach(writeTag)
        module.globals.foreach(writeGlobal)
        module.exports.foreach(writeExport)
        module.start.foreach(writeStart)
        module.elems.foreach(writeElement)
        module.datas.foreach(writeData)
      }
    )
  }

  private def writeRecType(recType: WasmRecType)(implicit b: WatBuilder): Unit = {
    recType.subTypes match {
      case singleSubType :: Nil =>
        writeTypeDefinition(singleSubType)
      case subTypes =>
        b.newLineList(
          "rec", {
            subTypes.foreach(writeTypeDefinition)
          }
        )
    }
  }

  private def writeTypeDefinition(subType: WasmSubType)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "type", {
        b.appendName(subType.name)
        subType match {
          case WasmSubType(_, true, None, compositeType) =>
            writeCompositeType(compositeType)
          case _ =>
            b.sameLineList(
              "sub", {
                if (subType.isFinal)
                  b.appendElement("final")
                for (superType <- subType.superType)
                  b.appendName(superType)
                writeCompositeType(subType.compositeType)
              }
            )
        }
      }
    )
  }

  private def writeCompositeType(t: WasmCompositeType)(implicit b: WatBuilder): Unit = {
    def writeFieldType(fieldType: WasmFieldType): Unit = {
      if (fieldType.isMutable)
        b.sameLineList(
          "mut", {
            writeType(fieldType.typ)
          }
        )
      else writeType(fieldType.typ)
    }

    def writeField(field: WasmStructField): Unit = {
      b.sameLineList(
        "field", {
          b.appendName(field.name)
          writeFieldType(field.fieldType)
        }
      )
    }

    t match {
      case WasmFunctionType(params, results) =>
        b.sameLineList(
          "func", {
            params.foreach { ty =>
              b.sameLineList("param", writeType(ty))
            }
            results.foreach { ty =>
              b.sameLineList("result", writeType(ty))
            }
          }
        )

      case WasmArrayType(fieldType) =>
        b.sameLineList(
          "array", {
            writeFieldType(fieldType)
          }
        )

      case WasmStructType(fields) =>
        b.sameLineList(
          "struct", {
            fields.foreach(writeField)
          }
        )
    }
  }

  private def writeImport(i: WasmImport)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "import", {
        b.appendElement("\"" + i.module + "\"")
        b.appendElement("\"" + i.name + "\"")

        i.desc match {
          case WasmImportDesc.Func(id, typeName) =>
            b.sameLineList(
              "func", {
                b.appendName(id)
                writeTypeUse(typeName)
              }
            )
          case WasmImportDesc.Global(id, typ, isMutable) =>
            b.sameLineList(
              "global", {
                b.appendName(id)
                if (isMutable)
                  b.sameLineList("mut", writeType(typ))
                else
                  writeType(typ)
              }
            )
          case WasmImportDesc.Tag(id, typeName) =>
            b.sameLineList(
              "tag", {
                b.appendName(id)
                writeTypeUse(typeName)
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
          b.appendName(l.name)
          writeType(l.typ)
        }
      )
    }

    def writeLocal(l: WasmLocal)(implicit b: WatBuilder): Unit = {
      b.sameLineList(
        "local", {
          b.appendName(l.name)
          writeType(l.typ)
        }
      )
    }

    b.newLineList(
      "func", {
        val (params, nonParams) = f.locals.partition(_.isParameter)
        b.appendName(f.name)
        writeTypeUse(f.typeName)

        b.newLine()
        params.foreach(writeParam)
        f.results.foreach(r => { b.sameLineList("result", writeType(r)) })

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
        b.appendName(tag.name)
        writeTypeUse(tag.typ)
      }
    )
  }

  private def writeGlobal(g: WasmGlobal)(implicit b: WatBuilder) =
    b.newLineList(
      "global", {
        b.appendName(g.name)
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
            { b.appendName(funcName) }
          )
        case WasmExport.Global(_, globalName) =>
          b.sameLineList(
            "global",
            { b.appendName(globalName) }
          )
      }
    }
  )

  private def writeStart(startFunction: WasmFunctionName)(implicit b: WatBuilder): Unit = {
    b.newLineList(
      "start", {
        b.appendName(startFunction)
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
        b.appendName(data.name)
        data.mode match {
          case WasmData.Mode.Passive => ()
        }
        b.appendElement("\"" + data.bytes.map("\\%02x".format(_)).mkString + "\"")
      }
    )
  }

  private def writeTypeUse(typeName: WasmTypeName)(implicit b: WatBuilder): Unit = {
    b.sameLineList("type", b.appendName(typeName))
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
      case WasmHeapType.Type(typeName)        => b.appendName(typeName)
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
        writeTypeUse(name)
      case BlockType.ValueType(optTy) =>
        for (ty <- optTy)
          b.sameLineList("result", writeType(ty))
    }
  }

  private def writeLabelIdx(labelIdx: WasmLabelName)(implicit b: WatBuilder): Unit =
    b.appendName(labelIdx)

  private def writeInstr(instr: WasmInstr)(implicit b: WatBuilder): Unit = {
    instr match {
      case WasmInstr.PositionMark(_) =>
        // ignore
        ()

      case _ =>
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
        b.appendName(instr.funcArgument)
      case instr: WasmTypeInstr =>
        b.appendName(instr.typeArgument)
      case instr: WasmTagInstr =>
        b.appendName(instr.tagArgument)
      case instr: WasmLocalInstr =>
        b.appendName(instr.localArgument)
      case instr: WasmGlobalInstr =>
        b.appendName(instr.globalArgument)
      case instr: WasmHeapTypeInstr =>
        writeHeapType(instr.heapTypeArgument)
      case instr: WasmRefTypeInstr =>
        writeType(instr.refTypeArgument)
      case instr: WasmStructFieldInstr =>
        b.appendName(instr.structTypeName)
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
              clause.tag.foreach(tag => b.appendName(tag))
              writeLabelIdx(clause.label)
            }
          )
        }

      case ARRAY_NEW_DATA(typeIdx, dataIdx) =>
        b.appendName(typeIdx)
        b.appendName(dataIdx)

      case ARRAY_NEW_FIXED(typeIdx, length) =>
        b.appendName(typeIdx)
        b.appendElement(Integer.toUnsignedString(length))

      case ARRAY_COPY(destType, srcType) =>
        b.appendName(destType)
        b.appendName(srcType)

      case BR_ON_CAST(labelIdx, from, to) =>
        writeLabelIdx(labelIdx)
        writeType(from)
        writeType(to)
      case BR_ON_CAST_FAIL(labelIdx, from, to) =>
        writeLabelIdx(labelIdx)
        writeType(from)
        writeType(to)

      case PositionMark(pos) =>
        throw new AssertionError(s"Unexpected $instr")
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

    def appendName(name: WasmName): Unit =
      appendElement("$" + sanitizeWatIdentifier(name.name))

    /** @see https://webassembly.github.io/spec/core/text/values.html#text-id */
    private def sanitizeWatIdentifier(name: String): String = {
      if (name.isEmpty) "_"
      else if (name.forall(isValidWatIdentifierChar)) name
      else name.map(c => if (isValidWatIdentifierChar(c)) c else '_').mkString
    }

    private def isValidWatIdentifierChar(c: Char): Boolean = {
      c.isDigit || c.isLetter ||
      "!#$%&'*+-./:<=>?@\\^_`|~".contains(c) ||
      "$.@_".contains(c)
    }

    override def toString: String =
      builder.toString()
  }

}
