package wasm
package converters

import wasm4s._
import wasm4s.Names._
import ir2wasm.WasmBuilder
import wasm.wasm4s.Types.WasmHeapType

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
          }
        )
        module.definedFunctions.foreach(writeFunction)
        module.globals.foreach(writeGlobal)
        module.exports.foreach(writeExport)
      }
    )
    // context.gcTypes
  }

  private def writeGCTypeDefinition(
      t: WasmGCTypeDefinition
  )(implicit b: WatBuilder): Unit = {
    def writeField(field: WasmStructField): Unit = {
      b.newLineList(
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
          b.newLine()
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
        b.newLine()
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

  private def writeInstr(instr: WasmInstr)(implicit b: WatBuilder): Unit = {
    b.appendElement(instr.mnemonic)
    instr.immediates.foreach { i =>
      val str = i match {
        case WasmImmediate.I64(v)          => v.toString()
        case WasmImmediate.I32(v)          => v.toString()
        case WasmImmediate.F64(v)          => v.toString()
        case WasmImmediate.F32(v)          => v.toString()
        case WasmImmediate.LocalIdx(name)  => name.show
        case WasmImmediate.GlobalIdx(name) => name.show
        case WasmImmediate.HeapType(ht) =>
          ht match {
            case WasmHeapType.Type(typ) => typ.show
            case WasmHeapType.Func(typ) => typ.show
            case s: WasmHeapType.Simple => s.name
          }
        case WasmImmediate.FuncIdx(name)                => name.show
        case WasmImmediate.TypeIdx(name)                => name.show
        case WasmImmediate.StructFieldIdx(v)            => v.toString
        case WasmImmediate.BlockType.FunctionType(name) => name.show
        case WasmImmediate.BlockType.ValueType(optTy) =>
          optTy.fold("") { ty => ty.show }
        case _ =>
          println(i)
          ???
      }
      b.appendElement(str)
    }
    b.newLine()
  }

}

object WasmTextWriter {
  private class WatBuilder {
    private val builder = new StringBuilder
    private var level = 0
    private val indent = "  "

    private def indented(body: => Unit): Unit = {
      level += 1
      body
      level -= 1
    }

    def newLine(): Unit = {
      builder.append("\n")
      builder.append(indent * level)
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
