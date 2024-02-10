package wasm
package converters

import wasm4s._
import wasm4s.Names._
import ir2wasm.WasmBuilder
import wasm.wasm4s.Types.WasmHeapType
import wasm.wasm4s.Types.WasmHeapType

class WasmTextWriter {
  import WasmTextWriter._

  def write(module: WasmModule)(implicit context: WasmContext): String = {
    implicit val b = new WatBuilder()
    // println(context.gcTypes.defined)
    // println(context.functions.defined)
    // println(context.functionTypes.defined)
    writeModule(module)
    b.toString()
  }

  private def writeModule(
      module: WasmModule
  )(implicit b: WatBuilder, context: WasmContext): Unit = {
    b.newLineList(
      "module", {
        module.functionTypes.foreach(writeFunctionType)
        b.newLineList(
          "rec", {
            module.recGroupTypes.foreach(writeGCTypeDefinition)
          }
        )
        module.definedFunctions.foreach(writeFunction)
        module.globals.foreach(writeGlobal)
      }
    )
    // context.gcTypes
  }

  private def writeGCTypeDefinition(
      t: WasmGCTypeDefinition
  )(implicit b: WatBuilder, ctx: WasmContext): Unit = {
    def writeField(field: WasmStructField): Unit = {
      b.sameLineList(
        "field", {
          b.appendName(field.name)
          if (field.isMutable)
            b.sameLineList(
              "mut", {
                b.appendElement(field.typ.toString)
              }
            )
          else b.appendElement(field.typ.toString)
        }
      )
    }

    b.newLineList(
      "type", {
        b.appendName(t.name)
        t match {
          case WasmArrayType(name, field) =>
          // (type $kotlin.Any___type_13 (struct (field (ref $kotlin.Any.vtable___type_12)) (field (ref null struct)) (field (mut i32)) (field (mut i32))))
          case WasmStructType(name, fields, superType) =>
            b.sameLineList(
              "sub", {
                superType.foreach(b.appendName)
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
  )(implicit b: WatBuilder, ctx: WasmContext): Unit = {
    // (type type-name (func (param ty) (param ty) (result ty)))
    b.newLineList(
      "type", {
        b.appendName(functionType.name)
        b.sameLineList(
          "func", {
            functionType.params.foreach { ty =>
              b.sameLineListOne("param", ty.toString)
            }
          }
        )
        if (functionType.results.nonEmpty)
          functionType.results.foreach { ty => b.sameLineListOne("result", ty.toString) }
      }
    )
  }

  private def writeFunction(f: WasmFunction)(implicit b: WatBuilder, ctx: WasmContext): Unit = {
    def writeParam(l: WasmLocal)(implicit b: WatBuilder): Unit = {
      b.sameLineList(
        "param", {
          b.appendName(l.name)
          b.appendElement(l.typ.toString)
        }
      )
    }

    // def writeLocal(l: WasmLocal)(implicit b: WatBuilder): Unit = {
    // }

    b.newLineList(
      "func", {
        b.appendName(f.name)
        b.sameLineListOne("type", f.typ.name)

        b.newLine()
        f.locals.filter(_.isParameter).foreach(p => { writeParam(p) })
        val fty = ctx.functionTypes.resolve(f.typ)
        fty.results.foreach(r => { b.sameLineListOne("result", r.toString) })

        b.newLine()
        f.body.instr.foreach(writeInstr)
      }
    )
  }

  private def writeGlobal(g: WasmGlobal)(implicit
      b: WatBuilder,
      ctx: WasmContext
  ) =
    b.newLineList(
      "global", {
        b.appendName(g.name)
        b.appendElement(g.typ.toString)
        // TODO: init
      }
    )

  private def writeInstr(instr: WasmInstr)(implicit b: WatBuilder, ctx: WasmContext): Unit = {
    b.appendElement(instr.mnemonic)
    instr.immediates.foreach { i =>
      val str = i match {
        case WasmImmediate.I64(v)          => v.toString
        case WasmImmediate.I32(v)          => v.toString
        case WasmImmediate.F64(v)          => v.toString
        case WasmImmediate.F32(v)          => v.toString
        case WasmImmediate.LocalIdx(name)  => name.name
        case WasmImmediate.GlobalIdx(name) => name.name
        case WasmImmediate.HeapType(ht) =>
          ht match {
            case WasmHeapType.Type(typ) => typ.name
            case s: WasmHeapType.Simple => s.name
          }
        case WasmImmediate.FuncIdx(name)                => name.name
        case WasmImmediate.TypeIdx(name)                => name.name
        case WasmImmediate.StructFieldIdx(name)         => name.name
        case WasmImmediate.BlockType.FunctionType(name) => name.name
        case WasmImmediate.BlockType.ValueType(optTy) =>
          optTy.fold("") { ty => ty.toString() }
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
  def sanitizeWatIdentifier(indent: String): String =
    if (indent.isEmpty) "_"
    else if (indent.forall(isValidWatIdentifierChar)) indent
    else indent.map(c => if (isValidWatIdentifierChar(c)) c else '_').mkString

  /** @see https://webassembly.github.io/spec/core/text/values.html#text-id */
  def isValidWatIdentifierChar(c: Char): Boolean =
    c.isDigit || c.isLetter ||
      "!#$%&'*+-./:<=>?@\\^_`|~".contains(c) ||
      "$.@_".contains(c)

  class WatBuilder {
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

    def sameLineListOne(name: String, wasmName: WasmName) =
      sameLineList(name, { appendName(wasmName) })

    def appendName(name: WasmName): Unit =
      appendElement(sanitizeWatIdentifier(name.name))

    def appendElement(value: String): Unit = {
      builder.append(" ")
      builder.append(value)
    }

    override def toString: String =
      builder.toString()
  }

}
