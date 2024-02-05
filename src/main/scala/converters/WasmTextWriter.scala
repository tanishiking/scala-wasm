package converters

import wasm4s._

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
        module.definedFunctions.foreach(writeFunction)
      }
    )
    // context.gcTypes
  }

  private def writeFunctionType(
      functionType: WasmFunctionType
  )(implicit b: WatBuilder, ctx: WasmContext): Unit = {
    // (type type-name (func (param ty) (param ty) (result ty)))
    b.newLineList(
      "type", {
        b.appendIdent(functionType.ident)
        b.sameLineList(
          "func", {
            functionType.params.foreach { ty =>
              b.sameLineListOne("param", ty.name)
            }
          }
        )
        if (functionType.results.nonEmpty)
          functionType.results.foreach { ty => b.sameLineListOne("result", ty.name) }
      }
    )
  }

  private def writeFunction(f: WasmFunction)(implicit b: WatBuilder, ctx: WasmContext): Unit = {
    def writeParam(l: WasmLocal)(implicit b: WatBuilder): Unit = {
      b.sameLineList(
        "param", {
          b.appendIdent(l.ident)
          b.appendElement(l.typ.name)
        }
      )
    }

    b.newLineList(
      "func", {
        b.appendIdent(f.ident)
        b.sameLineListOne("type", f.typ.ident)

        b.newLine()
        f.locals.filter(_.isParameter).foreach(p => { writeParam(p) })
        val fty = ctx.functionTypes.resolve(f.typ)
        fty.results.foreach(r => { b.sameLineListOne("result", r.name) })

        b.newLine()
        f.body.instr.foreach(writeInstr)
      }
    )
  }

  def writeInstr(instr: WasmInstr)(implicit b: WatBuilder, ctx: WasmContext): Unit = {
    b.appendElement(instr.mnemonic)
    instr.immediates.foreach { i =>
      val str = i match {
        case WasmImmediate.I64(v) => v.toString
        case WasmImmediate.I32(v) => v.toString
        case WasmImmediate.F64(v) => v.toString
        case WasmImmediate.F32(v) => v.toString
        case WasmImmediate.LocalIdx(sym) =>
          sym.ident.name
        case _ => ???
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

    def sameLineListOne(name: String, ident: Ident) =
      sameLineList(name, { appendIdent(ident) })

    def appendIdent(ident: Ident): Unit =
      appendElement(sanitizeWatIdentifier(ident.name))

    def appendElement(value: String): Unit = {
      builder.append(" ")
      builder.append(value)
    }

    override def toString: String =
      builder.toString()
  }

}
