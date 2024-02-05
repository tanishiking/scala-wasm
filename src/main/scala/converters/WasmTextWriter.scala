package converters

import wasm4s._

class WasmTextWriter {
  private val indent = "  "

  def write(module: WasmModule)(implicit context: WasmContext): String = {
    implicit val b = new StringBuilder()
    // println(context.gcTypes.defined)
    // println(context.functions.defined)
    // println(context.functionTypes.defined)
    writeModule(module)
    b.toString()
  }

  private def writeModule(
      module: WasmModule
  )(implicit b: StringBuilder, context: WasmContext): Unit = {
    b.append("(module")
    module.functionTypes.foreach { ty =>
      b.append("\n").append(indent)
      writeFunctionType(ty)
    }
    // context.gcTypes
    module.definedFunctions.foreach { fun =>
      b.append("\n").append(indent)
      writeFunction(fun)
    }
    b.append(")")
  }

  private def writeFunctionType(
      functionType: WasmFunctionType
  )(implicit b: StringBuilder, ctx: WasmContext): Unit = {
    b.append(s"(type ${functionType.ident.name} ")
    b.append("(func ")

    functionType.params.foreach { ty =>
      b.append(s"(param ${ty.name}) ")
    }
    if (functionType.results.nonEmpty) {
      functionType.results.foreach { ty =>
        b.append(s"(result ${ty.name})")
      }
    }
    b.append(")") // "(func"
    b.append(")") // "(type $name ..."
  }

  private def writeFunction(f: WasmFunction)(implicit b: StringBuilder, ctx: WasmContext): Unit = {
    def writeParam(l: WasmLocal)(implicit b: StringBuilder): Unit = {
      b.append(s"(param ${l.ident.name} ${l.typ.name})")
    }
    b.append(s"(func ${f.ident.name} (type ${f.typ.ident.name}) ")
    f.locals.filter(_.isParameter).foreach(p => { writeParam(p); b.append(" ") })

    val fty = ctx.functionTypes.resolve(f.typ)
    fty.results.foreach(r => { b.append(s"(result ${r.name})") })

    b.append("\n")
    // body
    f.body.instr.foreach(i => { writeInstr(i); b.append("\n") })
    b.append(")") // (func ...
  }

  def writeInstr(instr: WasmInstr)(implicit b: StringBuilder, ctx: WasmContext): Unit = {
    b.append(s"${instr.mnemonic} ")
    instr.immediates.foreach { i =>
      val str = i match {
        case WasmImmediate.I64(v) => v.toString
        case WasmImmediate.I32(v) => v.toString
        case WasmImmediate.F64(v) => v.toString
        case WasmImmediate.F32(v) => v.toString
        case _                    => ???
      }
      b.append(s"$str ")
    }
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

}
