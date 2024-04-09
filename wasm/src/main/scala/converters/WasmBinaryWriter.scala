package wasm
package converters

import scala.annotation.tailrec

import java.io.OutputStream
import java.io.ByteArrayOutputStream

import wasm.wasm4s._
import wasm.wasm4s.Names._
import wasm.wasm4s.Types._
import wasm.wasm4s.WasmInstr.END

final class WasmBinaryWriter(module: WasmModule, emitDebugInfo: Boolean) {
  import WasmBinaryWriter._

  private val allTypeDefinitions: List[WasmTypeDefinition[_ <: WasmTypeName]] = {
    module.recGroupTypes :::
      module.functionTypes :::
      module.arrayTypes
  }

  private val importTypeDefinitions: List[WasmFunctionType] = {
    module.imports.flatMap { imprt =>
      imprt.desc match {
        case WasmImportDesc.Func(id, typ)   => List(typ)
        case WasmImportDesc.Global(_, _, _) => Nil
        case WasmImportDesc.Tag(id, typ)    => List(typ)
      }
    }
  }

  private val typeIdxValues: Map[WasmTypeName, Int] =
    allTypeDefinitions.map(_.name).zipWithIndex.toMap

  private val dataIdxValues: Map[WasmDataName, Int] =
    module.data.map(_.name).zipWithIndex.toMap

  private val allFunctionNames: List[WasmFunctionName] = {
    val importedFunctionNames = module.imports.collect {
      case WasmImport(_, _, WasmImportDesc.Func(id, _)) => id
    }
    importedFunctionNames ::: module.definedFunctions.map(_.name)
  }

  private val funcIdxValues: Map[WasmFunctionName, Int] =
    allFunctionNames.zipWithIndex.toMap

  private val tagIdxValues: Map[WasmTagName, Int] = {
    val importedTagNames = module.imports.collect {
      case WasmImport(_, _, WasmImportDesc.Tag(id, _)) => id
    }
    val allNames = importedTagNames ::: module.tags.map(_.name)
    allNames.zipWithIndex.toMap
  }

  private val globalIdxValues: Map[WasmGlobalName, Int] = {
    val importedGlobalNames = module.imports.collect {
      case WasmImport(_, _, WasmImportDesc.Global(id, _, _)) => id
    }
    val allNames = importedGlobalNames ::: module.globals.map(_.name)
    allNames.zipWithIndex.toMap
  }

  private var localIdxValues: Option[Map[WasmLocalName, Int]] = None
  private var labelsInScope: List[Option[WasmImmediate.LabelIdx]] = Nil

  private def withLocalIdxValues(values: Map[WasmLocalName, Int])(f: => Unit): Unit = {
    val saved = localIdxValues
    localIdxValues = Some(values)
    try f
    finally localIdxValues = saved
  }

  def write(): Array[Byte] = {
    val fullOutput = new Buffer()

    // magic header: null char + "asm"
    fullOutput.byte(0)
    fullOutput.byte('a')
    fullOutput.byte('s')
    fullOutput.byte('m')

    // version
    fullOutput.byte(1)
    fullOutput.byte(0)
    fullOutput.byte(0)
    fullOutput.byte(0)

    writeSection(fullOutput, SectionType)(writeTypeSection(_))
    writeSection(fullOutput, SectionImport)(writeImportSection(_))
    writeSection(fullOutput, SectionFunction)(writeFunctionSection(_))
    writeSection(fullOutput, SectionTag)(writeTagSection(_))
    writeSection(fullOutput, SectionGlobal)(writeGlobalSection(_))
    writeSection(fullOutput, SectionExport)(writeExportSection(_))
    if (module.startFunction.isDefined)
      writeSection(fullOutput, SectionStart)(writeStartSection(_))
    writeSection(fullOutput, SectionElement)(writeElementSection(_))
    if (module.data.nonEmpty)
      writeSection(fullOutput, SectionDataCount)(writeDataCountSection(_))
    writeSection(fullOutput, SectionCode)(writeCodeSection(_))
    writeSection(fullOutput, SectionData)(writeDataSection(_))

    if (emitDebugInfo)
      writeCustomSection(fullOutput, "name")(writeNameCustomSection(_))

    fullOutput.result()
  }

  private def writeSection(fullOutput: Buffer, sectionID: Byte)(f: Buffer => Unit): Unit = {
    fullOutput.byte(sectionID)
    fullOutput.byteLengthSubSection(f)
  }

  private def writeCustomSection(fullOutput: Buffer, customSectionName: String)(
      f: Buffer => Unit
  ): Unit = {
    writeSection(fullOutput, SectionCustom) { buf =>
      buf.name(customSectionName)
      f(buf)
    }
  }

  private def writeTypeSection(buf: Buffer): Unit = {
    buf.u32(1 + importTypeDefinitions.size) // a single `rectype` + the import type definitions

    // the big rectype

    buf.byte(0x4E) // `rectype` tag
    buf.u32(typeIdxValues.size) // number of `subtype`s in our single `rectype`

    def writeFieldType(field: WasmStructField): Unit = {
      writeType(buf, field.typ)
      buf.boolean(field.isMutable)
    }

    for (typeDef <- allTypeDefinitions) {
      typeDef match {
        case WasmArrayType(name, field) =>
          buf.byte(0x5E) // array
          writeFieldType(field)
        case WasmStructType(name, fields, superType) =>
          buf.byte(0x50) // sub
          buf.opt(superType)(writeTypeIdx(buf, _))
          buf.byte(0x5F) // struct
          buf.vec(fields)(writeFieldType(_))
        case WasmFunctionType(name, params, results) =>
          buf.byte(0x60) // func
          writeResultType(buf, params)
          writeResultType(buf, results)
      }
    }

    // the import type definitions, outside the rectype

    for (typeDef <- importTypeDefinitions) {
      val WasmFunctionType(name, params, results) = typeDef
      buf.byte(0x60) // func
      writeResultType(buf, params)
      writeResultType(buf, results)
    }
  }

  private def writeImportSection(buf: Buffer): Unit = {
    buf.vec(module.imports) { imprt =>
      buf.name(imprt.module)
      buf.name(imprt.name)

      val indexBase = allTypeDefinitions.size
      val importedFunTypeIdx =
        importTypeDefinitions.map(_.name).zipWithIndex.map(kv => (kv._1, kv._2 + indexBase)).toMap

      def writeImportedTypeIdx(typeName: WasmTypeName.WasmFunctionTypeName): Unit =
        buf.u32(importedFunTypeIdx(typeName))

      imprt.desc match {
        case WasmImportDesc.Func(id, typ) =>
          buf.byte(0x00) // func
          writeImportedTypeIdx(typ.name)
        case WasmImportDesc.Global(id, typ, isMutable) =>
          buf.byte(0x03) // global
          writeType(buf, typ)
          buf.boolean(isMutable)
        case WasmImportDesc.Tag(id, typ) =>
          buf.byte(0x04) // tag
          buf.byte(0x00) // exception kind (that is the only valid kind for now)
          writeImportedTypeIdx(typ.name)
      }
    }
  }

  private def writeFunctionSection(buf: Buffer): Unit = {
    buf.vec(module.definedFunctions) { fun =>
      writeTypeIdx(buf, fun.typ.name)
    }
  }

  private def writeTagSection(buf: Buffer): Unit = {
    buf.vec(module.tags) { tag =>
      buf.byte(0x00) // exception kind (that is the only valid kind for now)
      writeTypeIdx(buf, tag.typ)
    }
  }

  private def writeGlobalSection(buf: Buffer): Unit = {
    buf.vec(module.globals) { global =>
      writeType(buf, global.typ)
      buf.boolean(global.isMutable)
      writeExpr(buf, global.init)
    }
  }

  private def writeExportSection(buf: Buffer): Unit = {
    buf.vec(module.exports) { exp =>
      buf.name(exp.exportName)
      exp match {
        case WasmExport.Function(_, funcName) =>
          buf.byte(0x00)
          writeFuncIdx(buf, funcName)
        case WasmExport.Global(_, globalName) =>
          buf.byte(0x03)
          writeGlobalIdx(buf, globalName)
      }
    }
  }

  private def writeStartSection(buf: Buffer): Unit = {
    writeFuncIdx(buf, module.startFunction.get)
  }

  private def writeElementSection(buf: Buffer): Unit = {
    buf.vec(module.elements) { element =>
      element.mode match {
        case WasmElement.Mode.Passive     => buf.byte(5)
        case WasmElement.Mode.Declarative => buf.byte(7)
      }
      writeType(buf, element.typ)
      buf.vec(element.init) { expr =>
        writeExpr(buf, expr)
      }
    }
  }

  /** https://webassembly.github.io/spec/core/binary/modules.html#data-section
    */
  private def writeDataSection(buf: Buffer): Unit = {
    buf.vec(module.data) { data =>
      data.mode match {
        case WasmData.Mode.Passive => buf.byte(1)
      }
      buf.vec(data.bytes)(buf.byte)
    }
  }

  private def writeDataCountSection(buf: Buffer): Unit =
    buf.u32(module.data.size)

  private def writeCodeSection(buf: Buffer): Unit = {
    buf.vec(module.definedFunctions) { func =>
      buf.byteLengthSubSection(writeFunc(_, func))
    }
  }

  private def writeNameCustomSection(buf: Buffer): Unit = {
    // Currently, we only emit the function names

    buf.byte(0x01) // function names
    buf.byteLengthSubSection { buf =>
      buf.vec(allFunctionNames.zipWithIndex) { elem =>
        buf.u32(elem._2)
        buf.name(elem._1.show)
      }
    }
  }

  private def writeFunc(buf: Buffer, func: WasmFunction): Unit = {
    buf.vec(func.locals.filter(!_.isParameter)) { local =>
      buf.u32(1)
      writeType(buf, local.typ)
    }

    withLocalIdxValues(func.locals.map(_.name).zipWithIndex.toMap) {
      writeExpr(buf, func.body)
    }
  }

  private def writeType(buf: Buffer, typ: WasmStorageType): Unit = {
    typ match {
      case typ: WasmSimpleType => buf.byte(typ.binaryCode)
      case typ: WasmPackedType => buf.byte(typ.binaryCode)

      case WasmRefType(true, heapType: WasmHeapType.AbsHeapType) =>
        buf.byte(heapType.binaryCode)

      case WasmRefType(nullable, heapType) =>
        buf.byte(if (nullable) 0x63 else 0x64)
        writeHeapType(buf, heapType)
    }
  }

  private def writeHeapType(buf: Buffer, heapType: WasmHeapType): Unit = {
    heapType match {
      case WasmHeapType.Type(typeName)        => writeTypeIdxs33(buf, typeName)
      case heapType: WasmHeapType.AbsHeapType => buf.byte(heapType.binaryCode)
    }
  }

  private def writeResultType(buf: Buffer, resultType: List[WasmType]): Unit =
    buf.vec(resultType)(writeType(buf, _))

  private def writeTypeIdx(buf: Buffer, typeName: WasmTypeName): Unit =
    buf.u32(typeIdxValues(typeName))

  private def writeDataIdx(buf: Buffer, dataName: WasmDataName): Unit =
    buf.u32(dataIdxValues(dataName))

  private def writeTypeIdxs33(buf: Buffer, typeName: WasmTypeName): Unit =
    buf.s33OfUInt(typeIdxValues(typeName))

  private def writeFuncIdx(buf: Buffer, funcName: WasmFunctionName): Unit =
    buf.u32(funcIdxValues(funcName))

  private def writeTagIdx(buf: Buffer, tagName: WasmTagName): Unit =
    buf.u32(tagIdxValues(tagName))

  private def writeGlobalIdx(buf: Buffer, globalName: WasmGlobalName): Unit =
    buf.u32(globalIdxValues(globalName))

  private def writeLocalIdx(buf: Buffer, localName: WasmLocalName): Unit = {
    localIdxValues match {
      case Some(values) => buf.u32(values(localName))
      case None         => throw new IllegalStateException(s"Local name table is not available")
    }
  }

  private def writeLabelIdx(buf: Buffer, labelIdx: WasmImmediate.LabelIdx): Unit = {
    val relativeNumber = labelsInScope.indexOf(Some(labelIdx))
    if (relativeNumber < 0)
      throw new IllegalStateException(s"Cannot find $labelIdx in scope")
    buf.u32(relativeNumber)
  }

  private def writeExpr(buf: Buffer, expr: WasmExpr): Unit = {
    for (instr <- expr.instr)
      writeInstr(buf, instr)
    buf.byte(0x0B) // end
  }

  private def writeInstr(buf: Buffer, instr: WasmInstr): Unit = {
    val opcode = instr.opcode
    if (opcode <= 0xFF) {
      buf.byte(opcode.toByte)
    } else {
      assert(
        opcode <= 0xFFFF,
        s"cannot encode an opcode longer than 2 bytes yet: ${opcode.toHexString}"
      )
      buf.byte((opcode >>> 8).toByte)
      buf.byte(opcode.toByte)
    }

    writeInstrImmediates(buf, instr)

    instr match {
      case instr: WasmInstr.StructuredLabeledInstr =>
        // We must register even the `None` labels, because they contribute to relative numbering
        labelsInScope ::= instr.label
      case END =>
        labelsInScope = labelsInScope.tail
      case _ =>
        ()
    }
  }

  private def writeInstrImmediates(buf: Buffer, instr: WasmInstr): Unit = {
    import WasmInstr._

    def writeBrOnCast(
        labelIdx: WasmImmediate.LabelIdx,
        from: WasmRefType,
        to: WasmRefType
    ): Unit = {
      val castFlags = ((if (from.nullable) 1 else 0) | (if (to.nullable) 2 else 0)).toByte
      buf.byte(castFlags)
      writeLabelIdx(buf, labelIdx)
      writeHeapType(buf, from.heapType)
      writeHeapType(buf, to.heapType)
    }

    instr match {
      // Convenience categories

      case instr: WasmSimpleInstr =>
        ()
      case instr: WasmBlockTypeLabeledInstr =>
        writeBlockType(buf, instr.blockTypeArgument)
      case instr: WasmLabelInstr =>
        writeLabelIdx(buf, instr.labelArgument)
      case instr: WasmFuncInstr =>
        writeFuncIdx(buf, instr.funcArgument.value)
      case instr: WasmTypeInstr =>
        writeTypeIdx(buf, instr.typeArgument.value)
      case instr: WasmTagInstr =>
        writeTagIdx(buf, instr.tagArgument.value)
      case instr: WasmLocalInstr =>
        writeLocalIdx(buf, instr.localArgument.value)
      case instr: WasmGlobalInstr =>
        writeGlobalIdx(buf, instr.globalArgument.value)
      case instr: WasmHeapTypeInstr =>
        writeHeapType(buf, instr.heapTypeArgument.value)
      case instr: WasmRefTypeInstr =>
        writeHeapType(buf, instr.refTypeArgument.heapType)
      case instr: WasmStructFieldInstr =>
        writeTypeIdx(buf, instr.structTypeIdx.value)
        buf.u32(instr.fieldIdx.value)

      // Specific instructions with unique-ish shapes

      case I32_CONST(v) => buf.i32(v.value)
      case I64_CONST(v) => buf.i64(v.value)
      case F32_CONST(v) => buf.f32(v.value)
      case F64_CONST(v) => buf.f64(v.value)

      case BR_TABLE(labelIdxVector, defaultLabelIdx) =>
        buf.vec(labelIdxVector.value)(writeLabelIdx(buf, _))
        writeLabelIdx(buf, defaultLabelIdx)

      case TRY_TABLE(blockType, clauses, _) =>
        writeBlockType(buf, blockType)
        buf.vec(clauses.value) { clause =>
          buf.byte(clause.opcode.toByte)
          clause.tag.foreach(tag => writeTagIdx(buf, tag.value))
          writeLabelIdx(buf, clause.label)
        }

      case ARRAY_NEW_DATA(typeIdx, dataIdx) =>
        writeTypeIdx(buf, typeIdx.value)
        writeDataIdx(buf, dataIdx.value)

      case ARRAY_NEW_FIXED(typeIdx, length) =>
        writeTypeIdx(buf, typeIdx.value)
        buf.u32(length.value)

      case BR_ON_CAST(labelIdx, from, to) =>
        writeBrOnCast(labelIdx, from, to)
      case BR_ON_CAST_FAIL(labelIdx, from, to) =>
        writeBrOnCast(labelIdx, from, to)
    }
  }

  private def writeBlockType(buf: Buffer, blockType: WasmImmediate.BlockType): Unit = {
    import WasmImmediate._

    blockType match {
      case BlockType.ValueType(None)        => buf.byte(0x40)
      case BlockType.ValueType(Some(typ))   => writeType(buf, typ)
      case BlockType.FunctionType(typeName) => writeTypeIdxs33(buf, typeName)
    }
  }
}

object WasmBinaryWriter {
  private final val SectionCustom = 0x00
  private final val SectionType = 0x01
  private final val SectionImport = 0x02
  private final val SectionFunction = 0x03
  private final val SectionTable = 0x04
  private final val SectionMemory = 0x05
  private final val SectionGlobal = 0x06
  private final val SectionExport = 0x07
  private final val SectionStart = 0x08
  private final val SectionElement = 0x09
  private final val SectionCode = 0x0A
  private final val SectionData = 0x0B
  private final val SectionDataCount = 0x0C
  private final val SectionTag = 0x0D

  private final class Buffer {
    private val buf = new java.io.ByteArrayOutputStream()

    def result(): Array[Byte] = buf.toByteArray()

    def byte(b: Byte): Unit =
      buf.write(b & 0xFF)

    def rawByteArray(array: Array[Byte]): Unit =
      buf.write(array)

    def boolean(b: Boolean): Unit =
      byte(if (b) 1 else 0)

    def u32(value: Int): Unit = unsignedLEB128(Integer.toUnsignedLong(value))

    def s32(value: Int): Unit = signedLEB128(value.toLong)

    def i32(value: Int): Unit = s32(value)

    def s33OfUInt(value: Int): Unit = signedLEB128(Integer.toUnsignedLong(value))

    def u64(value: Long): Unit = unsignedLEB128(value)

    def s64(value: Long): Unit = signedLEB128(value)

    def i64(value: Long): Unit = s64(value)

    def f32(value: Float): Unit = {
      val bits = java.lang.Float.floatToIntBits(value)
      byte(bits.toByte)
      byte((bits >>> 8).toByte)
      byte((bits >>> 16).toByte)
      byte((bits >>> 24).toByte)
    }

    def f64(value: Double): Unit = {
      val bits = java.lang.Double.doubleToLongBits(value)
      byte(bits.toByte)
      byte((bits >>> 8).toByte)
      byte((bits >>> 16).toByte)
      byte((bits >>> 24).toByte)
      byte((bits >>> 32).toByte)
      byte((bits >>> 40).toByte)
      byte((bits >>> 48).toByte)
      byte((bits >>> 56).toByte)
    }

    def vec[A](elems: Iterable[A])(op: A => Unit): Unit = {
      u32(elems.size)
      for (elem <- elems)
        op(elem)
    }

    def opt[A](elemOpt: Option[A])(op: A => Unit): Unit =
      vec(elemOpt.toList)(op)

    def name(s: String): Unit = {
      val utf8 = org.scalajs.ir.UTF8String(s)
      val len = utf8.length
      u32(len)
      var i = 0
      while (i != len) {
        byte(utf8(i))
        i += 1
      }
    }

    def byteLengthSubSection(f: Buffer => Unit): Unit = {
      val subBuffer = new Buffer()
      f(subBuffer)
      val subResult = subBuffer.result()

      this.u32(subResult.length)
      this.rawByteArray(subResult)
    }

    @tailrec
    private def unsignedLEB128(value: Long): Unit = {
      val next = value >>> 7
      if (next == 0) {
        buf.write(value.toInt)
      } else {
        buf.write((value.toInt & 0x7F) | 0x80)
        unsignedLEB128(next)
      }
    }

    @tailrec
    private def signedLEB128(value: Long): Unit = {
      val chunk = value.toInt & 0x7F
      val next = value >> 7
      if (next == (if ((chunk & 0x40) != 0) -1 else 0)) {
        buf.write(chunk)
      } else {
        buf.write(chunk | 0x80)
        signedLEB128(next)
      }
    }
  }
}
