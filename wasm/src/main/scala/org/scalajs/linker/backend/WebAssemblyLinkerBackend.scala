package org.scalajs.linker.backend

import scala.concurrent.{ExecutionContext, Future}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import org.scalajs.logging.Logger

import org.scalajs.linker._
import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._
import org.scalajs.linker.standard._

import org.scalajs.linker.backend.javascript.{ByteArrayWriter, SourceMapWriter}
import org.scalajs.linker.backend.webassembly._

import org.scalajs.linker.backend.wasmemitter.Emitter

final class WebAssemblyLinkerBackend(
    linkerConfig: StandardConfig,
    val coreSpec: CoreSpec
) extends LinkerBackend {
  require(
    linkerConfig.moduleKind == ModuleKind.ESModule,
    s"The WebAssembly backend only supports ES modules; was ${linkerConfig.moduleKind}."
  )
  require(
    !linkerConfig.optimizer,
    "The WebAssembly backend does not support the optimizer yet."
  )

  val loaderJSFileName = OutputPatternsImpl.jsFile(linkerConfig.outputPatterns, "__loader")

  private val emitter: Emitter =
    new Emitter(Emitter.Config(coreSpec, loaderJSFileName))

  val symbolRequirements: SymbolRequirement = emitter.symbolRequirements

  def injectedIRFiles: Seq[IRFile] = emitter.injectedIRFiles

  def emit(moduleSet: ModuleSet, output: OutputDirectory, logger: Logger)(implicit
      ec: ExecutionContext
  ): Future[Report] = {
    val onlyModule = moduleSet.modules match {
      case onlyModule :: Nil =>
        onlyModule
      case modules =>
        throw new UnsupportedOperationException(
          "The WebAssembly backend does not support multiple modules. Found: " +
            modules.map(_.id.id).mkString(", ")
        )
    }
    val moduleID = onlyModule.id.id

    val emitterResult = emitter.emit(onlyModule, logger)
    val wasmModule = emitterResult.wasmModule

    val outputImpl = OutputDirectoryImpl.fromOutputDirectory(output)

    val watFileName = s"$moduleID.wat"
    val wasmFileName = s"$moduleID.wasm"
    val sourceMapFileName = s"$wasmFileName.map"
    val jsFileName = OutputPatternsImpl.jsFile(linkerConfig.outputPatterns, moduleID)

    val filesToProduce0 = Set(
      wasmFileName,
      loaderJSFileName,
      jsFileName
    )
    val filesToProduce1 =
      if (linkerConfig.sourceMap) filesToProduce0 + sourceMapFileName
      else filesToProduce0
    val filesToProduce =
      if (linkerConfig.prettyPrint) filesToProduce1 + watFileName
      else filesToProduce1

    def maybeWriteWatFile(): Future[Unit] = {
      if (linkerConfig.prettyPrint) {
        val textOutput = new TextWriter(wasmModule).write()
        val textOutputBytes = textOutput.getBytes(StandardCharsets.UTF_8)
        outputImpl.writeFull(watFileName, ByteBuffer.wrap(textOutputBytes))
      } else {
        Future.unit
      }
    }

    def writeWasmFile(): Future[Unit] = {
      val emitDebugInfo = !linkerConfig.minify

      if (linkerConfig.sourceMap) {
        val sourceMapWriter = new ByteArrayWriter

        val wasmFileURI = s"./$wasmFileName"
        val sourceMapURI = s"./$sourceMapFileName"

        val smWriter =
          new SourceMapWriter(sourceMapWriter, wasmFileURI, linkerConfig.relativizeSourceMapBase)
        val binaryOutput = new BinaryWriter.WithSourceMap(
          wasmModule,
          emitDebugInfo,
          smWriter,
          sourceMapURI
        ).write()
        smWriter.complete()

        outputImpl.writeFull(wasmFileName, ByteBuffer.wrap(binaryOutput)).flatMap { _ =>
          outputImpl.writeFull(sourceMapFileName, sourceMapWriter.toByteBuffer())
        }
      } else {
        val binaryOutput = new BinaryWriter(wasmModule, emitDebugInfo).write()
        outputImpl.writeFull(wasmFileName, ByteBuffer.wrap(binaryOutput))
      }
    }

    def writeLoaderFile(): Future[Unit] =
      outputImpl.writeFull(loaderJSFileName, ByteBuffer.wrap(emitterResult.loaderContent))

    def writeJSFile(): Future[Unit] = {
      val jsFileOutputBytes = emitterResult.jsFileContent.getBytes(StandardCharsets.UTF_8)
      outputImpl.writeFull(jsFileName, ByteBuffer.wrap(jsFileOutputBytes))
    }

    for {
      existingFiles <- outputImpl.listFiles()
      _ <- Future.sequence(existingFiles.filterNot(filesToProduce).map(outputImpl.delete(_)))
      _ <- maybeWriteWatFile()
      _ <- writeWasmFile()
      _ <- writeLoaderFile()
      _ <- writeJSFile()
    } yield {
      val reportModule = new ReportImpl.ModuleImpl(
        moduleID,
        jsFileName,
        None,
        linkerConfig.moduleKind
      )
      new ReportImpl(List(reportModule))
    }
  }
}
