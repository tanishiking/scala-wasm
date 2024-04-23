package org.scalajs.linker.backend.webassembly

import java.net.URI
import java.nio.ByteBuffer

import org.scalajs.linker.backend.javascript.{ByteArrayWriter, SourceMapWriter}

object SourceMapWriterAccess {

  /** A box to hold a `ByteArrayWriter`, which is unfortunately package-private in Scala.js but
    * required to create a `SourceMapWriter`.
    */
  final class ByteArrayWriterBox() {
    private val underlying = new ByteArrayWriter()

    def createSourceMapWriter(jsFileName: String, relativizeBaseURI: Option[URI]): SourceMapWriter =
      new SourceMapWriter(underlying, jsFileName, relativizeBaseURI)

    def toByteBuffer(): ByteBuffer =
      underlying.toByteBuffer()
  }
}
