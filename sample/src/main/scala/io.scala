package scala.scalajs.wasm

object io {
  def printImpl(message: String, newLine: Boolean): Unit = {
    scala.scalajs.wasm.memory.withAllocator { allocator =>
      val bytes = message.toCharArray().map(_.toByte)
      wasiPrintImpl(allocator, bytes, newLine)
    }
  }
  def wasiPrintImpl(
      allocator: scala.scalajs.wasm.MemoryAllocator,
      bytes: Array[Byte],
      newLine: Boolean
  ) = {
    val size = bytes.size
    val memorySize = size + (if (newLine) 1 else 0)
    val segment = allocator.allocate(memorySize)
    for ((b, offset) <- bytes.zipWithIndex) {
      segment.setByte(offset, b)
    }
    if (newLine) {
      segment.setByte(memorySize - 1, 0x0A)
    }

    val iovs = allocator.allocate(8)
    iovs.setInt(0, segment.start)
    iovs.setInt(4, memorySize)

    val rp0 = allocator.allocate(4)

    val ret = wasi.fdWrite(
      descriptor = wasi.STDOUT,
      iovs = iovs.start,
      iovsLen = 1,
      rp = rp0.start
    )

    if (ret != 0) {
      // TODO: check return pointer's error code
    }
  }
}
