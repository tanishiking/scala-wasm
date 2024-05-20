package scala.scalajs.wasm

object memory {
  def withAllocator(block: MemoryAllocator => Unit): Unit = {
    val allocator = new MemoryAllocator()
    try {
      block(allocator)
    } finally {
      allocator.free()
    }
  }

  def intrinsic: Nothing = throw new NotImplementedError("This is a stub for wasm intrinsics")
}

class MemorySegment(val start: Int, val size: Int) {
  import scala.scalajs.wasm.memory.intrinsic
  private def validate(offset: Int, requiredBytes: Int): Unit = {
    require(
      offset + requiredBytes >= 0 && offset + requiredBytes <= size,
      s"MemorySegment.validate($requiredBytes) failed, can't available $requiredBytes bytes"
    )
  }

  // def getByte(offset: Int): Int = {
  //   validate(offset, 1)
  //   _loadByte(offset + start)
  // }
  def getInt(offset: Int): Int = {
    validate(offset, 4)
    _loadInt(offset + start)
  }

  def setByte(offset: Int, value: Byte): Unit = {
    validate(offset, 1)
    _storeByte(offset + start, value)
  }
  def setInt(offset: Int, value: Int): Unit = {
    validate(offset, 4)
    _storeInt(offset + start, value)
  }

  private def _loadInt(offset: Int): Int = intrinsic
  private def _storeByte(offset: Int, value: Byte): Unit = intrinsic
  private def _storeInt(offset: Int, value: Int): Unit = intrinsic
}

class MemoryAllocator { // 1MB default initial size
  import scala.scalajs.wasm.memory.intrinsic
  def allocate(size: Int): MemorySegment = intrinsic
  def free(): Unit = intrinsic
}
