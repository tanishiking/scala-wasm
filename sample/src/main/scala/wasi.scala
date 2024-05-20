package scala.scalajs.wasm

object wasi {
  val STDOUT = 1
  val STDERR = 2

  // "wasi_snapshot_preview1", "fd_write"
  def fdWrite(descriptor: Int, iovs: Int, iovsLen: Int, rp: Int): Int = ???
}
