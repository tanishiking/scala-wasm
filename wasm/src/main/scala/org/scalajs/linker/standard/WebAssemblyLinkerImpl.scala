package org.scalajs.linker.standard

import org.scalajs.linker._
import org.scalajs.linker.interface._

import org.scalajs.linker.backend.WebAssemblyLinkerBackend

object WebAssemblyLinkerImpl {
  def linker(config: StandardConfig): Linker = {
    val frontend = StandardLinkerFrontend(config)
    val backend = new WebAssemblyLinkerBackend(config, frontend.coreSpec)
    WebAssemblyStandardLinkerImpl(frontend, backend)
  }

  def clearableLinker(config: StandardConfig): ClearableLinker =
    ClearableLinker(() => linker(config), config.batchMode)
}
