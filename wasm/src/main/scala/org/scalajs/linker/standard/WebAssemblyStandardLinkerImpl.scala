package org.scalajs.linker.standard

import scala.concurrent._

import java.util.concurrent.atomic.AtomicBoolean

import org.scalajs.logging.Logger

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._

import org.scalajs.linker.backend.WebAssemblyLinkerBackend
import org.scalajs.linker.backend.wasmemitter.LibraryPatches

/** Standard implementation of a Scala.js WebAssembly linker.
  *
  * TODO This is mostly a copy-paste of `org.scalajs.standard.StandardLinkerImpl`. The only
  * difference is that it patches the input `IRFile`s with `LibraryPatches` before giving them to
  * the linker frontend. Normally, this would be handled with `LinkerBackend.injectedIRFiles`, but
  * we cannot use it for two reasons:
  *
  *   - `injectedIRFiles` are injected *after* user IR files, but we use them to *override* some
  *     stuff from the library, so they must come before.
  *   - Some our injected IR files are *derived* from the input IR files.
  */
private final class WebAssemblyStandardLinkerImpl private (
    frontend: LinkerFrontend,
    backend: LinkerBackend
) extends LinkerImpl {

  require(
    frontend.coreSpec == backend.coreSpec,
    "Frontend and backend must implement the same core specification"
  )

  private[this] var _valid = true
  private[this] val _linking = new AtomicBoolean(false)

  def link(
      irFiles: Seq[IRFile],
      moduleInitializers: Seq[ModuleInitializer],
      output: OutputDirectory,
      logger: Logger
  )(implicit ec: ExecutionContext): Future[Report] = {
    if (!_linking.compareAndSet(false, true)) {
      throw new IllegalStateException("Linker used concurrently")
    }

    checkValid()
      .flatMap(_ => LibraryPatches.patchIRFiles(irFiles)) // <-- HERE we differ from standard
      .flatMap { patchedIRFiles =>
        frontend.link(
          patchedIRFiles ++ backend.injectedIRFiles,
          moduleInitializers,
          backend.symbolRequirements,
          logger
        )
      }
      .flatMap(linkingUnit => backend.emit(linkingUnit, output, logger))
      .andThen { case t if t.isFailure => _valid = false }
      .andThen { case t => _linking.set(false) }
  }

  private def checkValid(): Future[Unit] = {
    if (!_valid) {
      Future.failed(
        new IllegalStateException("Linker is invalid due to a previous exception in a component")
      )
    } else {
      Future.successful(())
    }
  }
}

object WebAssemblyStandardLinkerImpl {
  def apply(frontend: LinkerFrontend, backend: WebAssemblyLinkerBackend): Linker =
    new WebAssemblyStandardLinkerImpl(frontend, backend)
}
