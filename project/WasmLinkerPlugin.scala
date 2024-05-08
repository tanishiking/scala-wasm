package build

import sbt._
import sbt.Keys._

import org.scalajs.linker._
import org.scalajs.linker.interface.{ModuleKind, _}

import org.scalajs.sbtplugin._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object WasmLinkerPlugin extends AutoPlugin {
  override lazy val requires = ScalaJSPlugin

  /** A `LinkerImpl` that reflectively loads our `WebAssemblyLinkerImpl`. */
  private class WasmLinkerImpl(base: LinkerImpl.Reflect)
      extends LinkerImpl.Forwarding(base) {

    private val loader = base.loader

    private val clearableLinkerMethod = {
      Class.forName("org.scalajs.linker.standard.WebAssemblyLinkerImpl", true, loader)
        .getMethod("clearableLinker", classOf[StandardConfig])
    }

    override def clearableLinker(config: StandardConfig): ClearableLinker =
      clearableLinkerMethod.invoke(null, config).asInstanceOf[ClearableLinker]
  }

  override def projectSettings: Seq[Setting[_]] = Def.settings(
    // Use a separate cache box for the LinkerImpl in this project (don't use the Global one)
    scalaJSLinkerImplBox := new CacheBox,

    // Use our custom WasmLinkerImpl as the linker implementation used by fast/fullLinkJS
    scalaJSLinkerImpl := {
      val cp = (scalaJSLinkerImpl / fullClasspath).value
      scalaJSLinkerImplBox.value.ensure {
        new WasmLinkerImpl(LinkerImpl.reflect(Attributed.data(cp)))
      }
    },

    // Automatically install all the configs required by the Wasm backend
    scalaJSLinkerConfig ~= { prev =>
      prev
        .withModuleKind(ModuleKind.ESModule)
        .withSemantics(_.optimized)
        .withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))
        .withOptimizer(false)
    },
  )
}
