package cli

import scala.concurrent.{ExecutionContext, Future}

import org.scalajs.linker.StandardImpl
import org.scalajs.linker.NodeIRContainer
import org.scalajs.linker.interface.IRFile

class CliReader(val classpath: List[String]) {

  def irFiles(implicit ec: ExecutionContext): Future[Seq[IRFile]] = {
    val cache = StandardImpl.irFileCache().newCache

    NodeIRContainer
      .fromClasspath(classpath)
      .map(x => {
        println(x)
        x
      })
      .map(_._1)
      .flatMap(cache.cached _)
  }
}
