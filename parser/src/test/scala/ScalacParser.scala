package psp
package parser

import scala.tools.nsc._
import scala.reflect.io.AbstractFile
import scala.reflect.internal.util._
import reporters._

object ScalacGlobal {
  private def ivy       = sys.props("user.home") + "/.ivy2/cache"
  private def version   = "2.11.4"
  private def jars      = List("library", "reflect", "compiler") map (x => s"$ivy/org.scala-lang/scala-$x/jars/scala-$x-$version.jar")
  private def classpath = jars mkString ":"

  val settings = new Settings
  settings processArgumentString s"-Ystop-after:parser -cp $classpath"

  object quietReporter extends ConsoleReporter(settings) {
    override def printMessage(msg: String): Unit = ()
  }

  val global = new Global(settings, quietReporter)
  new global.Run

  import global._

  def apply(path: java.nio.file.Path) = synchronized {
    val f      = AbstractFile getFile path.toFile
    val source = new BatchSourceFile(f)
    val unit   = new CompilationUnit(source)
    val parser = new syntaxAnalyzer.UnitParser(unit)

    reporter.reset()
    val result = parser.parse()
    !reporter.hasErrors -> result
  }
}
