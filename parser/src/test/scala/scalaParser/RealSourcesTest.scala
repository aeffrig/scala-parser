package psp
package parser
package tests

import org.parboiled2.ParseError
import psp.std._, api._, pio._, ansi._, StdEq._

sealed trait Result { def ansi: String }
final case object Pass extends Result { def ansi = "ok".green.to_s }
final case object Fail extends Result { def ansi = "failed".red.to_s }
final case object Skip extends Result { def ansi = "skip".yellow.to_s }

object RealSourcesTest {
  implicit val pathOrder: Order[Path] = orderBy[Path](_.to_s)
  implicit val resultEq = Eq.natural[Result]()

  def checkPath(f: Path): Result = {
    val input = try f.slurp() catch { case _: NoSuchFileException => return Skip }
    newScalaParser(input).parseAllRule.run() match {
      case Success(`input`) => Pass
      case _                => if (ScalacGlobal(f)._1) Fail else Skip
    }
  }

  def checkRoot(root: Path): Result = {
    val srcs = root.deepFiles filter (_ hasExtension "scala") force;
    Console.putOut(s"%4s source files in %25s: ".format(srcs.length, root.filename))
    val results = srcs map checkPath
    val skipped = results count (_ === Skip) match {
      case 0 => ""
      case n => s" ($n skipped - nobody can compile)"
    }
    val res = if (results contains Fail) Fail else Pass
    println(res.ansi + skipped)
    res
  }

  def main(args: Array[String]): Unit = {
    val roots   = args flatMap (x => path(x).entries) force;
    val results = roots map checkRoot
    if (results contains Fail)
      runtimeException("There were test failures.")
  }
}
