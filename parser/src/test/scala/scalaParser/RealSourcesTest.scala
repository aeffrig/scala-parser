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
  def maxFileLen   = 120
  def maxFileFmt   = "%-" + maxFileLen + "s"

  implicit val pathOrder: Order[Path] = orderBy[Path](_.to_s)

  def checkPath(f: Path): Result = {
    val input    = try f.slurp() catch { case _: NoSuchFileException => return Skip }
    val path_s = {
      try f.toString stripPrefix "real/" stripPrefix "scala-sources/" match {
        case s if s.length <= maxFileLen => s
        case s                           => "..." + (s takeRight maxFileLen - 3).force
      }
      catch { case _: Exception => return Skip }
    }
    lazy val parser = newScalaParser(input)
    Console.putOut(s"[%6s] $maxFileFmt  ".format(input.length, path_s))

    def finish(res: Result, str: String): Result = {
      Console.putOut(res.ansi)
      if (str == "") println("")
      else if (res == Fail) println(s"\n$str\n")
      else println(str)

      res
    }

    def checkScalac(err: ParseError): Result = ScalacGlobal(f) match {
      case ((true, res))  => finish(Fail, parser.failMessage(f, err))
      case ((false, res)) => finish(Pass, " (nobody can parse)")
    }
    parser.parseAllRule.run() match {
      case Success(`input`)       => finish(Pass, "")
      case Failure(t: ParseError) => checkScalac(t)
      case Failure(t)             => finish(Fail, s"Unexpected failure $t")
    }
  }

  def main(args: Array[String]): Unit = {
    val files   = args.flatMap(arg => path(arg).deepFiles).distinctBy(_.realPath.toString) filter (_ hasExtension "scala") force;
    println(s"${files.length} scala files to check.")

    val results = files.sorted.map(f => f -> checkPath(f)).force
    val pass    = results filter (_._2 == Pass) force
    val skip    = results filter (_._2 == Skip) force
    val fail    = results filter (_._2 == Fail) force
    val total   = skip.length + pass.length + fail.length

    println(s"%s tests: %s pass, %s fail, %s skipped".format(total, pass.length, fail.length, skip.length))

    if (fail.nonEmpty) {
      println("\nSkipped:\n")
      skip foreach (x => println(x._1))
      println("\nFailures:\n")
      fail foreach (x => println(x._1))

      runtimeException(s"There were (${fail.length}) test failures.")
    }
  }
}
