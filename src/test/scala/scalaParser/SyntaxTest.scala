package scalaParser

import org.parboiled2.ParseError
import psp.std.{ assert => _, _}
import psp.std.api._
import scala.util.{Failure, Success}
import psp.std.ansi._
import scala.sys.process.Process

object SyntaxTest {
  import Predef.{ augmentString => _, wrapString => _, ArrowAssoc => _, _ }

  private def tryOpt[A](body: => Option[A]): Option[A] = Try(body) | None

  lazy val skipPaths: Set[String] = resourceString("skip.txt").trim.lines.toSet

  def maxFileLen            = 100
  def maxFileFmt            = "%-" + maxFileLen + "s"
  def scalaSources          = "."

  def scalaPaths(root: Path): Seq[Path] = Process(Seq("find", root.toString, "-name", "*.scala", "-print")).lines map (x => path(x))

  def dump(f: ParseError) {
    println(f.position)
    println(f.formatExpectedAsString)
    println(f.formatTraces)
  }

  def checkNeg[T](input: String) = {
    new ScalaSyntax(input).CompilationUnit.run() match {
      case Failure(f: ParseError) => () // yay
      case Failure(f)             => runtimeException(s"Unexpected failure: $f")
      case Success(parsed)        => assert(parsed != input, parsed)
    }
  }

  def snippet(input: String, parsed: String): String = {
    val offset = parsed.length
    Seq(input take offset, s"[$offset]".red.to_s, input drop offset) mkString ""
  }

  def check[T](input: String) = {
    new ScalaSyntax(input).CompilationUnit.run() match{
      case Success(`input`)       => ()
      case Success(parsed)        => runtimeException(snippet(input, parsed))
      case Failure(f: ParseError) => runtimeException(f.position + "\t" + f.formatTraces)
      case Failure(f)             => runtimeException(s"Unexpected failure: $f")
    }
  }

  def checkPath(root: Path, f: Path): Boolean = {
    val input    = f.slurp()
    val fs       = f.toString
    val segments = (fs splitChar '/').toSet
    val path_s = fs stripPrefix root.toString stripPrefix "/" match {
      case s if s.length <= maxFileLen => s
      case s                           => "..." + (s takeRight maxFileLen - 3)
    }
    val isNeg  = segments("neg")
    val isSkip = (skipPaths exists fs.endsWith) || segments("disabled") || segments("pending") || segments("script")
    val fmt    = s"[%6s] $maxFileFmt  "

    print((s"[%6s] $maxFileFmt  ").format(input.length, path_s))

    if (isSkip) {
      println("skip".yellow.to_s)
      true
    }
    else {
      val result = Try(check(input))
      val passed = result match {
        case Success(_) => !isNeg
        case Failure(_) => isNeg
      }
      println(
        if (passed) "ok".green.to_s
        else result match {
          case Success(_) => "failed".red.to_s + " (expected failure)"
          case Failure(t) => "failed".red.to_s + "\n" + t.getMessage
        }
      )
      passed
    }
  }

  def main(args0: Array[String]): Unit = {
    val args     = ( if (args0.isEmpty) Seq(".") else args0.toSeq ) map (x => path(x))
    val failures = args flatMap (root => scalaPaths(root) filterNot (p => checkPath(root, p)))
    if (failures.nonEmpty) {
      println("\nFailures:")
      failures foreach println
      runtimeException("There were test failures.")
    }
  }
}
