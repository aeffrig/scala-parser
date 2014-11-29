package scalaParser

import org.parboiled2.ParseError
import utest._
import utest.framework.Test
import utest.util.Tree
import psp.std.{ assert => _, _}
import psp.std.api._
import scala.util.{Failure, Success}
import psp.std.ansi._
import scala.sys.process.Process

object SyntaxTest extends TestSuite {
  import Predef.{ augmentString => _, wrapString => _, ArrowAssoc => _, _ }

  private def tryOpt[A](body: => Option[A]): Option[A] = Try(body) | None

  lazy val skipPaths: Set[String] = resourceString("skip.txt").trim.lines.toSet

  def maxFileLen            = 100
  def maxFileFmt            = "%-" + maxFileLen + "s"
  def scalaSources          = "."
  def scalaPaths: Seq[Path] = Process(s"find $scalaSources -name *.scala -print").lines map (x => path(x))

  def dump(f: ParseError) {
    println(f.position)
    println(f.formatExpectedAsString)
    println(f.formatTraces)
  }

  def checkNeg[T](input: String) = {
    new ScalaSyntax(input).CompilationUnit.run() match {
      case Failure(f: ParseError) => () // yay
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
    }
  }
  def tests = TestSuite {
    val failures = Vector.newBuilder[Path]
    'sources {
      scalaPaths foreach { f =>
        val input = f.slurp()
        val fs = f.toString
        val segments = (fs splitChar '/').toSet
        val path_s = fs stripPrefix (scalaSources + "/") match {
          case s if s.length <= maxFileLen => s
          case s                           => "..." + s.substring(s.length - (maxFileLen - 3), s.length)
        }
        val isNeg = segments("neg")
        val skip  = (skipPaths exists fs.endsWith) || segments("disabled") || segments("pending") || segments("script")
        val fmt   = s"[%6s] $maxFileFmt  "
        print((s"[%6s] $maxFileFmt  ").format(input.length, path_s))
        println(
          if (skip) "skip".yellow.to_s
          else Try(check(input)) match {
            case Success(_) if isNeg => failures += f ; "failed".red.to_s + " (expected failure)"
            case Success(_)          => "ok".green.to_s
            case Failure(_) if isNeg => "ok".green.to_s
            case Failure(t)          => failures += f ; "failed".red.to_s + "\n" + t.getMessage
          }
        )
      }
      println("\nFailures:")
      val failed = failures.result
      failed foreach println
      assert(failed.isEmpty, "There were test failures.")
    }
  }
}
