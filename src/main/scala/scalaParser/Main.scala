package scalaParser

import org.parboiled2._
import psp.std._, api._

object Main {
  def parse(path: Path) = {
    val input  = path.slurp()
    val syntax = new ScalaSyntax(input)
    syntax.CompilationUnit.run() match {
      case scala.util.Success(result)        => println(result)
      case scala.util.Failure(e: ParseError) => println("Expression is not valid: " + syntax.formatError(e))
      case scala.util.Failure(e)             => println("Unexpected error during parsing run: " + e)
    }
  }

  def main(args: Array[String]): Unit = args foreach (arg => parse(path(arg)))
}
