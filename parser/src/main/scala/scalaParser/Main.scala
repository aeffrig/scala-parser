package scalaParser

import org.parboiled2._
import psp.std._, api._

object Main {
  def parse(path: Path) = {
    val input  = path.slurp()
    val syntax = new ScalaSyntax(input)
    syntax.CompilationUnit.run() match {
      case scala.util.Success(`input`)       => println("Parsed %s chars.".format(input.length))
      case scala.util.Success(result)        => println("Failed: parsed %s/%s chars.".format(result.length, input.length))
      case scala.util.Failure(e: ParseError) => println("Expression is not valid: " + syntax.formatError(e))
      case scala.util.Failure(e)             => println("Unexpected error during parsing run: " + e)
    }
  }

  def main(args: Array[String]): Unit = args foreach (arg => parse(path(arg)))
}
