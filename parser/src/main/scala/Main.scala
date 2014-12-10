package psp
package parser

import org.parboiled2._
import psp.std._, pio._

object Main {
  def parse(path: Path) = {
    val input  = path.slurp()
    val syntax = newScalaParser(input)
    import syntax._

    CompilationUnit.run() match {
      case scala.util.Success(_)               => println(s"[ok] $path")
      case scala.util.Failure(err: ParseError) => println("[fail] " + failMessage(path, err))
      case scala.util.Failure(e)               => println("Unexpected error during parsing run: " + e)
    }
  }

  def main(args: Array[String]): Unit = args flatMap (x => paths(x)) foreach parse
}
