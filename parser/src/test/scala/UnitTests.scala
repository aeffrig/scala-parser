package psp
package parser
package tests

import psp.std._
import scala.Console
import org.expecty._

/** We're not really using the power of expecty yet, but I expect it
 *  to be nice once there's an AST.
 */
object UnitTests {
  val expect = new Expecty()

  def resourcesIn(root: String) = resourceNames(path(root)) map (name => (name, resourceString(s"$root/$name")))

  lazy val posTests = resourcesIn("tests/pos")
  lazy val negTests = resourcesIn("tests/neg")

  def csvTest() {
    val code   = resourceString("embed-csv.scala")
    val result = newScalaParser(code).parseAll()
    assert(result.isSuccess)
  }

  def fprint(msg: String): Unit = {
    Console.print(msg)
    Console.flush()
  }

  def runPos(): Unit = {
    fprint(s"${posTests.size} positive tests: ")
    for ((name, code) <- posTests) {
      val parser = newScalaParser(code)
      val result = parser.parseAll()
      expect {
        (name, result.isSuccess) == (name, true)
      }
      fprint(".")
    }
    fprint("\n")
  }

  def runNeg(): Unit = {
    fprint(s"${negTests.size} negative tests: ")
    for ((name, code) <- negTests) {
      val parser = newScalaParser(code)
      val result = parser.parseAll()
      expect {
        !result.isSuccess
      }
      if (isDebug)
        println(result)

      fprint(".")
    }
    fprint("\n")
  }

  def main(args: Array[String]): Unit = {
    println("Run 'sbt real' to test against all scala sources discoverable via the real/ subdirectory.")
    runPos()
    runNeg()
  }
}
