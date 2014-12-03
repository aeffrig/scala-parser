package object scalaParser {
  val any2stringadd = null

  type HList = shapeless.HList

  def doto[A](value: A)(f: A => Unit): A = { f(value) ; value }
}
