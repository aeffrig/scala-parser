object O{
  val jarFile =
      try { 1 }
      catch { case _: F => G }
}
