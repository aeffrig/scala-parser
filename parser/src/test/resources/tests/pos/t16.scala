object Compiler{

  def apply = {
    def rec = t match {
      case 0 => 0
    }

    rec(tree)
  }
}
