package inverse_macros.internal

object invmacs_options {
  val longNames = Set[String]("engine", "mock-engine", "prepend-any", "repait-owner-chain")
  val shortNames = Map[Char, String](
    'e' -> "engine",
    'm' -> "mock-engine",
    'a' -> "prepend-any",
    'o' -> "repair-owner-chain",
    'w' -> "wildcard-effect"
  )
  val default = List("e", "M", "a", "o")

  def apply(options: List[String], error: (String) => Unit) = {
    val processed =
      for (option <- options) yield
        if (option.length == 1) {
          val c = option(0)
          if (!c.isLetter)
            error(s"$c is not a letter.")
          val optKind = c.toLower
          val optOn = c.isLower
          if (!shortNames.contains(optKind))
            error(s"$c is not a recognizable option.")
          shortNames(optKind) -> optOn
        } else {
          val optOn = !option.startsWith("no-")
          val optKind =
            if (optOn)
              option
            else
              option.substring(3)
          if (!longNames.contains(optKind))
            error(s"$optKind is not a recognizable option.")
          optKind -> optOn
        }
    Map(processed: _*)
  }

  def help = ""

  def di(plugin: inverse_macros.Plugin, map: Map[String, Boolean]): Unit = {
    import plugin._
    import global._
    import analyzer._
    if(map.getOrElse("mock-engine", false))
      inverseMacrosEngine = mockInverseMacrosEngine
    if(!map.getOrElse("prepend-any", true))
      prependAny = new AnalyzerPlugin {}
    if(!map.getOrElse("repair-owner-chain", true))
      repairOwnerChain = new MacroPlugin with RepairOwnerChainTraverser {
        def repairOwnerChainTraverse(typer: Typer, tree: Tree) {}
      }
    if(!map.getOrElse("wildcard-effect", true))
      wildCardEffect = new AnalyzerPlugin {}
  }
}
