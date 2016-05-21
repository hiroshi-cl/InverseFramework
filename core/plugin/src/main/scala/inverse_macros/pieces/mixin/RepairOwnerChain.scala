package inverse_macros.pieces.mixin

// repair owner chain
trait RepairOwnerChain extends inverse_macros.UseGlobal with inverse_macros.pieces.UseRepairOwnerChain {

  import global._
  import analyzer._
  import scala.tools.nsc.Mode

  var repairOwnerChain = new MacroPlugin with RepairOwnerChainTraverser {
    private[this] def repairOwner(tree: Tree, ans: Symbol): Unit =
      if (phase.id <= currentRun.typerPhase.id && tree.symbol.owner != ans) {
        if (globalSettings.verbose)
          typer.context.echo(tree.pos, s"The owner of ${tree.symbol} is repaired: ${tree.symbol.owner} -> $ans")
        tree.symbol.owner = ans
      }

    private[this] val traverser = new Traverser {
      override def traverse(tree: Tree): Unit = {
        tree match {
          case Template(parents, self, body) =>
            repairOwner(tree, currentOwner)
            traverseParents(parents)
            traverseSelfType(self)
            // traverseStats(body, tree.symbol) // default impl. is not suitable
            traverseStats(body, currentOwner)
          // includes Bind
          case _: DefTree =>
            repairOwner(tree, currentOwner)
            super.traverse(tree)
          case _: Function =>
            repairOwner(tree, currentOwner)
            super.traverse(tree)
          case _ =>
            super.traverse(tree)
        }
      }
    }

    override def pluginsMacroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Option[Tree] = {
      val t = standardMacroExpand(typer, expandee, mode, pt)
      traverser.atOwner(typer.context.owner)(traverser.traverse(t))
      Some(t)
    }

    def repairOwnerChainTraverse(typer: Typer, tree: Tree): Unit = {
      traverser.atOwner(typer.context.owner)(traverser.traverse(tree))
    }
  }

}