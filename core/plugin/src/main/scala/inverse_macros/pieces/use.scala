package inverse_macros
package pieces

trait UsePrependAny extends UseGlobal {
  def prependAny: global.analyzer.AnalyzerPlugin
}

trait UseRepairOwnerChain extends UseGlobal {

  trait RepairOwnerChainTraverser {
    def repairOwnerChainTraverse(typer: global.analyzer.Typer, tree: global.Tree): Unit
  }

  def repairOwnerChain: global.analyzer.MacroPlugin with RepairOwnerChainTraverser
}

trait UseWildCardEffect extends UseGlobal {
  def wildCardEffect: global.analyzer.AnalyzerPlugin
}

trait UseImplicitImport extends UseGlobal {
  def implicitImport: global.analyzer.AnalyzerPlugin with global.analyzer.MacroPlugin
}

trait UseSavedTreeExtractor extends UseGlobal {

  trait SavedTreeExtractor {
    def extract(tree: global.Tree): global.Tree
  }

  def savedTreeExtractor: SavedTreeExtractor
}



trait AbstractPieces
  extends UsePrependAny
    with UseRepairOwnerChain
    with UseWildCardEffect
    with UseImplicitImport
    with UseSavedTreeExtractor