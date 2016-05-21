package inverse_macros.pieces.mixin

import scala.tools.nsc.Mode

trait ImplicitImport extends inverse_macros.UseGlobal with inverse_macros.pieces.UseSavedTreeExtractor {

  import global._
  import analyzer._

  var implicitImport =
    new global.analyzer.AnalyzerPlugin with global.analyzer.MacroPlugin {
      private[this] val IMImport = rootMirror.staticClass("inverse_macros.IMImport")

      private[this] val transformer = new Transformer {
        override def transform(tree: Tree): Tree =
          if (tree.hasAttachment[Tree])
            tree.attachments.get[Tree].get
          else
            super.transform(tree)
      }

      override val isActive = false

      override def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Mode): Type = {
        if (pt.hasAnnotation(IMImport)) {
          val annot = pt.getAnnotation(IMImport).get
          val annotName = typer.context.unit.freshTermName("at_imimport")
          val annotCompanion = annot.symbol.companionModule
          val annotCompanionImp =
            if (annotCompanion == NoSymbol)
              EmptyTree
            else
              q"import $annotCompanion._"
          val arg = savedTreeExtractor.extract(tree)
          val imported =
            q"val $annotName = ${annot.tree.duplicate.clearType()}; import $annotName._; $annotCompanionImp; $arg"
          val typed = typer.typed(imported, mode, pt.removeAnnotation(IMImport))
          tree.updateAttachment(typed)
          tree.setType(typed.tpe)
        }
        pt
      }

      // for an independent plugin
      override def pluginsMacroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Option[Tree] =
        Some(standardMacroExpand(typer, savedTreeExtractor.extract(expandee), mode, pt))
    }
}