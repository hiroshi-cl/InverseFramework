package inverse_macros.mixin

import scala.tools.nsc.Mode

trait InverseMacrosPlugins extends inverse_macros.UseGlobal
  with inverse_macros.UseParadisePlugins
  with inverse_macros.typers.AbstractTypers
  with inverse_macros.pieces.AbstractPieces {

  import global._
  import analyzer._

  val inverseMacrosAnnotationChecker = new AnnotationChecker {
    override def annotationsConform(tpe1: Type, tpe2: Type): Boolean =
      inverseMacrosTyper.annotationsConform(tpe1, tpe2)

    override def annotationsLub(tp: Type, ts: List[Type]): Type =
      inverseMacrosTyper.annotationsLub(tp, ts)

    override def adaptBoundsToAnnotations(bounds: List[TypeBounds], tparams: List[Symbol], targs: List[Type]): List[TypeBounds] =
      inverseMacrosTyper.adaptBoundsToAnnotations(bounds, tparams, targs)
  }

  val inverseMacrosAnalyzerPlugin = new analyzer.AnalyzerPlugin {
    override def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Mode): Type = {
      val pt1 = prependAny.pluginsPt(pt, typer, tree, mode)
      val pt2 = implicitImport.pluginsPt(pt1, typer, tree, mode)
      inverseMacrosTyper.pluginsPt(pt2, typer, tree, mode)
    }

    override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
      val tpe1 = byNameTyper.pluginsTyped(tpe, typer, tree, mode, pt)
      inverseMacrosTyper.pluginsTyped(tpe1, typer, savedTreeExtractor.extract(tree), mode, pt)
    }

    override def pluginsTypeSig(tpe: Type, typer: Typer, defTree: Tree, pt: Type): Type = defTree match {
      case _: Template | _: ClassDef =>
        paradiseAnalyzerPlugin.pluginsTypeSig(tpe, typer, defTree, pt)
      case ddef: DefDef =>
        val sig1 = wildCardEffect.pluginsTypeSig(tpe, typer, defTree, pt)
        whileTyper.pluginsTypeSig(sig1, typer, defTree, pt)
      case vdef: ValDef if !vdef.mods.isLazy =>
        inverseMacrosTyper.pluginsTypeSig(tpe, typer, defTree, pt)
      case _ =>
        tpe
    }
  }

  val inverseMacrosMacroPlugin = new analyzer.MacroPlugin {
    override def pluginsEnterSym(namer: Namer, tree: Tree): Boolean =
      paradiseMacroPlugin.pluginsEnterSym(namer, tree)

    override def pluginsEnterStats(typer: Typer, stats: List[Tree]): List[Tree] = {
      paradiseMacroPlugin.pluginsEnterStats(typer, stats).flatMap { t =>
        t match {
          // "!= null" is dummy. this comparing invokes pluginTypeSig
          case ddef: DefDef if ddef.symbol.info != null && ddef.symbol.hasAttachment[List[Tree]] =>
            ddef :: ddef.symbol.attachments.get[List[Tree]].get
          case _ =>
            List(t)
        }
      }
    }

    override def pluginsEnsureCompanionObject(namer: Namer, cdef: ClassDef, creator: (ClassDef) => Tree): Option[Symbol] =
      paradiseMacroPlugin.pluginsEnsureCompanionObject(namer, cdef, creator)

    override def pluginsTypedMacroBody(typer: Typer, ddef: DefDef): Option[Tree] =
      paradiseMacroPlugin.pluginsTypedMacroBody(typer, ddef)

    override def pluginsMacroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Option[Tree] = {
      val expanded = standardMacroExpand(typer, savedTreeExtractor.extract(expandee), mode, pt)
      inverseMacrosTyper.validate(expandee, expanded)
      repairOwnerChain.repairOwnerChainTraverse(typer, expanded)
      Some(expanded)
    }
  }
}