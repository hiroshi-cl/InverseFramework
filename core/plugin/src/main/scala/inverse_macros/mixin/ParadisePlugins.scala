package inverse_macros.mixin

import inverse_macros._

trait ParadisePlugins extends org.scalamacros.paradise.typechecker.AnalyzerPlugins
  with UseGlobal
  with UseInverseMacrosEngine {

  import global._
  import analyzer.{MacroPlugin => NscMacroPlugin, Namer => NscNamer, _}

  val paradiseAnalyzerPlugin = AnalyzerPlugin
  val paradiseMacroPlugin = new NscMacroPlugin {
    // modify enterSym
    override def pluginsEnterSym(namer: NscNamer, tree: Tree) = {
      tree match {
        case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) =>
          MacroPlugin.pluginsEnterSym(namer, tree)
        case ddef@DefDef(mods, _, _, _, _, _)
          if ddef.symbol == NoSymbol && !mods.isMacro && !mods.hasAnnotationNamed(TypeName("IMEngineApplied")) && !mods.isSynthetic =>
          val d = ddef.copy(mods = mods.withAnnotations(List(inverseMacrosEngine, q"new inverse_macros.IMEngineApplied()")))
          MacroPlugin.pluginsEnterSym(namer, d)
          // set d.symbol to ddef
          ddef.setSymbol(d.symbol)
        case _ =>
          MacroPlugin.pluginsEnterSym(namer, tree)
      }
      true
    }

    override def pluginsTypedMacroBody(typer: Typer, ddef: DefDef): Option[Tree] =
      MacroPlugin.pluginsTypedMacroBody(typer, ddef)

    override def pluginsEnterStats(typer: Typer, stats: List[Tree]): List[Tree] =
      MacroPlugin.pluginsEnterStats(typer, stats)

    override def pluginsEnsureCompanionObject(namer: NscNamer, cdef: ClassDef, creator: (ClassDef) => Tree): Option[Symbol] =
      MacroPlugin.pluginsEnsureCompanionObject(namer, cdef, creator)
  }
}