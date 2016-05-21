package inverse_macros

import internal._

class Plugin(override val global: scala.tools.nsc.Global) extends AbstractPlugin
  with mixin.ParadisePlugins
  with mixin.InverseMacrosPlugins
  with mixin.InverseMacrosEngine
  with typers.mixin.All
  with pieces.mixin.All {

  val name = "invmacs"
  val description = "Inverse macros, macro paradise, etc."
  val components = Nil

  override val optionsHelp = Some(invmacs_options.help)

  override def init(options: List[String], error: (String) => Unit): Boolean = {
    val map = invmacs_options(options, error)
    if (!map.getOrElse("engine", true)) {
      global.analyzer.addAnalyzerPlugin(AnalyzerPlugin)
      global.analyzer.addMacroPlugin(MacroPlugin)
    } else {
      global.addAnnotationChecker(inverseMacrosAnnotationChecker)
      global.analyzer.addAnalyzerPlugin(inverseMacrosAnalyzerPlugin)
      global.analyzer.addMacroPlugin(inverseMacrosMacroPlugin)
      if (map.nonEmpty)
        invmacs_options.di(this, map)
    }
    true
  }
}
