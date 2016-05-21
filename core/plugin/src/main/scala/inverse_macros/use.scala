package inverse_macros

trait UseGlobal {
  val global: scala.tools.nsc.Global
}

trait UseParadisePlugins extends UseGlobal {
  def paradiseAnalyzerPlugin: global.analyzer.AnalyzerPlugin

  def paradiseMacroPlugin: global.analyzer.MacroPlugin
}

trait UseInverseMacrosPlugins extends UseGlobal {
  def inverseMacrosAnnotationChecker: global.AnnotationChecker

  def inverseMacrosAnalyzerPlugin: global.analyzer.AnalyzerPlugin

  def inverseMacrosMacroPlugin: global.analyzer.MacroPlugin
}

trait UseInverseMacrosEngine extends UseGlobal {
  def inverseMacrosEngine: global.Tree
}

trait AbstractPlugin extends scala.tools.nsc.plugins.Plugin
  with UseGlobal
  with UseInverseMacrosPlugins
  with UseParadisePlugins
  with typers.AbstractTypers
  with pieces.AbstractPieces

