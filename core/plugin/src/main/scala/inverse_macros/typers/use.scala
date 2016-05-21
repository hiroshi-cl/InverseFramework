package inverse_macros
package typers

trait UseInverseMacrosTyper extends UseGlobal {

  trait Validator {
    def validate(expandee: global.Tree, expanded: global.Tree): Unit
  }

  def inverseMacrosTyper: global.AnnotationChecker with global.analyzer.AnalyzerPlugin with Validator
}

trait UseWhileTyper extends UseGlobal {
  def whileTyper: global.analyzer.AnalyzerPlugin
}

trait UseByNameTyper extends UseGlobal {
  def byNameTyper: global.analyzer.AnalyzerPlugin
}

trait AbstractTypers
  extends UseInverseMacrosTyper
    with UseWhileTyper
    with UseByNameTyper

