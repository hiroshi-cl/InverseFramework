package inverse_macros.typers.mixin

import scala.tools.nsc.Mode

trait ByNameTyper extends inverse_macros.UseGlobal {

  import global._
  import analyzer._

  val byNameTyper = new AnalyzerPlugin {
    private[this] val IMAnnotation = rootMirror.staticClass("inverse_macros.IMAnnotation")
    private[this] val IMAnnotated = rootMirror.staticClass("inverse_macros.IMAnnotated")

    // IMAnnotated is added in prependAny
    override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type =
      if (mode.typingExprNotValue && pt.hasAnnotation(IMAnnotated)) {
        val typed = typer.typed(q"inverse_macros.transform[Any](${
          tree match {
            case Assign(_, _) =>
              // avoid SI-8846: macro system's bug;
              // false positive of detecting named parameter / parameter with a default value
              q"${tree.duplicate}; ()"
            case _ =>
              tree.duplicate
          }
        })")
        typed.tpe.removeAnnotation(IMAnnotated)
      } else
        tpe.removeAnnotation(IMAnnotated)
  }
}