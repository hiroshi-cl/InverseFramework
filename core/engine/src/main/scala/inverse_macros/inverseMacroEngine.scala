package inverse_macros

import scala.reflect.macros._
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

@scala.annotation.compileTimeOnly("unexpanded inverseMacroEngine remaining")
class inverseMacroEngine extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro inverseMacroEngine.annotationImpl
}

object inverseMacroEngine {
  def annotationImpl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    annottees match {
      case List(annottee) => annottee.tree match {
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          val newRhs = q"inverse_macros.transform[$tpt](${
            rhs match {
              case Assign(_, _) =>
                // avoid SI-8846: macro system's bug;
                // false positive of detecting named parameter / parameter with a default value
                q"$rhs; ()"
              case _ =>
                rhs
            }
          })"
          c.Expr[Any](q"$mods def $name[..$tparams](...$vparamss): $tpt = $newRhs")
      }
    }
  }
}