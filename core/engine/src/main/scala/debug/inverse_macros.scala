package debug

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object inverseMacroEngine {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    annottees match {
      case List(annottee) => annottee.tree match {
        case ddef: DefDef =>
          val mods = Modifiers(ddef.mods.flags | Flag.SYNTHETIC, ddef.mods.privateWithin, ddef.mods.annotations)
          c.Expr[Any](q"$mods def ${ddef.name}[..${ddef.tparams}](...${ddef.vparamss}): ${ddef.tpt} = ${ddef.rhs}")
        case _ =>
          c.abort(c.enclosingPosition, "not suitable annottee:\t" + annottee)
      }
      case _ => c.abort(c.enclosingPosition, "not suitable annottees:\t" + annottees)
    }
  }
}

@scala.annotation.compileTimeOnly("unexpanded inverseMacroEngine remaining")
class inverseMacroEngine extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro inverseMacroEngine.impl
}