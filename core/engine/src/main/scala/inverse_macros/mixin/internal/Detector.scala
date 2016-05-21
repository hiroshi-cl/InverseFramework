package inverse_macros
package mixin
package internal

trait Detector extends UseContext {

  import c.universe._
  import c.internal._

  def detectAnnotatedTyped(tree: Tree): Boolean =
    transform(tree) { (t, api) =>
      t match {
        // for compatibility
        case ValDef(mods, _, _, _)
          if mods.annotations.exists(_.tpe <:< typeOf[IMFix]) || // for untyped trees
            t.symbol != null && t.symbol.annotations.exists(_.tree.tpe <:< typeOf[IMFix]) => // for typed trees
          EmptyTree
        case _: ValDef => api.default(t)
        case _: LabelDef => api.default(t)
        case _: CaseDef => api.default(t)
        case _: MemberDef => EmptyTree
        case _: Ident => api.default(t)
        case _ => api.default(t)
      }
    } exists {
      _.tpe match {
        case AnnotatedType(annotations, underlying) =>
          annotations.exists(_.tree.tpe <:< typeOf[IMAnnotation])
        case _ => false
      }
    }
}
