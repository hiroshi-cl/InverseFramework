package inverse_macros
package mixin
package internal

trait ReturnRemover extends UseContext {
  import c.universe._
  import c.internal._

  // return expressions are typed to Nothing
  // therefore type modification is needed
  def stripLastReturns(tree: Tree): Tree = {
    def transformCaseDef(caseDef: CaseDef): CaseDef = {
      val c = treeCopy.CaseDef(caseDef, caseDef.pat, caseDef.guard, stripLastReturns(caseDef.body))
      setType(c, c.body.tpe)
      c
    }

    def retype(tree: Tree): Tree = {
      setType(tree, null)
      setType(tree, c.typecheck(tree).tpe)
      tree
    }

    tree match {
      case Return(expr) => expr
      case Block(stats, expr) =>
        retype(treeCopy.Block(tree, stats, stripLastReturns(expr)))
      case If(cond, thenp, elsep) =>
        retype(treeCopy.If(tree, cond, stripLastReturns(thenp), stripLastReturns(elsep)))
      case Match(selector, cases) =>
        retype(treeCopy.Match(tree, selector, cases.map(transformCaseDef)))
      case Try(block, catches, finalizer) =>
        retype(treeCopy.Try(tree, stripLastReturns(block), catches.map(transformCaseDef), stripLastReturns(finalizer)))
      case _ => tree
    }
  }
}
