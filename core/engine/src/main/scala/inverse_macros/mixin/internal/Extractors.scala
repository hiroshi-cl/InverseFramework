package inverse_macros
package mixin
package internal

trait Extractors extends UseContext {
  import c.universe._
  import c.internal._

  // by-name (!attention! "&&" and "||" do not contains by-name flag!)
  final case class IsByName(value: Boolean)

  implicit class GetIsByName(symbol: Symbol) {
    def isByName =
      if (symbol == NoSymbol)
        false
      else
        attachments(symbol).get[IsByName].getOrElse {
          c.warning(symbol.pos, "by-name flag undefined: " + symbol)
          IsByName(false)
        }.value
  }

  // copy and modify from TreeInfo in the scala compiler
  final class FineGrainedApply(val tree: Apply) {

    val callee: Tree = {
      def loop(tree: Tree): Tree = tree match {
        case Apply(fn, _) => loop(fn)
        case t => t
      }
      loop(tree)
    }

    val tapply: FineGrainedTypeApply = new FineGrainedTypeApply(callee)

    val argss: List[List[Tree]] = {
      def loop(tree: Tree): List[List[Tree]] = tree match {
        case Apply(fn, args) => loop(fn) :+ args
        case _ => Nil
      }
      loop(tree)
    }

    def isBooleanSC(symbol: Symbol): Boolean = {
      val and = definitions.BooleanTpe.member(TermName("&&").encodedName)
      val or = definitions.BooleanTpe.member(TermName("||").encodedName)
      if (symbol == and || symbol == or)
        true
      else
        false
    }

    // by-name (!attention! "&&" and "||" do not contains by-name flag!)
    for (paramList <- tapply.core.tpe.paramLists)
      for (paramSymbol <- paramList)
        if (isBooleanSC(callee.symbol))
          updateAttachment(paramSymbol, IsByName(true))
        else
          updateAttachment(paramSymbol, IsByName(paramSymbol.asTerm.isByNameParam))
  }

  object FineGrainedApply {
    def unapply(tree: Tree): Option[(Tree, TermName, List[Tree], List[List[(Tree, Symbol)]])] =
      tree match {
        case t@Apply(_, _) =>
          val applied = new FineGrainedApply(t)
          val tapply = applied.tapply
          Some((tapply.receiver, tapply.method, tapply.targs,
            applied.argss.zip(tapply.core.tpe.paramLists).map(p => p._1.zipAll(p._2, EmptyTree, NoSymbol))))
        case _ =>
          None
      }

    def copy(tree: Tree, receiver: Tree, method: TermName,
             targs: List[Tree], argss: List[List[Tree]]): Tree = {

      def applyChain(t: Tree, a: List[List[Tree]]): Tree = t match {
        case Apply(fn, _) =>
          treeCopy.Apply(t, applyChain(fn, a.init), a.last)
        case _ =>
          FineGrainedTypeApply.copy(t, receiver, method, targs)
      }

      applyChain(tree, argss).asInstanceOf[Apply]
    }
  }

  final class FineGrainedTypeApply(val tree: Tree) {
    val core: Tree = tree match {
      case TypeApply(fn, _) => fn
      case AppliedTypeTree(fn, _) => fn
      case tree => tree
    }

    val targs: List[Tree] = tree match {
      case TypeApply(_, args) => args
      case AppliedTypeTree(_, args) => args
      case _ => Nil
    }

    val receiver: Tree = core match {
      case Select(r, _) => r
      case Ident(_) => EmptyTree
      case _ =>
        c.abort(c.enclosingPosition, "Invalid apply tree: " + tree)
    }

    val method: TermName = core match {
      case Select(_, m) => m.toTermName
      case Ident(m) => m.toTermName
      case _ =>
        c.abort(c.enclosingPosition, "Invalid apply tree: " + tree)
    }
  }


  object FineGrainedTypeApply {
    def unapply(tree: Tree): Option[(Tree, TermName, List[Tree])] =
      tree match {
        case TypeApply(_, _) | AppliedTypeTree(_, _) =>
          val tapply = new FineGrainedTypeApply(tree)
          Some((tapply.receiver, tapply.method, tapply.targs))
        case _ =>
          None
      }

    def copy(tree: Tree, receiver: Tree, method: TermName, targs: List[Tree]): Tree = {

      def tapplyChain(t: Tree): Tree = t match {
        case TypeApply(fn, _) =>
          treeCopy.TypeApply(t, core(fn), targs)
        case AppliedTypeTree(fn, _) =>
          treeCopy.AppliedTypeTree(t, core(fn), targs)
        case fn => core(fn)
      }

      def core(t: Tree): Tree = t match {
        case Select(_, _) => treeCopy.Select(t, receiver, method)
        case Ident(_) => treeCopy.Ident(tree, method)
        case _ =>
          c.abort(c.enclosingPosition, "Invalid type apply tree: " + tree)
      }

      tapplyChain(tree)
    }
  }

}
