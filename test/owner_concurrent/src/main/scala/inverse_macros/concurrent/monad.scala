package inverse_macros.concurrent

import inverse_macros.{IMAnnotation, IMTransformer}
import scala.language.higherKinds

class monad[+CX] extends IMAnnotation

object monad extends IMTransformer {

  import scala.reflect.macros.blackbox


  override def transform(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
                        (api: c.internal.TypingTransformApi)
                        (head: c.Tree, cont: List[c.Tree]): (List[c.Tree], List[c.Tree]) = {
    import c.internal._
    import c.universe._
    import Flag._

    val ccsym = symbolOf[ConcurrentContext[_]]
    val cpsym = symbolOf[monad[_]]

    val mtp = targs.head

    def decomposeAnn(tp: Type): (Type, List[Type]) = tp match {
      case AnnotatedType(annotations, underlying)
        if annotations.exists(_.tree.tpe.typeConstructor <:< cpsym.toTypeConstructor) =>
        annotations.filter(_.tree.tpe.typeConstructor <:< cpsym.toTypeConstructor) match {
          case List(ann) => underlying -> List(ann.tree.tpe.typeArgs.head)
        }
      case t =>
        t -> Nil
    }

    head match {
      case ValDef(mods, name, tpt, rhs) =>
        c.typecheck(q"@inline $SYNTHETIC def ${TermName(c.freshName(name + ""))} (${TermName(c.freshName(name + ""))} : $tpt) : ${TypeTree(cont.last.tpe)} = ???") match {
          case ddef@DefDef(_, _, _, List(List(param)), _, _) =>
            val paramRef = gen.mkAttributedRef(param.symbol)
            val newHead = treeCopy.ValDef(head, mods, name, tpt, paramRef)
            val newRhs = api.recur(api.typecheck(q"{$newHead; ..$cont}"))
//            c.internal.setOwner(ddef.symbol, api.currentOwner)
            // change owners of local variables
//            c.internal.changeOwner(rhs, head.symbol, api.currentOwner)
//            c.internal.changeOwner(newRhs, api.currentOwner, ddef.symbol)
            // build new defDef
            val func = treeCopy.DefDef(ddef, ddef.mods, ddef.name, ddef.tparams, ddef.vparamss, TypeTree(newRhs.tpe), newRhs)
            setInfo(func.symbol, methodType(List(param.symbol), newRhs.tpe))

            def fun_call(pure: Tree)(impure: Tree => Tree) = {
              val (pureTp, ann) = decomposeAnn(api.typecheck(q"${func.symbol} ($pure)").tpe)
              val impureTp = intersectionType(decomposeAnn(api.typecheck(impure(q"???")).tpe)._2 ++ ann)
              val ex = TermName(c.freshName(ddef.name + "$" + "ex"))
              val tp = c.typecheck(tq"$pureTp@$cpsym[$impureTp]", mode = c.TYPEmode)
              q"${func.symbol} (try {$pure} catch { case $ex: $ccsym[_] => throw new $ccsym(${impure(q"$ex.m")}) }): $tp"
            }

            val (newTp, newAnn) = decomposeAnn(newRhs.tpe)

            val ctlBlock = newAnn match {
              case List(newMtp) =>
                fun_call(rhs)(m => q"$m.asInstanceOf[$mtp].bind(${func.symbol})")
              case _ =>
                fun_call(rhs)(m => q"$m.asInstanceOf[$mtp].fmap(${func.symbol})")
            }

//            List(func, c.internal.changeOwner(c.typecheck(ctlBlock), c.internal.enclosingOwner, api.currentOwner)) -> Nil
            List(func, c.typecheck(ctlBlock)) -> Nil
        }

      case _ =>
        c.typecheck(q"@inline $SYNTHETIC def ${TermName(c.freshName("unit"))} () : ${TypeTree(cont.last.tpe)} = ???") match {
          case ddef@DefDef(_, _, _, _, _, _) =>
            val newRhs = api.recur(api.typecheck(q"{..$cont}"))
//            c.internal.setOwner(ddef.symbol, api.currentOwner)
            // change owners of local variables
//            c.internal.changeOwner(newRhs, api.currentOwner, ddef.symbol)
            // build new defDef
            val f = treeCopy.DefDef(ddef, ddef.mods, ddef.name, ddef.tparams, ddef.vparamss, TypeTree(newRhs.tpe), newRhs)
            setInfo(f.symbol, methodType(Nil, newRhs.tpe))
            val func = c.typecheck(f)

            def fun_call(pure: Tree)(impure: Tree => Tree) = {
              val (pureTp, ann) = decomposeAnn(api.typecheck(q"${func.symbol}").tpe)
              val impureTp = intersectionType(decomposeAnn(api.typecheck(impure(q"???")).tpe)._2 ++ ann)
              val ex = TermName(c.freshName(ddef.name + "$" + "ex"))
              val tp = c.typecheck(tq"$pureTp@$cpsym[$impureTp]", mode = c.TYPEmode)
              List(
                q"try {$pure} catch { case $ex: $ccsym[_] => throw new $ccsym(${impure(q"$ex.m")}) }",
                q"${func.symbol} : $tp"
              )
            }

            val (newTp, newAnn) = decomposeAnn(newRhs.tpe)

            val ctlBlocks = newAnn match {
              case List(newMtp) =>
                fun_call(head)(m => q"$m.asInstanceOf[$mtp].bind(_ => ${func.symbol})")
              case _ =>
                fun_call(head)(m => q"$m.asInstanceOf[$mtp].fmap(_ => ${func.symbol})")
            }

//            (func +: ctlBlocks.map(t => c.internal.changeOwner(c.typecheck(t), c.internal.enclosingOwner, api.currentOwner))) -> Nil
            (func +: ctlBlocks.map(t => c.typecheck(t))) -> Nil
        }
    }
  }
}