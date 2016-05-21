package inverse_macros

import scala.reflect.macros.blackbox

class test1 extends IMAnnotation

object test1 extends IMTransformer

class test1ex extends test1

object test1ex extends IMTransformer

class test2 extends IMAnnotation

object test2 extends IMTransformer

class test2gen[+T] extends IMAnnotation

object test2gen extends IMTransformer

class abort extends IMAnnotation

object abort extends IMTransformer {

  import scala.reflect.macros.blackbox

  override def transform(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
                        (api: c.internal.TypingTransformApi)(head: c.Tree, cont: List[c.Tree]) =
    c.abort(c.enclosingPosition, "Abort")
}

class cpsIdent extends IMAnnotation

object cpsIdent extends IMTransformer {

  import scala.reflect.macros.blackbox

  override def transform(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
                        (api: c.internal.TypingTransformApi)
                        (head: c.Tree, cont: List[c.Tree]): (List[c.Tree], List[c.Tree]) = {
    import c.universe._
    import c.internal._
    import Flag._
    head match {
      case ValDef(mods, name, tpt, rhs) =>
        c.typecheck(q"@inline $SYNTHETIC def ${TermName(c.freshName())} (${TermName(c.freshName())} : $tpt) : ${TypeTree(cont.last.tpe)} = ???") match {
          case ddef@DefDef(_, _, _, List(List(param)), _, _) =>
            val paramRef = gen.mkAttributedRef(param.symbol)
            val newHead = treeCopy.ValDef(head, mods, name, tpt, paramRef)
            val newRhs = api.recur(api.typecheck(q"{$newHead; ..$cont}"))
            // change owners of local variables
            c.internal.changeOwner(newHead, newHead.symbol.owner, ddef.symbol)
            c.internal.changeOwner(rhs, head.symbol, api.currentOwner)
            c.internal.changeOwner(newRhs, api.currentOwner, ddef.symbol)
            // build new defDef
            val func = treeCopy.DefDef(ddef, ddef.mods, ddef.name, ddef.tparams, ddef.vparamss, TypeTree(newRhs.tpe), newRhs)
            setInfo(func.symbol, methodType(List(param.symbol), newRhs.tpe))
            List(func, gen.mkMethodCall(func.symbol, List(rhs))) -> Nil
        }

      case _ =>
        c.typecheck(q"@inline $SYNTHETIC def ${TermName(c.freshName())} () : ${TypeTree(cont.last.tpe)} = ???") match {
          case ddef@DefDef(_, _, _, _, _, _) =>
            val newRhs = api.recur(api.typecheck(q"{..$cont}"))
            // change owners of local variables
            c.internal.changeOwner(newRhs, api.currentOwner, ddef.symbol)
            // build new defDef
            val func = treeCopy.DefDef(ddef, ddef.mods, ddef.name, ddef.tparams, ddef.vparamss, TypeTree(newRhs.tpe), newRhs)
            setInfo(func.symbol, methodType(Nil, newRhs.tpe))
            List(func, head, gen.mkMethodCall(func.symbol, Nil)) -> Nil
        }
    }
  }
}

class traverse extends IMAdaptableAnnotation

object traverse extends IMAdapter {
  override def adapt(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
                    (api: c.internal.TypingTransformApi)(tree: c.Tree): c.Tree = {
    import c.universe._
    tree match {
      case q"$prefix.map[..$_]($func)($_)" =>
        func.tpe match {
          case TypeRef(_, _, _ :+ (AnnotatedType(annotations, _)))
            if annotations.exists(_.tree.tpe <:< typeOf[IMAdaptableAnnotation]) =>
            c.typecheck(q"$prefix.traverse($func)")
          case _ =>
            tree
        }
      case _ =>
        tree
    }
  }
}

class importWithCompanion extends IMImport {
  val fromClass = 10
}

object importWithCompanion {
  val fromModule = 10
}

class importWithoutCompanion extends IMImport {
  val fromClass = 10
}
