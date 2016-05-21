package inverse_macros
package mixin

trait Transformer extends UseContext with UseMirror with UseTransformer
  with internal.IMAnnotations
  with internal.IMAdaptableAnnotations
  with internal.Detector
  with internal.ANFTransformers
  with internal.ReturnRemover {

  import c.universe._
  import c.internal._

  val transformer = new Transformer {

    override def transform(tree: Tree): Tree =
      if (detectAnnotatedTyped(tree)) {
        c.typecheck(typingTransform(stripLastReturns(tree)) { (t, api) =>
          if (t.isEmpty)
            EmptyTree
          else
            api.typecheck(q"{..${enumerate(t, api)}}")
        })
      } else
        tree

    override def enumerate(tree: Tree, api: TypingTransformApi): List[Tree] =
      tree match {
        case EmptyTree => Nil
        case Block(stats, expr) =>
          setType(expr, null)
          setType(expr, c.typecheck(expr).tpe)
          detect((stats :+ expr).filterNot(_.isEmpty), api)
        case _ => detect(tree :: Nil, api)
      }

    override def detect(list: List[Tree], api: TypingTransformApi): List[Tree] =
      if (list.isEmpty)
        Nil
      else
        normalize(list, api) match {
          case Nil => Nil

          // last is not invoked
          case last :: Nil => adapt(last, api) :: Nil

          case (head@ValDef(mods, name, tpt, rhs)) :: cont =>
            val adaptedRhs = adapt(rhs, api)
            val adaptedHead = treeCopy.ValDef(head, mods, name, tpt, adaptedRhs)
            if (getIMAnnotations(adaptedRhs.tpe).nonEmpty) {
              val (hs, ts) = invoke(adaptedRhs.tpe, adaptedHead, cont, api)
              recur(hs, ts, api)
            } else
              adaptedHead :: detect(cont, api)

          // for efficiency (the case discarding the returned value)
          case head :: cont =>
            val adaptedHead = adapt(head, api)
            if (getIMAnnotations(adaptedHead.tpe).nonEmpty) {
              val (hs, ts) = invoke(adaptedHead.tpe, adaptedHead, cont, api)
              recur(hs, ts, api)
            } else
              adaptedHead :: detect(cont, api)
        }

    override def normalize(list: List[Tree], api: TypingTransformApi): List[Tree] =
      list match {
        case head :: cont if detectAnnotatedTyped(head) =>
          anfTransform(head, api) match {
            case Nil => Nil
            case single :: Nil => single :: cont
            case newHead => normalize(newHead ++ cont, api)
          }
        case _ =>
          list
      }

    override def adapt(tree: Tree, api: TypingTransformApi): Tree = {
      var r = tree
      for (adapter <- getIMAdaptableAnnotationTrees(tree)) {
        val q"new ${tpt: Tree}(...${argss: List[List[Tree]]})" = adapter
        // avoid SI-9218
        val fullName = tpt.tpe.typeConstructor.typeSymbol.fullName
        val um = cm.reflectModule(cm.staticModule(fullName)).instance.asInstanceOf[IMAdapter]
        r = um.adapt(c)(tpt.tpe.typeArgs, argss)(api)(r).asInstanceOf[Tree]
      }
      r
    }

    override def invoke(tpe: Type, head: Tree, cont: List[Tree], api: TypingTransformApi) = {
      val q"new ${tpt: Tree}(...${argss: List[List[Tree]]})" = getIMAnnotationTree(tpe)
      // avoid SI-9218
      val fullName = tpt.tpe.typeConstructor.typeSymbol.fullName
      val um = cm.reflectModule(cm.staticModule(fullName)).instance.asInstanceOf[IMTransformer]
      um.transform(c)(tpt.tpe.typeArgs, argss)(api)(head, cont).asInstanceOf[(List[Tree], List[Tree])]
    }

    // cont is getting shorter
    override def recur(newHead: List[Tree], newCont: List[Tree], api: TypingTransformApi) =
      newHead.map(api.typecheck) ++ detect(newCont.map(api.typecheck), api)

  }
}
