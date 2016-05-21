package inverse_macros
package mixin
package internal

trait ANFTransformers extends UseContext with Detector with Extractors with IMAnnotations {

  import c.universe._
  import c.internal._

  def anfTransform(tree: Tree, api: TypingTransformApi) = anfTransformers((tree, api))

  private[this] type LTr = PartialFunction[(Tree, TypingTransformApi), List[Tree]]

  private[this] lazy val anfTransformers: LTr = Seq[LTr](
    valDef, fineGrainedApply, simpleConstructs, blockConstrucs, miscConstructs, default
  ) reduceLeft (_ orElse _)

  private[this] def defineVal(tree: Tree, api: TypingTransformApi): ValDef = {
    val name = TermName(c.freshName())
    import Flag._
    // c.typecheck: type check & assign symbols
    // api.typecheck: only type check
    val tpe = removeIMAnnotations(tree.tpe)
    val newValdef = c.typecheck(q"@inverse_macros.IMLinearizerSynth ${ARTIFACT | SYNTHETIC} val $name: $tpe = $tree").asInstanceOf[ValDef]
    // change owners of local variables
    c.internal.changeOwner(newValdef, newValdef.symbol.owner, api.currentOwner)
    c.internal.changeOwner(newValdef.rhs, api.currentOwner, newValdef.symbol)
    newValdef
  }

  private[this] def makeRef(valDef: ValDef) =
    gen.mkAttributedRef(valDef.symbol)

  private[this] def annotTrans[T <: Tree](tree: T)(fun: T => T) =
    if (detectAnnotatedTyped(tree))
      fun(tree)
    else
      tree

  private[this] def expand(whole: Tree, sub: Tree, api: TypingTransformApi)(copier: (Tree, RefTree) => Tree) =
    if (detectAnnotatedTyped(sub)) {
      val newDef = defineVal(sub, api)
      val newRef = makeRef(newDef)
      newDef :: copier(whole, newRef) :: Nil
    } else
      whole :: Nil

  private[this] def extend[T <: Tree](tree: T, api: TypingTransformApi) =
    expand(tree, tree, api)((whole, ref) => ref)

  private[this] def retype(tree: Tree): Tree = {
    setType(tree, null)
    setType(tree, c.typecheck(tree).tpe)
    tree
  }

  private[this] lazy val valDef: LTr = {
    case (tree@ValDef(mods, name, tpt, rhs), api) =>
      if (mods.hasFlag(Flag.LAZY) && getIMAnnotations(tpt.tpe).nonEmpty)
        updateAttachment(tree, "Sorry! Impure typed lazy is not supported.\n\t" + tpt) :: Nil
      else if (detectAnnotatedTyped(rhs)) {
        val init :+ last = api.atOwner(tree.symbol)(anfTransform(rhs, api))
        init.foreach(c.internal.changeOwner(_, tree.symbol, api.currentOwner))
        init :+ treeCopy.ValDef(tree, mods, name, TypeTree(tree.symbol.info), last)
      } else
        api.typecheck(tree) :: Nil
  }

  private[this] lazy val fineGrainedApply: LTr = {

    case (tree@FineGrainedApply(receiver, method, targs, argss), api) =>
      if (detectAnnotatedTyped(receiver) || argss.exists(_.exists(p => detectAnnotatedTyped(p._1)))) {
        val receiverList = extend(receiver, api)
        val argListList =
          for (args <- argss) yield
            for ((arg, paramSymbol) <- args) yield
              if (paramSymbol.isByName)
                List(api.recur(arg))
              else
                extend(arg, api)
        receiverList.init ++ argListList.flatMap(_.flatMap(_.init)) :+
          api.typecheck(FineGrainedApply.copy(tree, receiverList.last, method, targs, argListList.map(_.map(_.last))))
      } else
        tree :: Nil

    case (tree@FineGrainedTypeApply(receiver, method, targs), api) =>
      expand(tree, receiver, api)(FineGrainedTypeApply.copy(_, _, method, targs))
  }

  private[this] lazy val simpleConstructs: LTr = {

    case (tree@Select(qual, name), api) =>
      expand(tree, qual, api)(treeCopy.Select(_, _, name))

    case (tree@Return(expr), api) =>
      expand(tree, expr, api)(treeCopy.Return)

    case (tree@Throw(expr), api) =>
      expand(tree, expr, api)(treeCopy.Throw)

    case (tree@Assign(lhs, rhs), api) =>
      expand(tree, rhs, api)(treeCopy.Assign(_, lhs, _))
  }

  // :TODO knf of pat
  private[this] def transformCaseDef(caseDef: CaseDef, api: TypingTransformApi): CaseDef =
    treeCopy.CaseDef(caseDef, caseDef.pat, caseDef.guard, api.recur(caseDef.body))

  // two steps
  // by-value arguments => by-name arguments
  private[this] lazy val blockConstrucs: LTr = {
    case (tree@Block(_, _), api) =>
      api.recur(tree) :: Nil

    case (tree@Function(vparams, body), api) =>
      val func = treeCopy.Function(tree, vparams, api.atOwner(tree.symbol)(api.recur(body)))
      func :: Nil

    case (tree@CaseDef(_, _, _), api) =>
      transformCaseDef(tree, api) :: Nil

    case (tree@If(cond, thenp, elsep), api) =>
      if (detectAnnotatedTyped(cond)) {
        val newDef = defineVal(cond, api)
        val newRef = makeRef(newDef)
        newDef :: retype(treeCopy.If(tree, newRef, thenp, elsep)) :: Nil
      } else {
        val newThenp = annotTrans(thenp)(api.recur)
        val newElsep = annotTrans(elsep)(api.recur)
        val newTree = retype(treeCopy.If(tree, cond, newThenp, newElsep))
        newTree :: Nil
      }

    case (tree@Match(selector, cases), api) =>
      if (detectAnnotatedTyped(selector)) {
        val newDef = defineVal(selector, api)
        val newRef = makeRef(newDef)
        newDef :: retype(treeCopy.Match(tree, newRef, cases)) :: Nil
      } else {
        val newCases = cases.map(annotTrans(_)(transformCaseDef(_, api)))
        val newTree = retype(treeCopy.Match(tree, selector, newCases))
        newTree :: Nil
      }

    case (tree@Try(block, catches, finalizer), api) =>
      val newBlock = annotTrans(block)(api.recur)
      val newCatches = catches.map(annotTrans(_)(transformCaseDef(_, api)))
      val newFinalizer = annotTrans(finalizer)(api.recur)
      if (getIMAnnotations(newFinalizer.tpe).isEmpty) {
        retype(treeCopy.Try(tree, newBlock, newCatches, newFinalizer)) :: Nil
      } else
        updateAttachment(retype(treeCopy.Try(tree, newBlock, newCatches, newFinalizer)),
          "Sorry! Finalyzer in Try support is incomplete.\n\t" + newFinalizer) :: Nil
  }

  private[this] lazy val miscConstructs: LTr = {
    case (tree@Typed(expr, tpt), api) =>
      expand(tree, expr, api)(treeCopy.Typed(_, _, tpt))

    case (tree@LabelDef(name, params, rhs), api) =>
      if (detectAnnotatedTyped(rhs)) {
        rhs match {
          case If(cond, Block(List(body), _), _) =>
            val stats: List[Tree] = body match {
              case Block(stats, expr) => stats :+ expr
              case _ => List(body)
            }
            val newName = TermName(c.freshName("trans" + name.toString))
            // typed in the plugin
            val ddef = c.typecheck(
              q"${Flag.SYNTHETIC} def $newName (): Unit @inverse_macros.IMWhile = if($cond){..$stats; $newName ()}")
            c.internal.setOwner(ddef.symbol, api.currentOwner)
            c.internal.changeOwner(cond, api.currentOwner, ddef.symbol)
            stats.foreach(c.internal.changeOwner(_, api.currentOwner, ddef.symbol))
            ddef :: api.typecheck(gen.mkMethodCall(ddef.symbol, Nil)) :: Nil

          case _ =>
            updateAttachment(treeCopy.LabelDef(tree, name, params, api.recur(rhs)),
              "not supported type of LabelDef.\n\t" + show(rhs).replaceAll("\\s+", " ")) :: Nil
        }
      } else
        tree :: Nil

    // not normalize pattern matches like Unapply
  }

  private[this] lazy val default: LTr = {
    case (t, _) => List(t)
  }
}
