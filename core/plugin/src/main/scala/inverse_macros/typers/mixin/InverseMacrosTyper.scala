package inverse_macros.typers.mixin

import scala.tools.nsc.Mode

trait InverseMacrosTyper extends inverse_macros.UseGlobal
  with inverse_macros.typers.UseInverseMacrosTyper
  with internal.Validators {

  import global._
  import analyzer._
  import validator._

  val inverseMacrosTyper = new AnnotationChecker with AnalyzerPlugin with Validator {
    private[this] val IMAnnotation = rootMirror.staticClass("inverse_macros.IMAnnotation")
    private[this] val wildMarker = AnnotationInfo.marker(rootMirror.staticClass("inverse_macros.IMWild").toType)

    // rough checking
    override def annotationsConform(tpe1: Type, tpe2: Type): Boolean = checkConform(tpe1, tpe2)

    // if/match/try/...
    override def annotationsLub(tp: Type, ts: List[Type]): Type = {
      val annots1 = tp.getAnnotation(IMAnnotation).toList
      val annots2 = ts.flatMap(_.getAnnotation(IMAnnotation).toList)
      if (annots2.nonEmpty) {
        val newAtp = weakLub((annots1 ++ annots2).map(_.atp))
        checkConcreteAnnotation(newAtp)(annots1 ++ annots2: _*)
        val newTp = tp.removeAnnotation(IMAnnotation).withAnnotation(AnnotationInfo.marker(newAtp))
        vinform("modify lub: " + tp + " -> " + newTp)
        newTp
      } else
        tp
    }

    // remove annotations from a type of a variable
    // this removal does not affect by name parameters because their types are ByNameParamter[T @annot]
    // lazy vals will not come here
    override def pluginsTypeSig(tpe: Type, typer: Typer, defTree: Tree, pt: Type): Type = defTree match {
      case vdef: ValDef if tpe.hasAnnotation(IMAnnotation) =>
        checkPureVariable(vdef, pt, typer)
        vinform(s"remove annotations from ${vdef.symbol.fullName} ($tpe)")
        tpe.removeAnnotation(IMAnnotation)
      case _ =>
        tpe
    }

    override def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Mode): Type = {
      checkSingleAnnotation(pt, "an ascripted type", tree.pos)
      pt
    }

    override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
      checkSingleAnnotation(tpe, "a term type", tree.pos)
      validateDefDef(tree)
      tpe
    }

    // TODO:
    override def adaptBoundsToAnnotations(bounds: List[TypeBounds], tparams: List[Symbol], targs: List[Type]): List[TypeBounds] = {
      //      println(s"bounds = $bounds, tparams = $tparams, targs = $targs")
      bounds
    }

    override def validate(expandee: Tree, expanded: Tree): Unit =
      if (treeInfo.isMacroApplication(expandee))
        expandee match {
          case treeInfo.Applied(fun, _, _) if fun.symbol.fullName == "inverse_macros.transform" =>
            for (t <- expanded if t.isTyped) {
              validateApply(t)
              if (t.hasAttachment[String])
                validator.error(t.attachments.get[String].get, t.pos)
            }
          case _ =>
        }

    override val isActive = false
  }
}