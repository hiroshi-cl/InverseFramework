package inverse_macros.pieces.mixin

// prepend from inferring to Any
trait PrependAny extends inverse_macros.UseGlobal {

  import global._
  import analyzer._
  import scala.tools.nsc.Mode

  var prependAny = new AnalyzerPlugin {
    private[this] val IMAnnotation = rootMirror.staticClass("inverse_macros.IMAnnotation")
    private[this] val annotatedMarker = AnnotationInfo.marker(rootMirror.staticClass("inverse_macros.IMAnnotated").toType)

    override def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Mode): Type =
      if (pt =:= global.definitions.AnyTpe && !tree.isDef)
        WildcardType
      else
      // more precisely...
        tree match {
          // PartialFunction requires an expected argument type
          case Match(_, _) if pt.typeConstructor =:= definitions.PartialFunctionClass.toTypeConstructor =>
            pt
          case Match(EmptyTree, _) =>
            pt
          // val <pat> = <rhs>
          case Match(_, List(kase)) =>
            typer.context.tree match {
              case ValDef(mods, name, _, rhs@Match(_, List(CaseDef(_, EmptyTree, Ident(name1)))))
                if rhs == tree && name == name1 =>
                pt
              case ValDef(mods, name, _, rhs@Match(_, List(CaseDef(_, EmptyTree, Apply(_, elems)))))
                if rhs == tree && elems.forall(_.isInstanceOf[Ident]) =>
                pt
              case _ =>
                if (pt.hasAnnotation(IMAnnotation) && !tree.isTyped && !isPastTyper)
                  WildcardType.withAnnotation(annotatedMarker)
                else
                  WildcardType
            }
          // avoid failing in typing before macro expansion
          case _: Block if !(pt =:= definitions.UnitTpe) =>
            if (pt.hasAnnotation(IMAnnotation) && !tree.isTyped && !isPastTyper)
              pt.removeAnnotation(IMAnnotation).withAnnotation(annotatedMarker)
            else
              pt
          // avoid the short circuit using isFullyDefined
          case _: If | _: Try | _: Match =>
            if (pt.hasAnnotation(IMAnnotation) && !tree.isTyped && !isPastTyper)
              WildcardType.withAnnotation(annotatedMarker)
            else
              WildcardType
          case _ => pt
        }
  }
}