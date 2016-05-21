package inverse_macros.pieces.mixin

// wildcard effect
trait WildCardEffect extends inverse_macros.UseGlobal {

  import global._
  import analyzer._

  var wildCardEffect = new AnalyzerPlugin {
    private[this] val IMWild = rootMirror.getRequiredClass("inverse_macros.IMWild")

    override def pluginsTypeSig(tpe: Type, typer: Typer, tree: Tree, pt: Type) = {
      var rtpe = tpe
      tree match {
        case ddef: DefDef if pt.hasAnnotation(IMWild) =>
          val rhsTpe = typer.silent {
            silentTyper =>
              typer.context.unit.transformed.getOrElseUpdate(
                ddef.rhs,
                silentTyper.typed(ddef.rhs, WildcardType)
              ).tpe.widen
          }.orElse { error =>
            typer.context.error(tree.pos, s"the effect of recursive function ${ddef.name} can not be inferred.")
            ErrorType
          }
          if (rhsTpe.underlying <:< pt.underlying) {
            val newInfo = pt.withAnnotations(rhsTpe.annotations).removeAnnotation(IMWild)
            rtpe = MethodType(tpe.params, newInfo)
            if (globalSettings.verbose)
              typer.context.echo(tree.pos, s"The type with a wildcard effect of ${tree.symbol} is inferenced: $rhsTpe --> $rtpe")
          } else
            typer.context.error(tree.pos, s"rhs type is incompatible: $rhsTpe -/-> ${ddef.symbol.info}")
        case _ =>
      }
      rtpe
    }
  }
}