package inverse_macros.typers.mixin

// wildcard effect for while
trait WhileTyper extends inverse_macros.UseGlobal {

  import global._
  import analyzer._
  
  val whileTyper = new AnalyzerPlugin {
    private[this] val IMWhile = rootMirror.getRequiredClass("inverse_macros.IMWhile")

    override def pluginsTypeSig(tpe: Type, typer: Typer, tree: Tree, pt: Type) = {
      var rtpe = tpe
      tree match {
        case ddef: DefDef if pt.hasAnnotation(IMWhile) =>
          rtpe = MethodType(tpe.params, rtpe.finalResultType.removeAnnotation(IMWhile))
          val MAX_REP = 1000
          var cnt = 0
          do {
            ddef.symbol.setInfo(rtpe)
            val rhsTpe = typer.silent {
              silentTyper =>
                typer.context.unit.transformed.getOrElse(
                  ddef.rhs,
                  silentTyper.typed(q"inverse_macros.transform(${ddef.rhs.duplicate})", WildcardType)
                ).tpe.widen
            }.orElse { error =>
              typer.context.error(tree.pos, s"the effect of recursive function ${ddef.name} can not be inferred.")
              ErrorType
            }
            if (rhsTpe.underlying =:= definitions.UnitTpe) {
              rtpe = MethodType(tpe.params, rhsTpe.removeAnnotation(IMWhile))
              if (globalSettings.verbose)
                typer.context.echo(tree.pos, s"The type of an effect of ${tree.symbol} is updated: ${ddef.symbol.info} --> $rtpe")
            } else
              typer.context.error(tree.pos, s"rhs type is incompatible: $rhsTpe -/-> ${ddef.symbol.info}")
            cnt += 1
            if (cnt > MAX_REP)
              typer.context.error(tree.pos, s"the macro expansion may diverge; the repetition limit ($MAX_REP) exceeded")
          } while (!(ddef.symbol.info == rtpe))

          ddef.symbol.setInfo(rtpe)
          val rhsTpe = typer.silent {
            silentTyper =>
              typer.context.unit.transformed.getOrElseUpdate(
                ddef.rhs,
                silentTyper.typed(q"inverse_macros.transform(${ddef.rhs})", WildcardType)
              ).tpe.widen
          }.orElse { error =>
            typer.context.error(tree.pos, s"the effect of recursive function ${ddef.name} can not be inferred.")
            ErrorType
          }
          if (rhsTpe.underlying =:= definitions.UnitTpe) {
            rtpe = MethodType(tpe.params, rhsTpe.removeAnnotation(IMWhile))
            if (globalSettings.verbose)
              typer.context.echo(tree.pos, s"The type of an effect of ${tree.symbol} is inferred: () --> $rtpe")
          } else
            typer.context.error(tree.pos, s"rhs type is incompatible: $rhsTpe -/-> ${ddef.symbol.info}")

        case _ =>
      }
      rtpe
    }
  }
}