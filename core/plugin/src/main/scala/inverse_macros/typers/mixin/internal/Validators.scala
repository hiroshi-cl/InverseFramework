package inverse_macros.typers.mixin.internal

trait Validators extends inverse_macros.UseGlobal {

  import global._
  import analyzer._

  object validator {
    private[this] val IMAnnotation = rootMirror.staticClass("inverse_macros.IMAnnotation")
    private[this] val wildMarker = AnnotationInfo.marker(rootMirror.staticClass("inverse_macros.IMWild").toType)

    def getIMAnnotation(tpe: Type) = tpe.getAnnotation(IMAnnotation).get

    def vinform(message: String) =
      if (global.settings.verbose)
        inform(message)

    def errorEx(message: String, pos: Position = NoPosition)(types: Type*): Unit =
      throw new TypeError(pos, message +
        s"\n" +
        s"\texpected: ${types.init.mkString(" / ")}\n" +
        s"\tfound   : ${types.last}")

    def error(message: String, pos: Position = NoPosition): Unit =
      throw new TypeError(pos, message)

    // rough checking
    def checkConform(tpe1: Type, tpe2: Type): Boolean = {
      val annots1 = tpe1.getAnnotation(IMAnnotation)
      val annots2 = tpe2.getAnnotation(IMAnnotation)
      // impossible to compare underlyings here
      val result = annots1.isEmpty || annots2.isEmpty ||
        annots1.forall(a1 => annots2.forall(a2 => a1.atp matchesPattern a2.atp))
      if (!result)
        inform(currentUnit.source.path)
      if (annots1.nonEmpty || annots2.nonEmpty)
        vinform(s"check annotations: $tpe1 ${if (result) "<:<" else "</<"} $tpe2")

      result
    }

    def checkConcreteAnnotation(atp: Type)(annots: AnnotationInfo*) =
      if (atp.typeSymbol.isAbstract)
        error(s"an IMAnnotation must be concrete, but @$atp is abstract.\n" +
          s"\t$atp = ${annots.mkString(" & ")}")

    def checkPureVariable(vdef: ValDef, pt: Type, typer: Typer) =
      if (pt.hasAnnotation(IMAnnotation) && !typer.context.unit.transformed.contains(vdef.rhs)) // pt is not inferred
        error(s"a variable type must be pure, but @${getIMAnnotation(pt)} found", vdef.pos)

    def checkSingleAnnotation(tpe: Type, kind: String, pos: Position) =
      if (tpe.annotations.count(_ matches IMAnnotation) > 1) {
        val annots = tpe.annotations.filter(_ matches IMAnnotation)
        error(s"$kind can contain at most one IMAnnotation, but ${annots.length} found.\n" +
          s"\tfound: ${annots.mkString(", ")}", pos)
      }

    def validateDefDef(tree: Tree) = tree match {
      case ddef: DefDef if !isPastTyper =>
        val res = ddef.symbol.info.finalResultType
        val rhs = if (ddef.rhs.isTyped) ddef.rhs.tpe else NoType
        // includes overriding abstract methods in trait
        val overridden = ddef.symbol.nextOverriddenSymbol
        val pos = ddef.pos

        if (res.hasAnnotation(IMAnnotation)) {
          val resAnn = getIMAnnotation(res)
          val resRM = res.removeAnnotation(IMAnnotation)
          if (rhs.hasAnnotation(IMAnnotation)) {
            val rhsAnn = getIMAnnotation(rhs)
            // cannot check more precisely...
            if (!(rhsAnn.atp.typeConstructor <:< resAnn.atp.typeConstructor))
              errorEx("the return type is incompatible with the body type about an IMAnnotation", pos)(
                resRM.withAnnotation(rhsAnn),
                resRM.withAnnotation(wildMarker),
                res)
          }

          // ddef.symbol.isOverride drops overrides defined in traits
          if (overridden != NoSymbol) {
            val overRes = overridden.info.finalResultType
            if (overRes.hasAnnotation(IMAnnotation)) {
              val overResAnn = getIMAnnotation(overRes)
              // cannot check more precisely...
              if (!(resAnn.atp.typeConstructor <:< overResAnn.atp.typeConstructor))
                errorEx("an overriding impure method must contain a compatible IMAnnotation", pos)(
                  resRM.withAnnotation(overResAnn), res)
            } else if (!overRes.typeSymbol.isParameter) // this filter allows unsoundness, but required for convenience
              errorEx("an impure method cannot override a pure method", pos)(resRM, res)
          }
        } else if (rhs.hasAnnotation(IMAnnotation))
          errorEx(s"the return type of an impure method must contain an IMAnnotation", pos)(
            res.withAnnotation(getIMAnnotation(rhs)), res.withAnnotation(wildMarker), res)

      case _ =>
    }

    def validateApply(tree: Tree) =
      tree match {
        case Apply(fun, args) if !isPastTyper =>
          if (fun.symbol == definitions.Boolean_and || fun.symbol == definitions.Boolean_or) {
            val argRes = args.head.tpe
            if (args.head.isTyped && argRes.hasAnnotation(IMAnnotation)) {
              errorEx(s"an impure argument cannot be passed to ${fun.symbol}",
                args.head.pos)(definitions.BooleanTpe, argRes)
            }
          } else {
            // fun.tpe != null will be failed in SuperAccessors
            treeInfo.foreachMethodParamAndArg(fun.tpe.params, args) { (param, arg) =>
              if (arg.isTyped)
                if (param.isByNameParam) {
                  val TypeRef(_, _, List(p)) = param.info
                  val a = arg.tpe
                  if (!p.hasAnnotation(IMAnnotation) && a.hasAnnotation(IMAnnotation)) {
                    val treeInfo.Applied(m, _, _) = tree
                    errorEx(s"an impure argument cannot be passed to a pure by-name parameter `${param.name}` of ${m.symbol}",
                      arg.pos)(param.info, TypeRef(NoPrefix, definitions.ByNameParamClass.asType, List(a)))
                  }
                  if (!checkConform(a, p)) {
                    val treeInfo.Applied(m, _, _) = tree
                    errorEx(s"an impure argument cannot be passed to an incompatible impure by-name parameter `${param.name}` of ${m.symbol}",
                      arg.pos)(p, a)
                  }
                  if (!typeRefMatch(p, a, arg.pos)) {
                    val treeInfo.Applied(m, _, _) = tree
                    errorEx(s"an impure object argument cannot be passed to a pure by-name object parameter `${param.name}` of ${m.symbol}",
                      arg.pos)(p, a)
                  }
                } else {
                  val p = param.info match {
                    case TypeRef(_, sym, List(tpe)) if sym == definitions.RepeatedParamClass => tpe
                    case tpe => tpe
                  }
                  val a = arg.tpe
                  if (!typeRefMatch(p, a, arg.pos)) {
                    val treeInfo.Applied(m, _, _) = tree
                    errorEx(s"an impure object argument cannot be passed to a pure object parameter `${param.name}` of ${m.symbol}",
                      arg.pos)(p, a)
                  }
                }
            }
          }
        case _ =>
      }

    def typeRefMatch(expected: Type, found: Type, pos: Position): Boolean = expected match {
      case TypeRef(_, esym, eargs) if eargs.nonEmpty =>
        found.baseType(expected.typeSymbol) match {
          case TypeRef(_, _, fargs) =>
            esym.typeParams.zip(eargs.zip(fargs)).forall { t =>
              val (param, (earg, farg)) = t
              val res =
                if (param.isContravariant) {
                  if (earg.hasAnnotation(IMAnnotation))
                    errorEx(s"a covariant type argument `${param.name}` of ${expected.typeSymbol} cannot be impure in a parameter type",
                      pos)(earg.removeAnnotation(IMAnnotation), earg)
                  if (farg.hasAnnotation(IMAnnotation))
                    errorEx(s"a covariant type argument `${param.name}` of ${found.typeSymbol} cannot be impure in an arugument type",
                      pos)(farg.removeAnnotation(IMAnnotation), farg)
                  true
                } else
                  !farg.hasAnnotation(IMAnnotation) || earg.hasAnnotation(IMAnnotation) || earg.typeSymbol.isTypeParameter

              res && typeRefMatch(earg, farg, pos)
            }

          // TODO: is this correct?
          case ExistentialType(_, TypeRef(_, _, fargs)) =>
            esym.typeParams.zip(eargs.zip(fargs)).forall { t =>
              val (param, (earg, farg)) = t
              val res =
                if (param.isContravariant) {
                  if (earg.hasAnnotation(IMAnnotation))
                    errorEx(s"a covariant type argument `${param.name}` of ${expected.typeSymbol} cannot be impure in a parameter type",
                      pos)(earg.removeAnnotation(IMAnnotation), earg)
                  if (farg.hasAnnotation(IMAnnotation))
                    errorEx(s"a covariant type argument `${param.name}` of ${found.typeSymbol} cannot be impure in an arugument type",
                      pos)(farg.removeAnnotation(IMAnnotation), farg)
                  true
                } else
                  !farg.hasAnnotation(IMAnnotation) || earg.hasAnnotation(IMAnnotation) || earg.typeSymbol.isTypeParameter

              res && typeRefMatch(earg, farg, pos)
            }

          case _ => // maybe null / Nothing
            true
        }
      case _ =>
        true
    }
  }
}