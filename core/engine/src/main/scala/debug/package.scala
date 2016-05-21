import language.experimental.macros
import scala.reflect.macros._

package object debug {

  private[this] class Bundle(val c: blackbox.Context) {

    import c.universe._

    def appliedImpl(prefix: Tree)(name: Tree)(a: Tree*) =
      catchError(macroApply(prefix)(name)(a: _*))

    def showImpl(t: Tree) = q"${c.universe.show(t)}"

    def showRawImpl(t: Tree) = q"${c.universe.showRaw(t)}"

    def showTpeImpl(t: Tree) = q"${c.universe.show(t.tpe)}"

    def parseImpl(code: Tree) = {
      val Literal(Constant(s: String)) = code
      catchError(c.parse(s))
    }

    def macroApply(prefix: Tree)(name: Tree)(a: Tree*) = {
      val Literal(Constant(s: String)) = name
      c.typecheck(q"$prefix.${TermName(s).encodedName.toTermName}(..$a)")
    }

    def catchError(tree: => Tree) = {
      try {
        c.typecheck(tree)
      } catch {
        case e: Throwable =>
          c.typecheck(q"throw new Exception(${e.toString})")
      }
    }

    import scala.reflect.macros.whitebox.Context

    def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._
      annottees match {
        case List(annottee) => annottee.tree match {
          case ddef: DefDef =>
            val mods = Modifiers(ddef.mods.flags | Flag.SYNTHETIC, ddef.mods.privateWithin, ddef.mods.annotations)
            c.Expr[Any](q"$mods def ${ddef.name}[..${ddef.tparams}](...${ddef.vparamss}): ${ddef.tpt} = ${ddef.rhs}")
          case _ =>
            c.abort(c.enclosingPosition, "not suitable annottee:\t" + annottee)
        }
        case _ => c.abort(c.enclosingPosition, "not suitable annottees:\t" + annottees)
      }
    }
  }

  def applied(prefix: Any)(name: String)(a: Any*): Any = macro Bundle.appliedImpl

  def show(t: Any): String = macro Bundle.showImpl

  def showRaw(t: Any): String = macro Bundle.showRawImpl

  def showTpe(t: Any): String = macro Bundle.showTpeImpl

  def parse(code: String): Any = macro Bundle.parseImpl

  private[this] val pat = java.util.regex.Pattern.compile(java.util.regex.Pattern.quote("fresh$macro$") + "\\d+")

  def replaceFreshVariables(code: String): String = pat.matcher(code).replaceAll("\\$\\$").
    replace("scala.this.", "scala."). // change in Scala_2.12.0-M3
    replace("java.this.", "java.")

  def expectException(a: => Any): Boolean = try {
    a
    false
  } catch {
    case e: Exception =>
      true
  }
}

package debug {

  private[this] class WhiteBundle(val c: whitebox.Context) {

    import c.universe._

    def typeErrorImpl(annottees: c.Expr[Any]*): c.Expr[Any] = {
      annottees.flatMap { annottee =>
        try {
          c.typecheck(annottee.tree)
          Nil
        } catch {
          case e: Throwable => List(e.getMessage.replaceAll("\\s+", " "))
        }
      }.headOption match {
        case Some(message) =>
          c.prefix.tree.asInstanceOf[Apply].args.headOption match {
            case Some(tree) =>
              val expected = tree.asInstanceOf[Literal].value.value.asInstanceOf[String]
              c.Expr[Any](q"{info(${s"expectedly type error: $message"}); assert($message === $expected)}")
            case None =>
              c.Expr[Any](q"info(${s"expectedly type error: $message"})")
          }
        case None => c.Expr[Any](q"fail(${"no type error"})")
      }
    }

    def typeableImpl(annottees: c.Expr[Any]*): c.Expr[Any] = {
      val typed = annottees.map(t => c.typecheck(t.tree)).head
      val t1 = replaceFreshVariables(typed.toString.replaceAll("\\s+", " "))
      c.prefix.tree.asInstanceOf[Apply].args.headOption match {
        case Some(tree) =>
          val expected = tree.asInstanceOf[Literal].value.value.asInstanceOf[String]
          val t2 = replaceFreshVariables(expected.replaceAll("\\s+", " "))
          c.Expr[Any](q"{assert($t1 === $t2)}")
        case None =>
          c.Expr[Any](q"info(${s"expectedly typed: $t1"})")
      }
    }
  }

  class typeError extends scala.annotation.StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro WhiteBundle.typeErrorImpl

    def this(expected: String) {
      this()
    }
  }

  class typeable extends scala.annotation.StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro WhiteBundle.typeableImpl

    def this(expected: String) {
      this()
    }
  }

}
