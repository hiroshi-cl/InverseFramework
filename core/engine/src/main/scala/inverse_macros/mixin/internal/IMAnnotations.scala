package inverse_macros
package mixin
package internal

trait IMAnnotations extends UseContext {
  import c.universe._

  def removeIMAnnotations(tpe: Type) =
    try {
      tpe match {
        case AnnotatedType(annotations, underlying) =>
          val newAnnotations = annotations.filterNot(_.tree.tpe <:< typeOf[IMAnnotation]).map(_.tree.tpe)
          if (newAnnotations.isEmpty)
            underlying
          else
            c.typecheck(tq"$underlying@..$newAnnotations", mode = c.TYPEmode).tpe
        case _ =>
          tpe
      }
    } catch {
      case e: Throwable => throw new Exception("What happened?\t" + tpe, e)
    }


  def getIMAnnotations(tpe: Type) =
    tpe match {
      case AnnotatedType(annotations, _) =>
        annotations.filter(_.tree.tpe <:< typeOf[IMAnnotation]).map(_.tree.tpe)
      case _ =>
        Nil
    }

  def getIMAnnotation(tpe: Type) =
    getIMAnnotations(tpe) match {
      case Nil =>
        definitions.NothingTpe
      case annot :: Nil =>
        annot
      case annots =>
        c.abort(c.enclosingPosition, "Multiple inverse_macros annotation is not allowed: " + annots)
    }

  def getIMAnnotationTree(tpe: Type) =
    tpe match {
      case AnnotatedType(annotations, _) =>
        annotations.filter(_.tree.tpe <:< typeOf[IMAnnotation]) match {
          case annotation :: Nil =>
            annotation.tree
          case _ :: _ =>
            c.abort(c.enclosingPosition, "Multiple inverse_macros annotation is not allowed: " + tpe)
          case _ =>
            c.abort(c.enclosingPosition, "No inverse_macros annotation is detected: " + tpe)
        }
      case _ =>
        c.abort(c.enclosingPosition, "Not annoted: " + tpe)
    }
}
