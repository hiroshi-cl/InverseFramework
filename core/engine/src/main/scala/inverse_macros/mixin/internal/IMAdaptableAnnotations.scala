package inverse_macros
package mixin
package internal

trait IMAdaptableAnnotations extends UseContext with Extractors {

  import c.universe._

  def getIMAdaptableAnnotationTrees(tree: Tree): List[Tree] = getAdaptableChildren(tree).flatMap(getAdapters)

  def getAdaptableChildren(tree: Tree): List[Tree] =
    tree match {
      case FineGrainedApply(receiver, _, _, argss) => receiver :: argss.flatten.map(_._1)
      case FineGrainedTypeApply(receiver, _, _) => receiver :: Nil
      case Select(qual, _) => qual :: Nil
      case Return(expr) => expr :: Nil
      case Throw(expr) => expr :: Nil
      case Assign(lhs, rhs) => rhs :: Nil
      case Block(_, expr) => expr :: Nil
      case Function(vparams, body) => body :: Nil
      case If(cond, thenp, elsep) => cond :: thenp :: elsep :: Nil
      case Match(selector, cases) => selector :: cases.map(_.body)
      case Try(block, catches, finalizer) => block +: catches.map(_.body) :+ finalizer
      case Typed(expr, _) => expr :: Nil
      case LabelDef(_, _, rhs) => rhs :: Nil
      case _ => Nil
    }

  def getAdapters(tree: Tree): List[Tree] = getAdapters(tree.tpe)

  def getAdapters(tpe: Type): List[Tree] = tpe match {
    case AnnotatedType(annotations, underlying) =>
      annotations.map(_.tree).filter(_.tpe <:< typeOf[IMAdaptableAnnotation]) ++ getAdapters(underlying)
    case TypeRef(_, _, args) =>
      args.flatMap(getAdapters)
    case _ =>
      Nil
  }
}
