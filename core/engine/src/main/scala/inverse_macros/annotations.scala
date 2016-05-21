package inverse_macros

import scala.annotation.{Annotation, TypeConstraint, StaticAnnotation}

trait IMAnnotation extends StaticAnnotation with TypeConstraint

trait IMTransformer {

  import scala.reflect.macros.blackbox

  def transform(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
               (api: c.internal.TypingTransformApi)
               (head: c.Tree, cont: List[c.Tree]): (List[c.Tree], List[c.Tree]) =
    List(head) -> cont
}

trait IMAdaptableAnnotation extends IMAnnotation

trait IMAdapter extends IMTransformer {

  import scala.reflect.macros.blackbox

  def adapt(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
               (api: c.internal.TypingTransformApi)
               (tree: c.Tree): c.Tree = tree
}

class IMLinearizerSynth extends Annotation

// internal use
class IMEngineApplied extends Annotation

// internal use
class IMAnnotated extends Annotation

class IMWild extends Annotation with TypeConstraint

class IMWhile extends Annotation with TypeConstraint

class IMFix extends Annotation

class IMImport extends StaticAnnotation with TypeConstraint

class IMMethod extends StaticAnnotation with TypeConstraint