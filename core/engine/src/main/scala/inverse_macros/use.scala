package inverse_macros

trait UseContext {
  val c: scala.reflect.macros.whitebox.Context
}

trait UseMirror {
  val cm: scala.reflect.runtime.universe.Mirror
}

trait UseTransformer extends UseContext {

  import c.universe._
  import c.internal._

  trait Transformer {
    def transform(tree: Tree): Tree

    def enumerate(tree: Tree, api: TypingTransformApi): List[Tree]

    def detect(list: List[Tree], api: TypingTransformApi): List[Tree]

    def normalize(list: List[Tree], api: TypingTransformApi): List[Tree]

    def adapt(tree: Tree, api: TypingTransformApi): Tree

    def invoke(tpe: Type, head: Tree, cont: List[Tree], api: TypingTransformApi): (List[Tree], List[Tree])

    def recur(newHead: List[Tree], newCont: List[Tree], api: TypingTransformApi): List[Tree]
  }

  def transformer: Transformer
}

trait AbstractBundle extends UseContext with UseMirror with UseTransformer