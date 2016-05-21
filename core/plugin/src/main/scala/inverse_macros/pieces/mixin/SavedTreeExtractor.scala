package inverse_macros.pieces.mixin

trait SavedTreeExtractor extends inverse_macros.UseGlobal with inverse_macros.pieces.UseSavedTreeExtractor {

  import global._

  var savedTreeExtractor = new SavedTreeExtractor {
    private[this] val transformer = new Transformer {
      override def transform(tree: Tree): Tree =
        if (tree.hasAttachment[Tree])
          tree.attachments.get[Tree].get
        else
          super.transform(tree)
    }

    override def extract(tree: Tree): Tree = transformer.transform(tree)
  }
}