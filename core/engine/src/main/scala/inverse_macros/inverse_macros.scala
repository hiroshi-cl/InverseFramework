
import scala.reflect.macros._
import scala.language.experimental.macros

package object inverse_macros {

  private[this] class Bundle(val c: whitebox.Context) extends AbstractBundle with mixin.Transformer {
    import c.universe._

    val cm = scala.reflect.runtime.currentMirror

    def transformImpl(a: Tree): Tree = transformer.transform(a)
  }

  def transform[T](a: T): T = macro Bundle.transformImpl

  @deprecated("Use @IMFix", "3.0")
  type IMSynth = IMFix

  type __ = IMWild
}