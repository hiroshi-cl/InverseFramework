package inverse_macros

import debug._
import org.scalatest.FunSuite

class WildCardEffectTest extends FunSuite {
  def _1(): Unit@IMWild = (): Unit@test1

  test("correct") {
    @typeable("@inverse_macros.IMEngineApplied def correct1(): Unit @inverse_macros.test1 = ((): Unit @inverse_macros.test1)")
    def correct1(): Unit@IMWild = (): Unit@test1

    @typeable("@inverse_macros.IMEngineApplied def correct2(): Unit @inverse_macros.IMWild = ((): Unit @inverse_macros.IMWild)")
    def correct2() = (): Unit@IMWild // no adaptation

    @typeable("@inverse_macros.IMEngineApplied def correct3(): Unit = ()")
    def correct3(): Unit@IMWild = () // no transform, no adaptation

    @typeable("@inverse_macros.IMEngineApplied def correct4(): Unit @inverse_macros.IMWild = ((): Unit @inverse_macros.IMWild)")
    def correct4() = (): Unit@IMWild // non-related annotation

    @typeable("@inverse_macros.IMEngineApplied def correct5(): Unit = { WildCardEffectTest.this._1(); () }")
    def correct5(): Unit = {
      _1() // Unit@IMAnnotation
      ()
    }

    @typeable("@inverse_macros.IMEngineApplied def correct6(): Unit @inverse_macros.test2gen[String] = ((): Unit @inverse_macros.test2gen[String])")
    def correct6(): Unit@IMWild = (): Unit@test2gen[String]

    @typeable("@inverse_macros.IMEngineApplied def correct7(): Unit @unchecked @inverse_macros.test1 = ((): Unit @unchecked @inverse_macros.test1)")
    def correct7(): Unit@IMWild = (): Unit@test1 @unchecked

    @typeable("@inverse_macros.IMEngineApplied def correct8(): Unit @unchecked @unchecked = ()")
    def correct8(): Unit@unchecked @unchecked = ()
  }

  test("__") {
    @typeable("@inverse_macros.IMEngineApplied def __1(): Unit @inverse_macros.test1 = ((): Unit @inverse_macros.test1)")
    def __1(): Unit@__ = (): Unit@test1

    @typeable("@inverse_macros.IMEngineApplied def __2(): Unit @inverse_macros.IMWild = ((): Unit @inverse_macros.IMWild)")
    def __2() = (): Unit@__ // no adaptation

    @typeable("@inverse_macros.IMEngineApplied def __3(): Unit = ()")
    def __3(): Unit@__ = () // no transform, no adaptation

    @typeable("@inverse_macros.IMEngineApplied def __4(): Unit @inverse_macros.IMWild = ((): Unit @inverse_macros.IMWild)")
    def __4() = (): Unit@__ // non-related annotation

    @typeable("@inverse_macros.IMEngineApplied def __5(): Unit = { WildCardEffectTest.this._1(); () }")
    def __5(): Unit = {
      _1() // Unit@IMAnnotation
      ()
    }

    @typeable("@inverse_macros.IMEngineApplied def __6(): Unit @inverse_macros.test2gen[String] = ((): Unit @inverse_macros.test2gen[String])")
    def __6(): Unit@__ = (): Unit@test2gen[String]

    @typeable("@inverse_macros.IMEngineApplied def __7(): Unit @unchecked @inverse_macros.test1 = ((): Unit @unchecked @inverse_macros.test1)")
    def __7(): Unit@__ = (): Unit@test1 @unchecked

    @typeable("@inverse_macros.IMEngineApplied def __8(): Unit @unchecked @unchecked = ()")
    def __8(): Unit@unchecked @unchecked = ()
  }
}
