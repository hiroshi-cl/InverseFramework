package inverse_macros

import debug.typeError
import org.scalatest.FunSuite

class AnnotationAdaptationTest extends FunSuite {
  test("errors") {
    @typeError("the return type of an impure method must contain an IMAnnotation expected: Unit @inverse_macros.test1 / Unit @inverse_macros.IMWild found : Unit")
    def error1(): Unit = (): Unit@test1 // adaptation problem

    @typeError("type mismatch; found : Unit @inverse_macros.test2gen[Any] required: Unit @inverse_macros.test2gen[String]")
    def error3(): Unit@test2gen[String] = (): Unit@test2gen[Any]

    @typeError("a term type can contain at most one IMAnnotation, but 2 found. found: inverse_macros.test2gen[Any], inverse_macros.test1")
    def error4(): Unit@test1 @test2gen[Any] = (): Unit

    @typeError("a term type can contain at most one IMAnnotation, but 2 found. found: inverse_macros.test2gen[Any], inverse_macros.test1")
    def error5(): Unit = (): Unit@test1 @test2gen[Any]

    @typeError("a term type can contain at most one IMAnnotation, but 2 found. found: inverse_macros.test2gen[Any], inverse_macros.test1")
    def error6(): Unit@test1 @test2gen[Any] = (): Unit@test1
  }

  def correct1(): Unit@test1 = (): Unit@test1

  def correct2() = (): Unit@test1 // no adaptation

  def correct3(): Unit@test1 = () // no transform, no adaptation

  def correct4() = (): Unit@unchecked // non-related annotation

  def correct5(): Unit = {
    correct1() // Unit@IMAnnotation
    ()
  }

  def correct6(): Unit@test2gen[Any] = (): Unit@test2gen[String]

  def correct7(): Unit@test1 = (): Unit@test1 @unchecked

  def correct8(): Unit@unchecked @unchecked = ()

  // legal, but we do not recommend
  def correct9(): Unit@test1 = (): Unit@test1ex
}
