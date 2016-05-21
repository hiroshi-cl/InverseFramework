package inverse_macros

import debug.typeError
import org.scalatest.FunSuite

class BasicTransformationTest extends FunSuite {

  def correct1() = {
    val _1 = (): Unit@test1
    @IMFix val _2 = (): Unit@test1
    (): Unit@test1
    ()
  }

  def correct2() = {
    val _1 = (): Unit@test1
    @IMFix val _2 = ()
    ()
    (): Unit@test1
  }

  def correct3() = {
    def _1() = (): Unit@test1
    class _2 {
      val _3 = (): Unit@test1
    }
    (): Unit@test1
  }

  def correct4() =
    (10: Int@test1).toByte // Select

  def correct5() =
    if (true: Boolean@test1) ()

  def correct6() =
    if (true) 10: Int@test1 else 20

  def correct7() = {
    lazy val hoge: Int = ??? : Int@test1
    hoge
  }

  def correct9(a: => Int) =
    if (true) 10: Int@test1 else a

  test("errors") {
    @typeError("an IMAnnotation must be concrete, but @inverse_macros.IMAnnotation is abstract. inverse_macros.IMAnnotation = inverse_macros.test1 & inverse_macros.test2")
    def error6() =
      if (true: Boolean@test1) (): Unit@test1 else (): Unit@test2

    @typeError("Sorry! Impure typed lazy is not supported. Int @inverse_macros.test1")
    def error7() = {
      lazy val hoge = ??? : Int@test1
      hoge
    }

    @typeError("the return type of an impure method must contain an IMAnnotation expected: Unit @inverse_macros.test1 / Unit @inverse_macros.IMWild found : Unit")
    def error8() = {
      def error1(): Unit = (): Unit@test1
      error1()
    }

    @typeError("an IMAnnotation must be concrete, but @inverse_macros.IMAnnotation is abstract. inverse_macros.IMAnnotation = inverse_macros.test1 & inverse_macros.test2")
    def error9(a: => Int@test2) =
      if (true) 10: Int@test1 else a
  }


}
