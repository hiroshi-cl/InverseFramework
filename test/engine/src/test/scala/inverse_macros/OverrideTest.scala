package inverse_macros

import debug._
import org.scalatest.FunSuite

class OverrideTest extends FunSuite {

  trait Base {
    def annot: Int
  }

  trait Base2 {
    def annot: Int@test2
  }

  class Impl extends Base2 {
    // legal
    override def annot: Int = 20
  }


  test("errors") {
    @typeError("an impure method cannot override a pure method expected: Int found : Int @inverse_macros.test1")
    class Impl1 extends Base {
      def annot: Int@test1 = 10
    }

    @typeError("an impure method cannot override a pure method expected: Int found : Int @inverse_macros.test2")
    class Impl2 extends Impl {
      override def annot: Int@test2 = 20
    }

    @typeError("an overriding impure method must contain a compatible IMAnnotation expected: Int @inverse_macros.test2 found : Int @inverse_macros.test1")
    class Impl3 extends Base2 {
      override def annot: Int@test1 = 20
    }
  }
}
