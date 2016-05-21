package inverse_macros

import debug._
import org.scalatest.FunSuite
import org.scalatest.exceptions.TestFailedException

class DebugTest extends FunSuite {

  test("abort") {
    assert(expectException(parse("error")))
  }
  test("normal function") {
    assert(show(applied(1)("to")(10, 1)).replace("scala.this.", "scala.") == "(scala.Predef.intWrapper(1).to(10, 1): Any)")
  }
  test("symbolic name function") {
    assert(show(applied(1)("+")(1)) == "(2: Any)")
  }
  test("normal function (show)") {
    assert(show(applied(1)("to")(10, 1)) == show(1 to(10, 1) : Any))
  }
  test("symbolic name function (show)") {
    assert(show(applied(1)("+")(1)) == show(1 + 1 : Any))
  }
  test("parse") {
    assert(parse("10") == 10)
    assert(parse("10 + 29") == 39)
  }
  test("compile error") {
    @typeError
    def hoge: Int = ()

    @typeError("type mismatch; found : Unit required: Int")
    def hoge: Int = ()

    @typeError
    class hoge { def hoge: Int = () }

    try {
      @typeError
      class piyo
      fail("not failed")
    } catch {
      case e: TestFailedException if e.getMessage == "no type error" =>
    }
  }
}