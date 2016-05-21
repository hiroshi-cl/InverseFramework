package inverse_macros

import debug._
import org.scalatest.FunSuite

class MiscTest extends FunSuite {

  // retyping in prependAny is in danger of failure in this test
  test("patdef") {
    val (x :: Nil) = List(10)
    val (z :: y :: Nil) = List(10, 20)
    val (w :: ww :: Nil) = List(10, 20)

    10 match {
      case x => x
    }
  }

  test("new") {
    @typeable
    def printHoge = transform {
      class Hoge() {
        def this(n: Int){
          this()
        }
      }

      // new exists on the AST.
      // constructor method is called for the new'ed result
      new Hoge()
    }
  }

  test("lazy") {
    @typeable("@inverse_macros.IMEngineApplied def piyo: Unit = { lazy <artifact> var hoge$lzy: Int = _; <stable> <accessor> lazy def hoge: Int = { hoge$lzy = 10; hoge$lzy }; { hoge; () } }")
    def piyo: Unit = {
      lazy val hoge = 10
      hoge
    }

    @typeable
    def piyo2: Unit = {
      lazy val hoge = System.currentTimeMillis()
      hoge
    }
  }
}
