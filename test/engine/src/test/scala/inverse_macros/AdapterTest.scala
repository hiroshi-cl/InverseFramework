package inverse_macros

import debug._
import org.scalatest.FunSuite

class AdapterTest extends FunSuite {
  import AdapterTest._

  test("adapt") {
    @typeable
    def _0 = List(10).traverse(i => (i + 1).asInstanceOf[Int @traverse])

    @typeable("@inverse_macros.IMEngineApplied def _1: List[Int] = { @inverse_macros.IMLinearizerSynth <synthetic> <artifact> val $$: Int => Int @inverse_macros.traverse = ((i: Int) => i.+(1).asInstanceOf[Int @inverse_macros.traverse]); AdapterTest.Traverse[Int](immutable.this.List.apply[Int](10)).traverse[Int]($$) }")
    def _1 = List(10).map(i => (i + 1).asInstanceOf[Int @traverse])

    assert(List(10).map(i => (i + 1).asInstanceOf[Int @traverse]) == List(11))
  }
}

object AdapterTest {
  implicit class Traverse[T](val list: List[T]) extends AnyVal {
    def traverse[U](func: T => U@traverse): List[U] =
      list.map(func(_).asInstanceOf[U])
  }
}
