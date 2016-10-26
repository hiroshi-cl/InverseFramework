package inverse_macros

import debug._
import org.scalatest.FunSuite

class AdapterTest extends FunSuite {
  import AdapterTest._

  test("adapt") {
    @typeable
    def _0 = List(10).traverse(i => (i + 1).asInstanceOf[Int @traverse])

    @typeable
    def _1 = scala.collection.immutable.List(10).map(i => (i + 1).asInstanceOf[Int @traverse])

    assert(List(10).map(i => (i + 1).asInstanceOf[Int @traverse]) == List(11))
  }
}

object AdapterTest {
  implicit class Traverse[T](val list: List[T]) extends AnyVal {
    def traverse[U](func: T => U@traverse): List[U] =
      list.map(func(_).asInstanceOf[U])
  }
}
