import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

class PlaceboAssortedZoo {
  @placebo def foo(x: Int) = x
  @placebo val bar = 2
  @placebo var baz = 3
  @placebo lazy val bax = 4
  @placebo type T = Int
}

class PlaceboAssorted extends FunSuite {
  // consider the affect of inverse macro system
  test("nested") {
    val output = typeOf[PlaceboAssortedZoo].decls.sorted.map(_.toString).mkString("\n")
    val answer = """
                   |constructor PlaceboAssortedZoo
                   |method foo
                   |type T
                   |value bar
                   |value bar
                   |method baz
                   |method baz_=
                   |variable baz
                   |value bax
                   |lazy value bax
                 """.trim.stripMargin
    if(output == answer)
      info("The inverse macro system is disabled.")
    else {
      info("The inverse macro system is enabled.")
      assert(output.lines.toList.sorted === answer.lines.toList.sorted)
    }
  }

  test("local") {
    @placebo def foo(x: Int) = x
    @placebo val bar = 2
    @placebo var baz = 3
    @placebo lazy val bax = 4
    @placebo type T = Int

    assert(foo(1) === 1)
    assert(bar === 2)
    assert(baz === 3)
    assert(bax === 4)
    assert(List[T](5) === List(5))
  }
}
