import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

class PlaceboParameterZoo {
  class C[@placebo T](@placebo val x: Int)
  object С
  def m[@placebo T, @placebo U](@placebo x: Int)(@placebo y: Int) = ???
  type T[@placebo U] = U
}

class PlaceboParameters extends FunSuite {

  // consider the affect of inverse macro system
  test("combo") {
    val output = typeOf[PlaceboParameterZoo].decls.sorted.map(_.toString).mkString("\n")
    val answer = """
                   |constructor PlaceboParameterZoo
                   |class C
                   |object С
                   |method m
                   |type T
                 """.trim.stripMargin
    if(output == answer)
      info("The inverse macro system is disabled.")
    else {
      info("The inverse macro system is enabled.")
      assert(output.lines.toList.sorted === answer.lines.toList.sorted)
    }
  }
}
