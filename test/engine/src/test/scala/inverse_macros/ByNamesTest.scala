package inverse_macros

import debug._
import org.scalatest.FunSuite

class ByNamesTest extends FunSuite {
  def intFunc(a: => Int): Int = ???

  def intAnnotFunc(a: => Int@test1): Int = ???

  def polyAnnotFunc[T](a: => T@test1): T = ???

  def polyAnnotFunc2[T](a: => Int@test2gen[T]): T = ???

  def int = ??? : Int

  def boolean = ??? : Boolean

  def intAnnot = ??? : Int@test1

  def intAnnot2 = ??? : Int@test2

  def booleanAnnot = ??? : Boolean@test1

  def &&(b: Boolean) = ???

  def compare(s1: String)(s2: String) = {
    val t1 = replaceFreshVariables(s1.replaceAll("\\s+", " "))
    val t2 = replaceFreshVariables(s2.replaceAll("\\s+", " "))
    assert(t1 == t2)
  }

  test("by name -") {
    @typeError("an impure argument cannot be passed to a pure by-name parameter `a` of method intFunc expected: => Int found : => Int @inverse_macros.test1")
    val error1 = transform(intFunc(intAnnot))

    @typeError("an impure argument cannot be passed to method && expected: Boolean found : Boolean @inverse_macros.test1")
    val error2 = transform(boolean && booleanAnnot)
  }

  test("by name +") {
    compare(show(transform(intAnnotFunc(intAnnot))))("ByNamesTest.this.intAnnotFunc(ByNamesTest.this.intAnnot)")
    compare(show(transform(intAnnotFunc(int))))("ByNamesTest.this.intAnnotFunc(ByNamesTest.this.int)")
    compare(show(transform(booleanAnnot && boolean)))(
      "{ @inverse_macros.IMLinearizerSynth <synthetic> <artifact> val fresh$macro$1: Boolean = ByNamesTest.this.booleanAnnot; fresh$macro$1.&&(ByNamesTest.this.boolean) }"
    )
    compare(show(transform(&&(booleanAnnot))))(
      "{ @inverse_macros.IMLinearizerSynth <synthetic> <artifact> val fresh$macro$2: Boolean = ByNamesTest.this.booleanAnnot; ByNamesTest.this.&&(fresh$macro$2) }"
    )
    compare(showTpe(transform(polyAnnotFunc(intAnnot))))("Int")
    compare(showTpe(transform(polyAnnotFunc2(int: Int@test2gen[Int]))))("Int")
  }


  // TODO: while / if / match / try validation

  test("if/match/try -") {
    @typeError("an IMAnnotation must be concrete, but @inverse_macros.IMAnnotation is abstract. inverse_macros.IMAnnotation = inverse_macros.test1 & inverse_macros.test2")
    val error1 = if (boolean) intAnnot else intAnnot2

    @typeError("an IMAnnotation must be concrete, but @inverse_macros.IMAnnotation is abstract. inverse_macros.IMAnnotation = inverse_macros.test1 & inverse_macros.test2")
    val error2 = int match {
      case _ => intAnnot;
      case _ => intAnnot2
    }

    @typeError("an IMAnnotation must be concrete, but @inverse_macros.IMAnnotation is abstract. inverse_macros.IMAnnotation = inverse_macros.test1 & inverse_macros.test2")
    val error3 = try intAnnot catch {
      case _: Throwable => intAnnot2
    }

    @typeError("an impure argument cannot be passed to a pure by-name parameter `a` of method intFunc expected: => Int found : => Int @inverse_macros.test1")
    val error4 = transform(intFunc(if (boolean) intAnnot else intAnnot))

    @typeError("an impure argument cannot be passed to a pure by-name parameter `a` of method intFunc expected: => Int found : => Int @inverse_macros.test1")
    val error5 = transform(intFunc(int match { case _ => intAnnot }))

    @typeError("an impure argument cannot be passed to a pure by-name parameter `a` of method intFunc expected: => Int found : => Int @inverse_macros.test1")
    val error6 = transform(intFunc(try intAnnot catch {
      case _: Throwable => intAnnot
    }))
  }

  test("if/match/try +") {
    compare(show(transform(if (booleanAnnot) int else int)))(
      "{ @inverse_macros.IMLinearizerSynth <synthetic> <artifact> val $$: Boolean = ByNamesTest.this.booleanAnnot; if ($$) ByNamesTest.this.int else ByNamesTest.this.int }"
    )

    compare(show(transform(if (boolean) intAnnot else int)))(
      "if (ByNamesTest.this.boolean) ByNamesTest.this.intAnnot else ByNamesTest.this.int"
    )

    compare(showTpe(transform(if (boolean) intAnnot else int)))("Int @inverse_macros.test1")

    compare(show(transform(if (boolean) int else intAnnot)))(
      "if (ByNamesTest.this.boolean) ByNamesTest.this.int else ByNamesTest.this.intAnnot"
    )

    compare(showTpe(transform(if (boolean) int else intAnnot)))("Int @inverse_macros.test1")

    compare(show(transform(intAnnot match { case _ => int })))(
      "{ @inverse_macros.IMLinearizerSynth <synthetic> <artifact> val $$: Int = ByNamesTest.this.intAnnot; $$ match { case _ => ByNamesTest.this.int } }"
    )

    compare(show(transform(int match { case _ => intAnnot })))(
      "ByNamesTest.this.int match { case _ => ByNamesTest.this.intAnnot }"
    )

    compare(showTpe(transform(int match { case _ => intAnnot })))(
      "Int @inverse_macros.test1"
    )

    compare(show(transform(try intAnnot catch {
      case _: Throwable => int
    })))(
      "try { ByNamesTest.this.intAnnot } catch { case (_: Throwable) => ByNamesTest.this.int }"
    )

    compare(showTpe(transform(try intAnnot catch {
      case _: Throwable => int
    })))(
      "Int @inverse_macros.test1"
    )

    compare(show(transform(try int catch {
      case _: Throwable => intAnnot
    })))(
      "try { ByNamesTest.this.int } catch { case (_: Throwable) => ByNamesTest.this.intAnnot }"
    )

    compare(showTpe(transform(try int catch {
      case _: Throwable => intAnnot
    })))(
      "Int @inverse_macros.test1"
    )

    compare(show(transform(try intAnnot)))(
      "try { ByNamesTest.this.intAnnot }"
    )

    compare(showTpe(transform(try intAnnot)))(
      "Int @inverse_macros.test1"
    )

    compare(show(transform(try intAnnot catch {
      case _: Throwable => intAnnot
    } finally intAnnot2)))(
      "try { ByNamesTest.this.intAnnot } catch { case (_: Throwable) => ByNamesTest.this.intAnnot } finally { ByNamesTest.this.intAnnot2; () }"
    )

    compare(showTpe(transform(try intAnnot catch {
      case _: Throwable => intAnnot
    } finally intAnnot2)))(
      "Int @inverse_macros.test1"
    )

    compare(show(transform(try intAnnot finally intAnnot2)))(
      "try { ByNamesTest.this.intAnnot } finally { ByNamesTest.this.intAnnot2; () }"
    )

    compare(showTpe(transform(try intAnnot finally intAnnot2)))(
      "Int @inverse_macros.test1"
    )

    compare(show(transform(if (boolean) intAnnot else booleanAnnot)))(
      "if (ByNamesTest.this.boolean) ByNamesTest.this.intAnnot else ByNamesTest.this.booleanAnnot"
    )

    compare(showTpe(transform(if (boolean) intAnnot else booleanAnnot)))(
      "AnyVal @inverse_macros.test1"
    )

    compare(showTpe(transform(if (boolean) intAnnot)))(
      "AnyVal @inverse_macros.test1"
    )
  }

  test("complex -") {
    @typeError("an impure argument cannot be passed to a pure by-name parameter `a` of method intFunc expected: => Int found : => Int @inverse_macros.test1")
    val complex1 = transform(int + intFunc(intAnnot))

    @typeError("an impure argument cannot be passed to method && expected: Boolean found : Boolean @inverse_macros.test1")
    val complex2 = transform(booleanAnnot && (booleanAnnot && booleanAnnot))
  }

  test("complex +") {
    compare(show(transform(intFunc(int + intAnnot))))(
      "ByNamesTest.this.intFunc({ @inverse_macros.IMLinearizerSynth <synthetic> <artifact> val $$: Int = ByNamesTest.this.intAnnot; ByNamesTest.this.int.+($$) })"
    )
    compare(show(transform(booleanAnnot && (booleanAnnot && boolean))))(
      "{ @inverse_macros.IMLinearizerSynth <synthetic> <artifact> val $$: Boolean = ByNamesTest.this.booleanAnnot; $$.&&({ @inverse_macros.IMLinearizerSynth <synthetic> <artifact> val $$: Boolean = ByNamesTest.this.booleanAnnot; $$.&&(ByNamesTest.this.boolean) }) }"
    )
  }


  test("function -") {
    @typeError("a variable type must be pure, but @inverse_macros.test1 found")
    val funtion1 = transform((a: Int@inverse_macros.test1) => a)

    @typeError("an impure argument cannot be passed to a pure by-name parameter `a` of method intFunc expected: => Int found : => Int @inverse_macros.test1")
    val funtion2 = transform((a: Int) => intFunc(intAnnot))
  }

  test("function +") {
    compare(show(transform {
      val func = (a: Int) => intAnnot
      println(func(int))
    }))(
      """{
        |  val func: Int => Int @inverse_macros.test1 = ((a: Int) => ByNamesTest.this.intAnnot);
        |  @inverse_macros.IMLinearizerSynth <synthetic> <artifact> val $$: Int = func.apply(ByNamesTest.this.int);
        |  scala.this.Predef.println($$)
        |}""".stripMargin
    )

    compare(show(transform {
      val func = (a: Int) => {
        println(intAnnot)
      }
      func(int)
    }))(
      """{
        |  val func: Int => Unit = ((a: Int) => {
        |     @inverse_macros.IMLinearizerSynth <synthetic> <artifact> val $$: Int = ByNamesTest.this.intAnnot;
        |     scala.this.Predef.println($$)
        |  });
        |  func.apply(ByNamesTest.this.int)
        |}""".stripMargin
    )

    compare(show(transform {
      println((a: Int) => intAnnot)
    }))(
      """{
        |  @inverse_macros.IMLinearizerSynth <synthetic> <artifact> val $$: Int => Int @inverse_macros.test1 =
        |    ((a: Int) => ByNamesTest.this.intAnnot);
        |  scala.this.Predef.println($$)
        |}""".stripMargin
    )
  }
}
