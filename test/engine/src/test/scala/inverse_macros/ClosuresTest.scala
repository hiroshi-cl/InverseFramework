package inverse_macros

import debug._
import org.scalatest.FunSuite

class ClosuresTest extends FunSuite {
  def intFunc(a: () => Int): Int = ???

  def intAnnotFunc(a: () => Int@test1): Int = ???

  def polyAnnotFunc[T](a: () => T@test1): T = ???

  def polyAnnotFunc2[T](a: () => Int@test2gen[T]): T = ???

  def int = ??? : Int

  def boolean = ??? : Boolean

  def intAnnot = ??? : Int@test1

  def intAnnot2 = ??? : Int@test2

  def booleanAnnot = ??? : Boolean@test1

  test("closure -") {
    @typeError("an impure object argument cannot be passed to a pure object parameter `a` of method intFunc expected: () => Int found : () => Int @inverse_macros.test1")
    val error1 = transform(intFunc(() => intAnnot))

    @typeError("type mismatch; found : Boolean @inverse_macros.test1 required: Int @inverse_macros.test1")
    val error2 = transform(intAnnotFunc(() => booleanAnnot))

    @typeError("type mismatch; found : Int @inverse_macros.test2 required: Int @inverse_macros.test1")
    val error3 = transform(intAnnotFunc(() => intAnnot2))
  }

  test("closure +") {
    @typeable("val correct1: Int = ClosuresTest.this.intFunc((() => ClosuresTest.this.int))")
    val correct1 = transform(intFunc(() => int))

    @typeable("val correct2: Int = { @inverse_macros.IMLinearizerSynth <synthetic> <artifact> val $$: () => Int @inverse_macros.test1 = (() => ClosuresTest.this.intAnnot); ClosuresTest.this.intAnnotFunc($$) }")
    val correct2 = transform(intAnnotFunc(() => intAnnot))

    @typeable("val correct3: Int = ClosuresTest.this.intAnnotFunc((() => ClosuresTest.this.int))")
    val correct3 = transform(intAnnotFunc(() => int))

    assert(showTpe(transform(polyAnnotFunc(() => intAnnot))) == "Int")
    assert(showTpe(transform(polyAnnotFunc2(() => int: Int@test2gen[Int]))) == "Int")
    assert(showTpe(transform(() => if (boolean) intAnnot else intAnnot)) == "() => Int @inverse_macros.test1")
    assert(showTpe(transform(() => int match {
      case _ => intAnnot
    })) == "() => Int @inverse_macros.test1")
    assert(showTpe(transform(() => try intAnnot catch {
      case _: Throwable => intAnnot
    })) == "() => Int @inverse_macros.test1")
  }

  test("if/match/try -") {
    @typeError()
    val error4 = transform(intFunc(() => if (boolean) intAnnot else intAnnot))

    @typeError()
    val error5 = transform(intFunc(() => int match {
      case _ => intAnnot
    }))

    @typeError()
    val error6 = transform(intFunc(() => try intAnnot catch {
      case _: Throwable => intAnnot
    }))

    @typeError()
    val error7 = transform(intFunc(() => {
      intAnnot
      intAnnot
    }))

//    @typeError()
//    val error4_2 = intFunc(() => if (boolean) intAnnot else intAnnot)
//
//    @typeError()
//    val error5_2 = intFunc(() => int match {
//      case _ => intAnnot
//    })
//
//    @typeError()
//    val error6_2 = intFunc(() => try intAnnot catch {
//      case _: Throwable => intAnnot
//    })
//
//    @typeError()
//    val error7_2 = intFunc(() => {
//      intAnnot
//      intAnnot
//    })
  }

  test("complex -") {
    @typeError("an impure object argument cannot be passed to a pure object parameter `a` of method intFunc expected: () => Int found : () => Int @inverse_macros.test1")
    val complex1 = transform(int + intFunc(() => intAnnot))
  }

  test("function -") {
    @typeError("an impure object argument cannot be passed to a pure object parameter `a` of method intFunc expected: () => Int found : () => Int @inverse_macros.test1")
    val funtion2 = transform((a: Int) => intFunc(() => intAnnot))
  }

}
