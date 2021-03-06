package inverse_macros.continuations

import debug._
import inverse_macros.IMWild
import org.scalatest.FunSuite
import org.junit.Assert._

import scala.language.implicitConversions

class ContinuationTest extends FunSuite {
  def compare(s1: String)(s2: String) = {
    val t1 = replaceFreshVariables(s1.replaceAll("\\s+", " "))
    val t2 = replaceFreshVariables(s2.replaceAll("\\s+", " "))
    assert(t1 == t2)
  }

  test("shift @") {
    inverse_macros.transform {
      def m0(): Int@IMWild = {
        shift((k: Int => Int) => k(k(7))) * 2
      }

      def m1(): Int@IMWild = {
        2 * shift((k: Int => Int) => k(k(7)))
      }
      assertEquals(28, reset[Int, Int](m0()))
      assertEquals(28, reset[Int, Int](m1()))
    }
  }


  test("IfThenElse") {
    inverse_macros.transform {
      val out = new StringBuilder

      def printOut(x: Any): Unit = out ++= x.toString

      {
        def test(x: Int): Int@IMWild = {
          if (x <= 7)
            shift { k: (Int => Int) => k(k(k(x))) }
          else
            shift { k: (Int => Int) => k(x) }
        }

        assert(10 == reset[Int, Int](1 + test(7)))
        assert(9 == reset[Int, Int](1 + test(8)))
      }

      def test1(x: Int): Int@IMWild = {
        if (x <= 7)
          shift { k: (Int => Int) => k(k(k(x))) }
        else
          x
      }

      def test2(x: Int): Int@IMWild = {
        if (x <= 7)
          x
        else
          shift { k: (Int => Int) => k(k(k(x))) }
      }

      {
        assertEquals(10, reset[Int, Int](1 + test1(7)))
        assertEquals(9, reset[Int, Int](1 + test1(8)))
        assertEquals(8, reset[Int, Int](1 + test2(7)))
        assertEquals(11, reset[Int, Int](1 + test2(8)))
      }

      def test3(x: Int): Unit@IMWild = {
        if (x <= 7)
          shift { k: (Unit => Unit) => printOut("abort") }
      }

      {
        out.clear()
        printOut(reset[Unit, Unit] {
          test3(7);
          printOut("alive")
        })
        printOut(reset[Unit, Unit] {
          test3(8);
          printOut("alive")
        })
        assertEquals("abort()alive()", out.toString)
      }

      def util(x: Boolean): Boolean@IMWild = shift { k: (Boolean => Int) => k(x) }

      def test4(x: Int): Int@IMWild = {
        if (util(x <= 7))
          x - 1
        else
          x + 1
      }

      {
        assertEquals(6, reset[Int, Int](test4(7)))
        assertEquals(9, reset[Int, Int](test4(8)))
      }

      def sh(x1: Int): Int@IMWild = shift((k: Int => Int) => k(k(k(x1))))


      def testA(x1: Int): Int@IMWild = {
        sh(x1)
        if (x1 == 42) x1 else sh(x1)
      }

      def testB(x1: Int): Int@IMWild = {
        if (sh(x1) == 43) x1 else x1
      }

      def testC(x1: Int): Int@IMWild = {
        sh(x1)
        if (sh(x1) == 44) x1 else x1
      }

      def testD(x1: Int): Int@IMWild = {
        sh(x1)
        if (sh(x1) == 45) x1 else sh(x1)
      }

      {
        assertEquals(10, reset[Int, Int](1 + testA(7)))
        assertEquals(10, reset[Int, Int](1 + testB(9)))
        assertEquals(10, reset[Int, Int](1 + testC(9)))
        assertEquals(10, reset[Int, Int](1 + testD(7)))
      }
    }
  }
}
