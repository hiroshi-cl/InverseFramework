package inverse_macros

import org.scalatest.FunSuite

class ImportTest extends FunSuite {

  def withCompanion(i: Int@importWithCompanion) = 10
  def withoutCompanion(i: Int@importWithoutCompanion) = 10

  test("@IMImport") {
    transform {
      withCompanion(10)
      withoutCompanion(10)

      withCompanion(10 + fromClass + fromModule)
      withoutCompanion(10 + fromClass)
    }
  }

}
