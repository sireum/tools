package isolette

import isolette.Isolette_Data_Model.{TempWstatus_impl, ValueStatus}
import org.scalatest.funsuite.AnyFunSuite

class SlangCheckTests extends AnyFunSuite {

  test("example") {
    val temp = TempWstatus_impl(80f, ValueStatus.Valid)
    println(temp)
  }
}
