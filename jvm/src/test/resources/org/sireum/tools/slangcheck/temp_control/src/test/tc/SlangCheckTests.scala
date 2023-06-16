package tc

import org.scalatest.funsuite.AnyFunSuite

class SlangCheckTests extends AnyFunSuite {

  test("example") {
    val setpoint = TempControlSoftwareSystem.SetPoint_i(TempSensor.Temperature_i(70f), TempSensor.Temperature_i(80f))
    println(setpoint)
  }
}
