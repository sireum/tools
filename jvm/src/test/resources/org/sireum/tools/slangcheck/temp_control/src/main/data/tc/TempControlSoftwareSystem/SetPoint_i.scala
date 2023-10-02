// #Sireum

package tc.TempControlSoftwareSystem

import org.sireum._
import tc._

// This file was auto-generated.  Do not edit

object SetPoint_i {
  def example(): TempControlSoftwareSystem.SetPoint_i = {
    return TempControlSoftwareSystem.SetPoint_i(
      low = TempSensor.Temperature_i.example(),
      high = TempSensor.Temperature_i.example())
  }

  /** invariant SetPoint_Data_Invariant
    */
  @strictpure def SetPoint_Data_Invariant_Invariant(value: TempControlSoftwareSystem.SetPoint_i): B =
    value.low.degrees >= 50.0f &
      value.high.degrees <= 110.0f &
      value.low.degrees <= value.high.degrees

  /** D-Inv Data Invariant for TempControlSoftwareSystem.SetPoint_i
    */
  @strictpure def D_Inv_SetPoint_i(value: TempControlSoftwareSystem.SetPoint_i): B =
    (SetPoint_Data_Invariant_Invariant(value))

  /** D-Inv-Guard Data Invariant for TempControlSoftwareSystem.SetPoint_i
    */
  @strictpure def D_Inv_Guard_SetPoint_i(value: Option[TempControlSoftwareSystem.SetPoint_i]): B =
    value.nonEmpty -->: D_Inv_SetPoint_i(value.get)
}

@datatype class SetPoint_i(
  val low: TempSensor.Temperature_i,
  val high: TempSensor.Temperature_i) {
  @spec def SetPoint_Data_Invariant = Invariant(
    low.degrees >= 50.0f &
      high.degrees <= 110.0f &
      low.degrees <= high.degrees
  )
}

object SetPoint_i_Payload {
  def example(): SetPoint_i_Payload = {
    return SetPoint_i_Payload(TempControlSoftwareSystem.SetPoint_i.example())
  }
}

@datatype class SetPoint_i_Payload(value: TempControlSoftwareSystem.SetPoint_i) extends art.DataContent
