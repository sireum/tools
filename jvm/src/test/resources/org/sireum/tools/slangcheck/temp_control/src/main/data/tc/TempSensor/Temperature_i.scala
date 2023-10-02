// #Sireum

package tc.TempSensor

import org.sireum._
import tc._

// This file was auto-generated.  Do not edit

object Temperature_i {
  def example(): TempSensor.Temperature_i = {
    return TempSensor.Temperature_i(
      degrees = Base_Types.Float_32_example())
  }

  /** invariant AbsZero
    */
  @strictpure def AbsZero_Invariant(value: TempSensor.Temperature_i): B =
    value.degrees >= GUMBO_Definitions.GUMBO__Library.absoluteZero()

  /** D-Inv Data Invariant for TempSensor.Temperature_i
    */
  @strictpure def D_Inv_Temperature_i(value: TempSensor.Temperature_i): B =
    (AbsZero_Invariant(value))

  /** D-Inv-Guard Data Invariant for TempSensor.Temperature_i
    */
  @strictpure def D_Inv_Guard_Temperature_i(value: Option[TempSensor.Temperature_i]): B =
    value.nonEmpty -->: D_Inv_Temperature_i(value.get)
}

@datatype class Temperature_i(
  val degrees: F32) {
  @spec def AbsZero = Invariant(
    degrees >= GUMBO_Definitions.GUMBO__Library.absoluteZero()
  )
}

object Temperature_i_Payload {
  def example(): Temperature_i_Payload = {
    return Temperature_i_Payload(TempSensor.Temperature_i.example())
  }
}

@datatype class Temperature_i_Payload(value: TempSensor.Temperature_i) extends art.DataContent

@datatype class example_type(value: Option[TempSensor.Temperature_i])
