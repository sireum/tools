package tc

import org.scalatest.funsuite.AnyFunSuite
import org.sireum.Random.Impl.Xoshiro256
import org.sireum._

class autogenTest extends AnyFunSuite{

  test("String Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_String(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Z Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Z(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("B Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_B(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("C Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_C(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("R Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_R(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("F32 Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_F32(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("F64 Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_F64(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("S8 Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_S8(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("S16 Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_S16(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("S32 Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_S32(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("S64 Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_S64(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("U8 Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_U8(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("U16 Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_U16(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("U32 Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_U32(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("U64 Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_U64(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("art.DataContent Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen__artDataContent(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("art.Empty Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen__artEmpty(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Boolean_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesBoolean_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Integer_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesInteger_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Integer_8_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesInteger_8_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Integer_16_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesInteger_16_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Integer_32_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesInteger_32_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Integer_64_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesInteger_64_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Unsigned_8_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesUnsigned_8_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Unsigned_16_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesUnsigned_16_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Unsigned_32_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesUnsigned_32_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Unsigned_64_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesUnsigned_64_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Float_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesFloat_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Float_32_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesFloat_32_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Float_64_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesFloat_64_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Character_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesCharacter_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.String_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesString_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("ISZ[B] Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_ISZB(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Base_Types.Bits_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesBits_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("CoolingFan.FanAck.Type Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_CoolingFanFanAckType(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("CoolingFan.FanAck_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_CoolingFanFanAck_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("CoolingFan.FanCmd.Type Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_CoolingFanFanCmdType(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("CoolingFan.FanCmd_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_CoolingFanFanCmd_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("TempControlSoftwareSystem.SetPoint_i Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_TempControlSoftwareSystemSetPoint_i(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("TempControlSoftwareSystem.SetPoint_i_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_TempControlSoftwareSystemSetPoint_i_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("TempSensor.Temperature_i Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_TempSensorTemperature_i(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("TempSensor.Temperature_i_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_TempSensorTemperature_i_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Option[TempSensor.Temperature_i] Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_OptionTempSensorTemperature_i(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("TempSensor.example_type Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_TempSensorexample_type(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

}
