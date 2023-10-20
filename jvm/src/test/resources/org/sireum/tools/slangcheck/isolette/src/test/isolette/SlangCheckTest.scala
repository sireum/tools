package isolette

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

  test("Base_Types.Bits_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Base_TypesBits_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.Failure_Flag_impl Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelFailure_Flag_impl(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.Failure_Flag_impl_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelFailure_Flag_impl_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.Monitor_Mode.Type Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelMonitor_ModeType(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.Monitor_Mode_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelMonitor_Mode_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.On_Off.Type Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelOn_OffType(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.On_Off_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelOn_Off_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.PhysicalTemp_impl Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelPhysicalTemp_impl(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.PhysicalTemp_impl_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelPhysicalTemp_impl_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.Regulator_Mode.Type Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelRegulator_ModeType(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.Regulator_Mode_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelRegulator_Mode_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.Status.Type Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelStatusType(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.Status_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelStatus_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.TempWstatus_impl Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelTempWstatus_impl(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.TempWstatus_impl_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelTempWstatus_impl_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.Temp_impl Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelTemp_impl(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.Temp_impl_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelTemp_impl_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.ValueStatus.Type Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelValueStatusType(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Data_Model.ValueStatus_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_Data_ModelValueStatus_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Environment.Heat.Type Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_EnvironmentHeatType(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Environment.Heat_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_EnvironmentHeat_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Environment.Interface_Interaction.Type Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_EnvironmentInterface_InteractionType(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("Isolette_Environment.Interface_Interaction_Payload Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_Isolette_EnvironmentInterface_Interaction_Payload(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

}
