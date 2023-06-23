// #Sireum

package tc

import org.sireum._
import org.sireum.Random.Gen64

/*
GENERATED FROM

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/temp_control_results/src/main/data/tc/Base_Types.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/temp_control_results/src/main/data/tc/CoolingFan/FanAck.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/temp_control_results/src/main/data/tc/CoolingFan/FanCmd.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/temp_control_results/src/main/data/tc/GUMBO_Definitions/GUMBO__Library.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/temp_control_results/src/main/data/tc/TempControlSoftwareSystem/SetPoint_i.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/temp_control_results/src/main/data/tc/TempControlSoftwareSystem/SetPoint_i_GumboX.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/temp_control_results/src/main/data/tc/TempSensor/Temperature_i.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/temp_control_results/src/main/data/tc/TempSensor/Temperature_i_GumboX.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/temp_control_results/src/main/art/DataContent.scala

*/

@datatype class Config_Z(low: Option[Z], high: Option[Z], attempts: Z, filter: Z => B) {}

@datatype class Config_B(attempts: Z, filter: B => B) {}

@datatype class Config_C(attempts: Z, filter: C => B) {}

@datatype class Config_R(low: Option[R], high: Option[R], attempts: Z, filter: R => B) {}

@datatype class Config_F32(low: Option[F32], high: Option[F32], attempts: Z, filter: F32 => B) {}

@datatype class Config_F64(low: Option[F64], high: Option[F64], attempts: Z, filter: F64 => B) {}

@datatype class Config_S8(low: Option[S8], high: Option[S8], attempts: Z, filter: S8 => B) {}

@datatype class Config_S16(low: Option[S16], high: Option[S16], attempts: Z, filter: S16 => B) {}

@datatype class Config_S32(low: Option[S32], high: Option[S32], attempts: Z, filter: S32 => B) {}

@datatype class Config_S64(low: Option[S64], high: Option[S64], attempts: Z, filter: S64 => B) {}

@datatype class Config_U8(low: Option[U8], high: Option[U8], attempts: Z, filter: U8 => B) {}

@datatype class Config_U16(low: Option[U16], high: Option[U16], attempts: Z, filter: U16 => B) {}

@datatype class Config_U32(low: Option[U32], high: Option[U32], attempts: Z, filter: U32 => B) {}

@datatype class Config_U64(low: Option[U64], high: Option[U64], attempts: Z, filter: U64 => B) {}

@datatype class Config_CoolingFanFanAckType(attempts: Z, filter: CoolingFan.FanAck.Type => B) {}

@datatype class Config_CoolingFanFanAck_Payload(attempts: Z, filter: CoolingFan.FanAck_Payload => B) {}

@datatype class Config_TempControlSoftwareSystemSetPoint_i(attempts: Z, filter: TempControlSoftwareSystem.SetPoint_i => B) {}

@datatype class Config_TempControlSoftwareSystemSetPoint_i_Payload(attempts: Z, filter: TempControlSoftwareSystem.SetPoint_i_Payload => B) {}

@datatype class Config_CoolingFanFanCmdType(attempts: Z, filter: CoolingFan.FanCmd.Type => B) {}

@datatype class Config_CoolingFanFanCmd_Payload(attempts: Z, filter: CoolingFan.FanCmd_Payload => B) {}

@datatype class Config_TempSensorTemperature_i(attempts: Z, filter: TempSensor.Temperature_i => B) {}

@datatype class Config_TempSensorTemperature_i_Payload(attempts: Z, filter: TempSensor.Temperature_i_Payload => B) {}

@datatype class Config_Base_TypesBoolean_Payload(attempts: Z, filter: Base_Types.Boolean_Payload => B) {}

@datatype class Config_Base_TypesInteger_Payload(attempts: Z, filter: Base_Types.Integer_Payload => B) {}

@datatype class Config_Base_TypesInteger_8_Payload(attempts: Z, filter: Base_Types.Integer_8_Payload => B) {}

@datatype class Config_Base_TypesInteger_16_Payload(attempts: Z, filter: Base_Types.Integer_16_Payload => B) {}

@datatype class Config_Base_TypesInteger_32_Payload(attempts: Z, filter: Base_Types.Integer_32_Payload => B) {}

@datatype class Config_Base_TypesInteger_64_Payload(attempts: Z, filter: Base_Types.Integer_64_Payload => B) {}

@datatype class Config_Base_TypesUnsigned_8_Payload(attempts: Z, filter: Base_Types.Unsigned_8_Payload => B) {}

@datatype class Config_Base_TypesUnsigned_16_Payload(attempts: Z, filter: Base_Types.Unsigned_16_Payload => B) {}

@datatype class Config_Base_TypesUnsigned_32_Payload(attempts: Z, filter: Base_Types.Unsigned_32_Payload => B) {}

@datatype class Config_Base_TypesUnsigned_64_Payload(attempts: Z, filter: Base_Types.Unsigned_64_Payload => B) {}

@datatype class Config_Base_TypesFloat_Payload(attempts: Z, filter: Base_Types.Float_Payload => B) {}

@datatype class Config_Base_TypesFloat_32_Payload(attempts: Z, filter: Base_Types.Float_32_Payload => B) {}

@datatype class Config_Base_TypesFloat_64_Payload(attempts: Z, filter: Base_Types.Float_64_Payload => B) {}

@datatype class Config_Base_TypesCharacter_Payload(attempts: Z, filter: Base_Types.Character_Payload => B) {}

@datatype class Config_Base_TypesString_Payload(attempts: Z, filter: Base_Types.String_Payload => B) {}

@datatype class Config_Base_TypesBits_Payload(attempts: Z, filter: Base_Types.Bits_Payload => B) {}

@datatype class Config__artDataContent(attempts: Z, additiveTypeFiltering: B, typeFilter: ISZ[_artDataContent_DataTypeId.Type], filter: art.DataContent => B) {}

@datatype class Config__artEmpty(attempts: Z, filter: art.Empty => B) {}


