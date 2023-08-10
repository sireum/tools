// #Sireum

package isolette

import org.sireum._
import org.sireum.Random.Gen64

/*
GENERATED FROM

DataContent.scala

Base_Types.scala

Failure_Flag_impl.scala

Monitor_Mode.scala

On_Off.scala

PhysicalTemp_impl.scala

Regulator_Mode.scala

Status.scala

TempWstatus_impl.scala

Temp_impl.scala

ValueStatus.scala

Heat.scala

Interface_Interaction.scala

*/

@datatype class Config_Z(low: Option[Z], high: Option[Z], attempts: Z, verbose: B, filter: Z => B) {}

@datatype class Config_B(attempts: Z, verbose: B, filter: B => B) {}

@datatype class Config_C(attempts: Z, verbose: B, filter: C => B) {}

@datatype class Config_R(low: Option[R], high: Option[R], attempts: Z, verbose: B, filter: R => B) {}

@datatype class Config_F32(low: Option[F32], high: Option[F32], attempts: Z, verbose: B, filter: F32 => B) {}

@datatype class Config_F64(low: Option[F64], high: Option[F64], attempts: Z, verbose: B, filter: F64 => B) {}

@datatype class Config_S8(low: Option[S8], high: Option[S8], attempts: Z, verbose: B, filter: S8 => B) {}

@datatype class Config_S16(low: Option[S16], high: Option[S16], attempts: Z, verbose: B, filter: S16 => B) {}

@datatype class Config_S32(low: Option[S32], high: Option[S32], attempts: Z, verbose: B, filter: S32 => B) {}

@datatype class Config_S64(low: Option[S64], high: Option[S64], attempts: Z, verbose: B, filter: S64 => B) {}

@datatype class Config_U8(low: Option[U8], high: Option[U8], attempts: Z, verbose: B, filter: U8 => B) {}

@datatype class Config_U16(low: Option[U16], high: Option[U16], attempts: Z, verbose: B, filter: U16 => B) {}

@datatype class Config_U32(low: Option[U32], high: Option[U32], attempts: Z, verbose: B, filter: U32 => B) {}

@datatype class Config_U64(low: Option[U64], high: Option[U64], attempts: Z, verbose: B, filter: U64 => B) {}

@datatype class Config__artDataContent(attempts: Z, verbose: B, additiveTypeFiltering: B, typeFilter: ISZ[_artDataContent_DataTypeId.Type], filter: art.DataContent => B) {}

@datatype class Config__artEmpty(attempts: Z, verbose: B, filter: art.Empty => B) {}

@datatype class Config_Base_TypesBoolean_Payload(attempts: Z, verbose: B, filter: Base_Types.Boolean_Payload => B) {}

@datatype class Config_Base_TypesInteger_Payload(attempts: Z, verbose: B, filter: Base_Types.Integer_Payload => B) {}

@datatype class Config_Base_TypesInteger_8_Payload(attempts: Z, verbose: B, filter: Base_Types.Integer_8_Payload => B) {}

@datatype class Config_Base_TypesInteger_16_Payload(attempts: Z, verbose: B, filter: Base_Types.Integer_16_Payload => B) {}

@datatype class Config_Base_TypesInteger_32_Payload(attempts: Z, verbose: B, filter: Base_Types.Integer_32_Payload => B) {}

@datatype class Config_Base_TypesInteger_64_Payload(attempts: Z, verbose: B, filter: Base_Types.Integer_64_Payload => B) {}

@datatype class Config_Base_TypesUnsigned_8_Payload(attempts: Z, verbose: B, filter: Base_Types.Unsigned_8_Payload => B) {}

@datatype class Config_Base_TypesUnsigned_16_Payload(attempts: Z, verbose: B, filter: Base_Types.Unsigned_16_Payload => B) {}

@datatype class Config_Base_TypesUnsigned_32_Payload(attempts: Z, verbose: B, filter: Base_Types.Unsigned_32_Payload => B) {}

@datatype class Config_Base_TypesUnsigned_64_Payload(attempts: Z, verbose: B, filter: Base_Types.Unsigned_64_Payload => B) {}

@datatype class Config_Base_TypesFloat_Payload(attempts: Z, verbose: B, filter: Base_Types.Float_Payload => B) {}

@datatype class Config_Base_TypesFloat_32_Payload(attempts: Z, verbose: B, filter: Base_Types.Float_32_Payload => B) {}

@datatype class Config_Base_TypesFloat_64_Payload(attempts: Z, verbose: B, filter: Base_Types.Float_64_Payload => B) {}

@datatype class Config_Base_TypesCharacter_Payload(attempts: Z, verbose: B, filter: Base_Types.Character_Payload => B) {}

@datatype class Config_Base_TypesString_Payload(attempts: Z, verbose: B, filter: Base_Types.String_Payload => B) {}

@datatype class Config_Base_TypesBits_Payload(attempts: Z, verbose: B, filter: Base_Types.Bits_Payload => B) {}

@datatype class Config_Isolette_Data_ModelFailure_Flag_impl(attempts: Z, verbose: B, filter: Isolette_Data_Model.Failure_Flag_impl => B) {}

@datatype class Config_Isolette_Data_ModelFailure_Flag_impl_Payload(attempts: Z, verbose: B, filter: Isolette_Data_Model.Failure_Flag_impl_Payload => B) {}

@datatype class Config_Isolette_Data_ModelMonitor_ModeType(attempts: Z, verbose: B, filter: Isolette_Data_Model.Monitor_Mode.Type => B) {}

@datatype class Config_Isolette_Data_ModelMonitor_Mode_Payload(attempts: Z, verbose: B, filter: Isolette_Data_Model.Monitor_Mode_Payload => B) {}

@datatype class Config_Isolette_Data_ModelOn_OffType(attempts: Z, verbose: B, filter: Isolette_Data_Model.On_Off.Type => B) {}

@datatype class Config_Isolette_Data_ModelOn_Off_Payload(attempts: Z, verbose: B, filter: Isolette_Data_Model.On_Off_Payload => B) {}

@datatype class Config_Isolette_Data_ModelPhysicalTemp_impl(attempts: Z, verbose: B, filter: Isolette_Data_Model.PhysicalTemp_impl => B) {}

@datatype class Config_Isolette_Data_ModelPhysicalTemp_impl_Payload(attempts: Z, verbose: B, filter: Isolette_Data_Model.PhysicalTemp_impl_Payload => B) {}

@datatype class Config_Isolette_Data_ModelRegulator_ModeType(attempts: Z, verbose: B, filter: Isolette_Data_Model.Regulator_Mode.Type => B) {}

@datatype class Config_Isolette_Data_ModelRegulator_Mode_Payload(attempts: Z, verbose: B, filter: Isolette_Data_Model.Regulator_Mode_Payload => B) {}

@datatype class Config_Isolette_Data_ModelStatusType(attempts: Z, verbose: B, filter: Isolette_Data_Model.Status.Type => B) {}

@datatype class Config_Isolette_Data_ModelStatus_Payload(attempts: Z, verbose: B, filter: Isolette_Data_Model.Status_Payload => B) {}

@datatype class Config_Isolette_Data_ModelTempWstatus_impl(attempts: Z, verbose: B, filter: Isolette_Data_Model.TempWstatus_impl => B) {}

@datatype class Config_Isolette_Data_ModelTempWstatus_impl_Payload(attempts: Z, verbose: B, filter: Isolette_Data_Model.TempWstatus_impl_Payload => B) {}

@datatype class Config_Isolette_Data_ModelTemp_impl(attempts: Z, verbose: B, filter: Isolette_Data_Model.Temp_impl => B) {}

@datatype class Config_Isolette_Data_ModelTemp_impl_Payload(attempts: Z, verbose: B, filter: Isolette_Data_Model.Temp_impl_Payload => B) {}

@datatype class Config_Isolette_Data_ModelValueStatusType(attempts: Z, verbose: B, filter: Isolette_Data_Model.ValueStatus.Type => B) {}

@datatype class Config_Isolette_Data_ModelValueStatus_Payload(attempts: Z, verbose: B, filter: Isolette_Data_Model.ValueStatus_Payload => B) {}

@datatype class Config_Isolette_EnvironmentHeatType(attempts: Z, verbose: B, filter: Isolette_Environment.Heat.Type => B) {}

@datatype class Config_Isolette_EnvironmentHeat_Payload(attempts: Z, verbose: B, filter: Isolette_Environment.Heat_Payload => B) {}

@datatype class Config_Isolette_EnvironmentInterface_InteractionType(attempts: Z, verbose: B, filter: Isolette_Environment.Interface_Interaction.Type => B) {}

@datatype class Config_Isolette_EnvironmentInterface_Interaction_Payload(attempts: Z, verbose: B, filter: Isolette_Environment.Interface_Interaction_Payload => B) {}


