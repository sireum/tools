// #Sireum

package isolette

import org.sireum._
import org.sireum.Random.Gen64

/*
GENERATED FROM

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/isolette_results/src/main/data/isolette/Base_Types.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/isolette_results/src/main/data/isolette/Isolette_Data_Model/Failure_Flag_impl.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/isolette_results/src/main/data/isolette/Isolette_Data_Model/Monitor_Mode.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/isolette_results/src/main/data/isolette/Isolette_Data_Model/On_Off.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/isolette_results/src/main/data/isolette/Isolette_Data_Model/PhysicalTemp_impl.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/isolette_results/src/main/data/isolette/Isolette_Data_Model/Regulator_Mode.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/isolette_results/src/main/data/isolette/Isolette_Data_Model/Status.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/isolette_results/src/main/data/isolette/Isolette_Data_Model/TempWstatus_impl.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/isolette_results/src/main/data/isolette/Isolette_Data_Model/Temp_impl.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/isolette_results/src/main/data/isolette/Isolette_Data_Model/ValueStatus.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/isolette_results/src/main/data/isolette/Isolette_Environment/Heat.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/isolette_results/src/main/data/isolette/Isolette_Environment/Interface_Interaction.scala

file:///Users/belt/devel/sireum/kekinian/tools/jvm/src/test/resources/org/sireum/tools/slangcheck/isolette_results/src/main/art/DataContent.scala

*/

@enum object _artDataContent_DataTypeId {
   "_artEmpty_Id"
   "Isolette_Data_ModelMonitor_Mode_Payload_Id"
   "Isolette_Data_ModelFailure_Flag_impl_Payload_Id"
   "Isolette_Data_ModelTemp_impl_Payload_Id"
   "Isolette_Data_ModelOn_Off_Payload_Id"
   "Isolette_EnvironmentInterface_Interaction_Payload_Id"
   "Isolette_EnvironmentHeat_Payload_Id"
   "Isolette_Data_ModelTempWstatus_impl_Payload_Id"
   "Isolette_Data_ModelStatus_Payload_Id"
   "Isolette_Data_ModelValueStatus_Payload_Id"
   "Isolette_Data_ModelRegulator_Mode_Payload_Id"
   "Base_TypesBoolean_Payload_Id"
   "Base_TypesInteger_Payload_Id"
   "Base_TypesInteger_8_Payload_Id"
   "Base_TypesInteger_16_Payload_Id"
   "Base_TypesInteger_32_Payload_Id"
   "Base_TypesInteger_64_Payload_Id"
   "Base_TypesUnsigned_8_Payload_Id"
   "Base_TypesUnsigned_16_Payload_Id"
   "Base_TypesUnsigned_32_Payload_Id"
   "Base_TypesUnsigned_64_Payload_Id"
   "Base_TypesFloat_Payload_Id"
   "Base_TypesFloat_32_Payload_Id"
   "Base_TypesFloat_64_Payload_Id"
   "Base_TypesCharacter_Payload_Id"
   "Base_TypesString_Payload_Id"
   "Base_TypesBits_Payload_Id"
   "Isolette_Data_ModelPhysicalTemp_impl_Payload_Id"
}

