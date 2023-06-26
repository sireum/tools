// #Sireum

package oa

import org.sireum._
import org.sireum.Random.Gen64

/*
GENERATED FROM

exampleType.scala

DataContent.scala

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

@datatype class Config__artDataContent(attempts: Z, additiveTypeFiltering: B, typeFilter: ISZ[_artDataContent_DataTypeId.Type], filter: art.DataContent => B) {}

@datatype class Config__artEmpty(attempts: Z, filter: art.Empty => B) {}

@datatype class Config_example(attempts: Z, filter: example => B) {}


