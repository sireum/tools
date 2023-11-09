package oa

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

  test("Option[C] Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_OptionC(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

  test("example Output") {
    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
    val gen = Gen_example(randomLib)

    for(r <- gen.take(100))
      println(r)
  }

}
