// #Sireum
package org.sireum.tools

import org.sireum._
import org.sireum.lang.symbol.Resolver.{NameMap, QName, TypeMap}
import org.sireum.lang.symbol.{GlobalDeclarationResolver, Info, Resolver, TypeInfo}
import org.sireum.lang.tipe.TypeHierarchy
import org.sireum.lang.{ast => AST}
import org.sireum.message.Reporter
import org.sireum.ops.ISZOps

object SlangCheck {
  def gen(fileUris: ISZ[String],
          programs: ISZ[AST.TopUnit.Program],
          reporter: Reporter,
          typeHierarchy: TypeHierarchy): ISZ[(ISZ[String], ST)] = {


    val gdr = GlobalDeclarationResolver(HashSMap.empty, HashSMap.empty, reporter)
    for (p <- programs) {
      gdr.resolveProgram(p) //get all names and types
    }
    val packageName = AST.Util.ids2strings(programs(0).packageName.ids)

    // call the various generators
    var ret: ISZ[(ISZ[String], ST)] = ISZ()

    val t = RanGen(
      gdr.globalNameMap,
      gdr.globalTypeMap,
      packageName,
      reporter,
      fileUris,
      typeHierarchy)

    val c = ConfigGen(
      gdr.globalNameMap,
      gdr.globalTypeMap,
      packageName,
      reporter, fileUris, typeHierarchy)

    val g = GeneratorGen(
      gdr.globalNameMap,
      gdr.globalTypeMap,
      packageName,
      reporter, fileUris, typeHierarchy)

    val h = EnumGen(gdr.globalNameMap, gdr.globalTypeMap, packageName, reporter, fileUris, typeHierarchy)

    ret = ret :+ t.gen()
    reporter.reports((t.reporter.messages))

    ret = ret :+ c.gen()
    reporter.reports((c.reporter.messages))

    ret = ret :+ g.gen()
    reporter.reports((g.reporter.messages))

    ret = ret :+ h.gen()
    reporter.reports((h.reporter.messages))

    return ret
  }

  @strictpure def toSimpleNames(fileUris: ISZ[String]): ISZ[String] = for (uri <- fileUris) yield ops.ISZOps(ops.StringOps(uri).split(c => c == '/')).last

  @pure def sortedTypes(types: ISZ[TypeInfo]): ISZ[TypeInfo] = {
    // see Resolver.sortedGlobalTypes

    @pure def sortURI(a: String, b: String): B = { return a < b }

    return ISZOps(types).sortWith(Resolver.ltTypeInfo(sortURI _))
  }

  def sortedTyedNames(names: ISZ[AST.Typed.Name]): ISZ[AST.Typed.Name] = {
    return ISZOps(names).sortWith((a: AST.Typed.Name, b: AST.Typed.Name) => s"${a.ids}${a.args}" < s"${b.ids}${b.args}")
  }
}

object SlangCheckTest {
  def gen(fileUris: ISZ[String],
          programs: ISZ[AST.TopUnit.Program],
          reporter: Reporter,
          typeHierarchy: TypeHierarchy): ISZ[(ISZ[String], ST)] = {

    val gdr = GlobalDeclarationResolver(HashSMap.empty, HashSMap.empty, reporter)
    for (p <- programs) {
      gdr.resolveProgram(p) //get all names and types
    }
    val packageName = AST.Util.ids2strings(programs(0).packageName.ids)

    // call the various generators
    var ret: ISZ[(ISZ[String], ST)] = ISZ()

    val t = TestGen(
      gdr.globalNameMap,
      gdr.globalTypeMap,
      packageName,
      reporter, fileUris, typeHierarchy)

    ret = ret :+ t.gen()
    reporter.reports((t.reporter.messages))


    return ret
  }
}

@record class EnumGen(val globalNameMap: NameMap,
                      val globalTypeMap: TypeMap,
                      val packageName: QName,
                      val reporter: Reporter,
                      val fileNames: ISZ[String],
                      val th: TypeHierarchy) {

  var enums: ISZ[ST] = ISZ()

  def gen(): (ISZ[String], ST) = {

    var cleanedTypeMapValues: ISZ[TypeInfo] = ISZ()

    for (v <- th.typeMap.values) {
      val temp = v
      v.posOpt match {
        case Some(v) if ops.ISZOps(fileNames).contains(v.uriOpt.get) =>
          cleanedTypeMapValues = cleanedTypeMapValues :+ temp
        case _ =>
      }
    }

    for (ti <- SlangCheck.sortedTypes(cleanedTypeMapValues)) {
      ti match {
        case ti: TypeInfo.Sig =>
          genSig(ti)
        case _ => {}
      }
    }

    return (packageName :+ "SlangCheckDataTypeId.scala",
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import org.sireum.Random.Gen64
          |
          |/*
          |GENERATED FROM
          |
          |${(SlangCheck.toSimpleNames(fileNames), "\n\n")}
          |
          |*/
          |
          |${(enums, "\n\n")}
          |
          |""")
  }

  def genSig(ti: TypeInfo.Sig): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)
    val leaves: ISZ[AST.Typed.Name] = SlangCheck.sortedTyedNames(th.substLeavesOfType(ti.posOpt, ti.tpe).left)
    val pn = packageName(0)

    var enumNames: ISZ[String] = ISZ()

    for (typ <- SlangCheck.sortedTyedNames(leaves)) {
      val ids = typ.ids
      if (ids(0) == pn) {
        enumNames = enumNames :+ st"\"${(ops.ISZOps(typ.ids).drop(1), "")}_Id\"".render
      } else {
        enumNames = enumNames :+ st"\"_${(typ.ids, "")}_Id\"".render
      }
    }


    enums = enums :+
      st"""@enum object ${adTypeName}_DataTypeId {
          |   ${(enumNames, "\n")}
          |}"""
  }
}

@record class RanGen(val globalNameMap: NameMap,
                     val globalTypeMap: TypeMap,
                     val packageName: QName,
                     val reporter: Reporter,
                     val fileNames: ISZ[String],
                     val th: TypeHierarchy) {

  //list of all nextMethods
  var nextMethods: ISZ[ST] = ISZ()

  //list of all base configs for each type
  var nextConfig: ISZ[ST] = ISZ()

  //base slang types
  var slangTypes: ISZ[String] = ISZ("Z", "B", "C", "R", "F32", "F64", "S8", "S16", "S32", "S64", "U8", "U16", "U32", "U64")

  //get the next function for the slang types
  var slangTypeRand: ISZ[ST] = for (p <- slangTypes) yield genNextSlangBaseTypes(p)

  //get the base config for the slang type
  var slangTypeConf: ISZ[ST] = for (p <- slangTypes) yield genConfigSlangBaseType(p)


  def gen(): (ISZ[String], ST) = {

    var cleanedTypeMapValues: ISZ[TypeInfo] = ISZ()

    for (v <- th.typeMap.values) {
      val temp = v
      v.posOpt match {
        case Some(v) if ops.ISZOps(fileNames).contains(v.uriOpt.get) =>
          cleanedTypeMapValues = cleanedTypeMapValues :+ temp
        case _ =>
      }
    }

    for (ti <- SlangCheck.sortedTypes(cleanedTypeMapValues)) {
      ti match {
        case ti: TypeInfo.Adt =>
          genAdt(ti)
        case ti: TypeInfo.Enum =>
          genEnum(ti)
        case ti: TypeInfo.Sig =>
          genSig(ti)
        case _ => {}
      }
    }

    return (packageName :+ "SlangCheckRandom.scala",
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import org.sireum.Random.Gen64
          |
          |/*
          |GENERATED FROM
          |
          |${(SlangCheck.toSimpleNames(fileNames), "\n\n")}
          |
          |*/
          |
          |@msig trait RandomLibI {
          |  def gen: org.sireum.Random.Gen
          |
          |  def get_Size: Z
          |  def set_Size(s: Z): Unit
          |
          |  ${(slangTypeRand, "\n\n")}
          |
          |  def nextString(): String = {
          |    val length: Z = gen.nextZBetween(0, get_Size)
          |    var str: String = ""
          |    for(r <- 0 until length){
          |      str = s"$${str}$${gen.nextC().string}"
          |    }
          |
          |    return str
          |  }
          |
          |  ${(nextMethods, "\n\n")}
          |}
          |
          |@record class RandomLib(val gen: org.sireum.Random.Gen) extends RandomLibI {
          |
          |  var size: Z = 50
          |
          |  def get_Size: Z = {return size}
          |
          |  def set_Size(s: Z): Unit ={
          |    size = s
          |  }
          |
          |  ${(slangTypeConf, "\n\n")}
          |
          |  ${(nextConfig, "\n\n")}
          |}
          |
          |""")
  }

  // get the next function for the slang base types
  def genNextSlangBaseTypes(typ: String): ST = {
    if (th.isSubZName(ISZ("org", "sireum", typ))) {
      return (
        st"""// ========  ${typ} ==========
            |  def get_Config_${typ}: Config_${typ}
            |  def set_Config_${typ}(config: Config_${typ}): Unit
            |
            |  def nextISZ$typ(): ISZ[$typ] = {
            |   val length: Z = gen.nextZBetween(0, get_Size)
            |      var temp: ISZ[$typ] = ISZ()
            |      for (r <- 0 until length) {
            |        temp = temp :+ next$typ()
            |      }
            |
            |      return temp
            |  }
            |
            |  def next$typ(): $typ = {
            |    val conf = get_Config_$typ
            |
            |    var r: $typ = if (conf.low.isEmpty) {
            |        if (conf.high.isEmpty)
            |          gen.next$typ()
            |        else
            |          gen.next${typ}Between(${typ}.Min, conf.high.get)
            |      } else {
            |        if (conf.high.isEmpty)
            |          gen.next${typ}Between(conf.low.get, ${typ}.Max)
            |        else
            |          gen.next${typ}Between(conf.low.get, conf.high.get)
            |      }
            |
            |    if(get_Config_$typ.attempts >= 0) {
            |     for (i <- 0 to get_Config_$typ.attempts) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       println(s"Retrying for failing value: $$r")
            |       r = if (conf.low.isEmpty) {
            |         if (conf.high.isEmpty)
            |           gen.next$typ()
            |         else
            |            gen.next${typ}Between(${typ}.Min, conf.high.get)
            |        } else {
            |          if (conf.high.isEmpty)
            |            gen.next${typ}Between(conf.low.get, ${typ}.Max)
            |          else
            |           gen.next${typ}Between(conf.low.get, conf.high.get)
            |       }
            |     }
            |    } else {
            |     while(T) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       println(s"Retrying for failing value: $$r")
            |       r = if (conf.low.isEmpty) {
            |         if (conf.high.isEmpty)
            |           gen.next$typ()
            |         else
            |            gen.next${typ}Between(${typ}.Min, conf.high.get)
            |        } else {
            |          if (conf.high.isEmpty)
            |            gen.next${typ}Between(conf.low.get, ${typ}.Max)
            |          else
            |           gen.next${typ}Between(conf.low.get, conf.high.get)
            |       }
            |     }
            |    }
            |    assert(F, "Requirements to strict to generate")
            |    halt("Requirements to strict to generate")
            |  }
            |
            |  def nextOption$typ(): Option[$typ] = {
            |     val none: Z = gen.nextZBetween(0,1)
            |
            |     if(none == 0)
            |       return Some(next${typ}())
            |     else
            |       return None()
            |  }""")
    } else if (typ == "Z") {
      return (
        st"""// ========  ${typ} ==========
            |  def get_Config_${typ}: Config_${typ}
            |  def set_Config_${typ}(config: Config_${typ}): Unit
            |
            |  def nextISZ$typ(): ISZ[$typ] = {
            |   val length: Z = gen.nextZBetween(0, get_Size)
            |      var temp: ISZ[$typ] = ISZ()
            |      for (r <- 0 until length) {
            |        temp = temp :+ next$typ()
            |      }
            |
            |      return temp
            |  }
            |
            |  def next$typ(): $typ = {
            |    val conf = get_Config_$typ
            |
            |    var r: $typ = if (conf.low.isEmpty) {
            |        if (conf.high.isEmpty)
            |          gen.next$typ()
            |        else
            |          gen.next${typ}Between(S64.Min.toZ, conf.high.get)
            |      } else {
            |        if (conf.high.isEmpty)
            |          gen.next${typ}Between(conf.low.get, S64.Max.toZ)
            |        else
            |          gen.next${typ}Between(conf.low.get, conf.high.get)
            |      }
            |
            |    if(get_Config_$typ.attempts >= 0) {
            |     for (i <- 0 to get_Config_$typ.attempts) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       println(s"Retrying for failing value: $$r")
            |       r = if (conf.low.isEmpty) {
            |         if (conf.high.isEmpty)
            |           gen.next$typ()
            |         else
            |            gen.next${typ}Between(S64.Min.toZ, conf.high.get)
            |        } else {
            |          if (conf.high.isEmpty)
            |            gen.next${typ}Between(conf.low.get, S64.Max.toZ)
            |          else
            |           gen.next${typ}Between(conf.low.get, conf.high.get)
            |       }
            |     }
            |    } else {
            |     while(T) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       println(s"Retrying for failing value: $$r")
            |       r = if (conf.low.isEmpty) {
            |         if (conf.high.isEmpty)
            |           gen.next$typ()
            |         else
            |            gen.next${typ}Between(S64.Min.toZ, conf.high.get)
            |        } else {
            |          if (conf.high.isEmpty)
            |            gen.next${typ}Between(conf.low.get, S64.Max.toZ)
            |          else
            |           gen.next${typ}Between(conf.low.get, conf.high.get)
            |       }
            |     }
            |    }
            |    assert(F, "Requirements to strict to generate")
            |    halt("Requirements to strict to generate")
            |  }
            |
            |  def nextOption$typ(): Option[$typ] = {
            |     val none: Z = gen.nextZBetween(0,1)
            |
            |     if(none == 0)
            |       return Some(next${typ}())
            |     else
            |       return None()
            |  }""")
    } else if (typ == "F32") {
      return (
        st"""// ========  ${typ} ==========
            |  def get_Config_${typ}: Config_${typ}
            |  def set_Config_${typ}(config: Config_${typ}): Unit
            |
            |  def nextISZ$typ(): ISZ[$typ] = {
            |   val length: Z = gen.nextZBetween(0, get_Size)
            |      var temp: ISZ[$typ] = ISZ()
            |      for (r <- 0 until length) {
            |        temp = temp :+ next$typ()
            |      }
            |
            |      return temp
            |  }
            |
            |  def next$typ(): $typ = {
            |    val conf = get_Config_$typ
            |
            |    var r: $typ = if (conf.low.isEmpty) {
            |        if (conf.high.isEmpty)
            |          gen.next$typ()
            |        else
            |          gen.next${typ}Between(f32"-3.40282347E38f", conf.high.get)
            |      } else {
            |        if (conf.high.isEmpty)
            |          gen.next${typ}Between(conf.low.get, f32"3.4028235E38f")
            |        else
            |          gen.next${typ}Between(conf.low.get, conf.high.get)
            |      }
            |
            |    if(get_Config_$typ.attempts >= 0) {
            |     for (i <- 0 to get_Config_$typ.attempts) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       println(s"Retrying for failing value: $$r")
            |       r = if (conf.low.isEmpty) {
            |         if (conf.high.isEmpty)
            |           gen.next$typ()
            |         else
            |            gen.next${typ}Between(f32"-3.40282347E38f", conf.high.get)
            |        } else {
            |          if (conf.high.isEmpty)
            |            gen.next${typ}Between(conf.low.get, f32"3.4028235E38f")
            |          else
            |           gen.next${typ}Between(conf.low.get, conf.high.get)
            |       }
            |     }
            |    } else {
            |     while(T) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       println(s"Retrying for failing value: $$r")
            |       r = if (conf.low.isEmpty) {
            |         if (conf.high.isEmpty)
            |           gen.next$typ()
            |         else
            |            gen.next${typ}Between(f32"-3.40282347E38f", conf.high.get)
            |        } else {
            |          if (conf.high.isEmpty)
            |            gen.next${typ}Between(conf.low.get, f32"3.4028235E38f")
            |          else
            |           gen.next${typ}Between(conf.low.get, conf.high.get)
            |       }
            |     }
            |    }
            |    assert(F, "Requirements to strict to generate")
            |    halt("Requirements to strict to generate")
            |  }
            |
            |  def nextOption$typ(): Option[$typ] = {
            |     val none: Z = gen.nextZBetween(0,1)
            |
            |     if(none == 0)
            |       return Some(next${typ}())
            |     else
            |       return None()
            |  }""")
    } else if (typ == "F64") {
      return (
        st"""// ========  ${typ} ==========
            |  def get_Config_${typ}: Config_${typ}
            |  def set_Config_${typ}(config: Config_${typ}): Unit
            |
            |  def nextISZ$typ(): ISZ[$typ] = {
            |   val length: Z = gen.nextZBetween(0, get_Size)
            |      var temp: ISZ[$typ] = ISZ()
            |      for (r <- 0 until length) {
            |        temp = temp :+ next$typ()
            |      }
            |
            |      return temp
            |  }
            |
            |  def next$typ(): $typ = {
            |    val conf = get_Config_$typ
            |
            |    var r: $typ = if (conf.low.isEmpty) {
            |        if (conf.high.isEmpty)
            |          gen.next$typ()
            |        else
            |          gen.next${typ}Between(f64"-1.7976931348623157E308f", conf.high.get)
            |      } else {
            |        if (conf.high.isEmpty)
            |          gen.next${typ}Between(conf.low.get, f64"1.7976931348623157E308f")
            |        else
            |          gen.next${typ}Between(conf.low.get, conf.high.get)
            |      }
            |
            |    if(get_Config_$typ.attempts >= 0) {
            |     for (i <- 0 to get_Config_$typ.attempts) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       println(s"Retrying for failing value: $$r")
            |       r = if (conf.low.isEmpty) {
            |         if (conf.high.isEmpty)
            |           gen.next$typ()
            |         else
            |            gen.next${typ}Between(f64"-1.7976931348623157E308f", conf.high.get)
            |        } else {
            |          if (conf.high.isEmpty)
            |            gen.next${typ}Between(conf.low.get, f64"1.7976931348623157E308f")
            |          else
            |           gen.next${typ}Between(conf.low.get, conf.high.get)
            |       }
            |     }
            |    } else {
            |     while(T) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       println(s"Retrying for failing value: $$r")
            |       r = if (conf.low.isEmpty) {
            |         if (conf.high.isEmpty)
            |           gen.next$typ()
            |         else
            |            gen.next${typ}Between(f64"-1.7976931348623157E308f", conf.high.get)
            |        } else {
            |          if (conf.high.isEmpty)
            |            gen.next${typ}Between(conf.low.get, f64"1.7976931348623157E308f")
            |          else
            |           gen.next${typ}Between(conf.low.get, conf.high.get)
            |       }
            |     }
            |    }
            |    assert(F, "Requirements to strict to generate")
            |    halt("Requirements to strict to generate")
            |  }
            |
            |  def nextOption$typ(): Option[$typ] = {
            |     val none: Z = gen.nextZBetween(0,1)
            |
            |     if(none == 0)
            |       return Some(next${typ}())
            |     else
            |       return None()
            |  }""")
    } else if (typ == "R") {
      return (
        st"""// ========  ${typ} ==========
            |  def get_Config_${typ}: Config_${typ}
            |  def set_Config_${typ}(config: Config_${typ}): Unit
            |
            |  def nextISZ$typ(): ISZ[$typ] = {
            |   val length: Z = gen.nextZBetween(0, get_Size)
            |      var temp: ISZ[$typ] = ISZ()
            |      for (r <- 0 until length) {
            |        temp = temp :+ next$typ()
            |      }
            |
            |      return temp
            |  }
            |
            |  def next$typ(): $typ = {
            |    val conf = get_Config_$typ
            |
            |    var r: $typ = if (conf.low.isEmpty) {
            |        if (conf.high.isEmpty)
            |          gen.next$typ()
            |        else
            |          gen.next${typ}Between(r"-1.7976931348623157E308", conf.high.get)
            |      } else {
            |        if (conf.high.isEmpty)
            |          gen.next${typ}Between(conf.low.get, r"1.7976931348623157E308")
            |        else
            |          gen.next${typ}Between(conf.low.get, conf.high.get)
            |      }
            |
            |    if(get_Config_$typ.attempts >= 0) {
            |     for (i <- 0 to get_Config_$typ.attempts) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       println(s"Retrying for failing value: $$r")
            |       r = if (conf.low.isEmpty) {
            |         if (conf.high.isEmpty)
            |           gen.next$typ()
            |         else
            |            gen.next${typ}Between(r"-1.7976931348623157E308", conf.high.get)
            |        } else {
            |          if (conf.high.isEmpty)
            |            gen.next${typ}Between(conf.low.get, r"1.7976931348623157E308")
            |          else
            |           gen.next${typ}Between(conf.low.get, conf.high.get)
            |       }
            |     }
            |    } else {
            |     while(T) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       println(s"Retrying for failing value: $$r")
            |       r = if (conf.low.isEmpty) {
            |         if (conf.high.isEmpty)
            |           gen.next$typ()
            |         else
            |            gen.next${typ}Between(r"-1.7976931348623157E308", conf.high.get)
            |        } else {
            |          if (conf.high.isEmpty)
            |            gen.next${typ}Between(conf.low.get, r"1.7976931348623157E308")
            |          else
            |           gen.next${typ}Between(conf.low.get, conf.high.get)
            |       }
            |     }
            |    }
            |    assert(F, "Requirements to strict to generate")
            |    halt("Requirements to strict to generate")
            |  }
            |
            |  def nextOption$typ(): Option[$typ] = {
            |     val none: Z = gen.nextZBetween(0,1)
            |
            |     if(none == 0)
            |       return Some(next${typ}())
            |     else
            |       return None()
            |  }""")
    } else {
      return (
        st"""// ========  ${typ} ==========
            |  def get_Config_${typ}: Config_${typ}
            |  def set_Config_${typ}(config: Config_${typ}): Unit
            |
            |  def nextISZ$typ(): ISZ[$typ] = {
            |   val length: Z = gen.nextZBetween(0, get_Size)
            |      var temp: ISZ[$typ] = ISZ()
            |      for (r <- 0 until length) {
            |        temp = temp :+ next$typ()
            |      }
            |
            |      return temp
            |  }
            |
            |  def next$typ(): $typ = {
            |    var r = gen.next$typ()
            |    if(get_Config_$typ.attempts >= 0) {
            |     for (i <- 0 to get_Config_$typ.attempts) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       println(s"Retrying for failing value: $$r")
            |       r = gen.next$typ()
            |     }
            |    } else {
            |     while(T) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       println(s"Retrying for failing value: $$r")
            |       r = gen.next$typ()
            |     }
            |    }
            |    assert(F, "Requirements to strict to generate")
            |    halt("Requirements to strict to generate")
            |  }
            |
            |  def nextOption$typ(): Option[$typ] = {
            |     val none: Z = gen.nextZBetween(0,1)
            |
            |     if(none == 0)
            |       return Some(next${typ}())
            |     else
            |       return None()
            |  }""")

    }
  }

  // get the base definition for the config for the Slang Base Types
  def genConfigSlangBaseType(typ: String): ST = {
    if (th.isSubZName(ISZ("org", "sireum", typ)) || typ == "F32" || typ == "F64" || typ == "Z" || typ == "R") {
      return (
        st"""// ============= ${typ} ===================
            |def alwaysTrue_$typ(v: $typ): B = {return T}
            |
            |var config_${typ}: Config_${typ} = Config_$typ(None(), None(), 100, alwaysTrue_$typ _)
            |def get_Config_${typ}: Config_${typ} = {return config_${typ}}
            |
            |def set_Config_${typ}(config: Config_${typ}): Unit ={
            |  config_${typ} = config
            |}""")
    } else {
      return (
        st"""// ============= ${typ} ===================
            |def alwaysTrue_$typ(v: $typ): B = {return T}
            |
            |var config_${typ}: Config_${typ} = Config_$typ(100, alwaysTrue_$typ _)
            |def get_Config_${typ}: Config_${typ} = {return config_${typ}}
            |
            |def set_Config_${typ}(config: Config_${typ}): Unit ={
            |  config_${typ} = config
            |}""")

    }
  }

  //get the initial call to the args' next functions
  def genVar(v: Info.Var): ST = {
    val typ: ISZ[String] = v.ast.tipeOpt match {
      case Some(t: AST.Type.Named) =>
        t.name.ids.map(m => m.value)
      case x => halt(s"Probably infeasible: $x")
    }

    val rs: ST = v.ast.tipeOpt match {
      case Some(t: AST.Type.Named) =>
        if (t.typeArgs.nonEmpty) {
          st"var ${v.ast.id.value}: ${(typ, ".")}[${(t.typeArgs, "")}] = next${(typ, "")}${(t.typeArgs, "")}()"
        }
        else {
          st"var ${v.ast.id.value}: ${(typ, ".")} = next${(typ, "")}()"
        }

      case _ => halt("Probably infeasible")
    }

    return rs

  }

  //get the second call to the args' next functions when args don't meeting the config
  def genVarRepeat(v: Info.Var): ST = {

    val typ: ISZ[String] = v.ast.tipeOpt match {
      case Some(t: AST.Type.Named) => //t.prettyST // use the provided 1pretty printer to emit v's type
        t.name.ids.map(m => m.value)
      case _ => halt("Probably infeasible")
    }

    val rs: ST = v.ast.tipeOpt match {
      case Some(t: AST.Type.Named) =>
        if (t.typeArgs.nonEmpty) {
          st"${v.ast.id.value} = next${(typ, "")}${(t.typeArgs, "")}()"
        }
        else {
          st"${v.ast.id.value} = next${(typ, "")}()"
        }

      case _ => halt("Probably infeasible")
    }

    return rs
  }

  //get variable names to be passed as arguments
  def genArgs(v: Info.Var): ST = {
    return st"${v.ast.id.value}"
  }

  //get the type of an enum without the .Type at the end
  def genShortEnumType(str: ISZ[String]): ST = {
    var newStr = ISZOps(str).drop(1)
    newStr = ISZOps(str).dropRight(1)

    return st"${(newStr, ".")}"
  }

  //get next methods and base configs for an enum type
  def genEnum(ti: TypeInfo.Enum): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    nextConfig = nextConfig :+
      st"""// ============= ${adTypeString} ===================
          |def alwaysTrue_$adTypeName(v: $adTypeString): B = {return T}
          |
          |var config_${adTypeName}: Config_${adTypeName} = Config_$adTypeName(100, alwaysTrue_$adTypeName _)
          |
          |def get_Config_${adTypeName}: Config_${adTypeName} = {return config_${adTypeName}}
          |
          |def set_Config_${adTypeName}(config: Config_${adTypeName}): Unit ={
          |  config_${adTypeName} = config
          |}"""


    nextMethods = nextMethods :+
      st"""// ============= ${adTypeString} ===================
          |
          |def get_Config_${adTypeName}: Config_${adTypeName}
          |def set_Config_${adTypeName}(config: Config_${adTypeName}): Unit
          |
          |def next${adTypeName}(): ${adTypeString} = {
          |
          |  var ordinal: Z = gen.nextZBetween(0, ${genShortEnumType(ti.name)}.numOfElements-1)
          |
          |  var v: ${adTypeString} = ${genShortEnumType(ti.name)}.byOrdinal(ordinal).get
          |  if(get_Config_${adTypeName}.attempts >= 0) {
          |   for(i <- 0 to get_Config_${adTypeName}.attempts) {
          |     if(get_Config_${adTypeName}.filter(v)) {
          |      return v
          |     }
          |     println(s"Retrying for failing value: $$v")
          |     ordinal= gen.nextZBetween(0, ${genShortEnumType(ti.name)}.numOfElements-1)
          |     v = ${genShortEnumType(ti.name)}.byOrdinal(ordinal).get
          |   }
          |  } else {
          |   while(T){
          |     if(get_Config_${adTypeName}.filter(v)) {
          |      return v
          |     }
          |     println(s"Retrying for failing value: $$v")
          |     ordinal= gen.nextZBetween(0, ${genShortEnumType(ti.name)}.numOfElements-1)
          |     v = ${genShortEnumType(ti.name)}.byOrdinal(ordinal).get
          |   }
          |  }
          |  assert(F, "Requirements to strict to generate")
          |  halt("Requirements to strict to generate")
          |}
          |
          |def nextOption${adTypeName}(): Option[${adTypeString}] = {
          |  val none: Z = gen.nextZBetween(0,1)
          |
          |  if(none == 0)
          |   return Some(next${adTypeName}())
          |  else
          |   return None()
          |}"""
  }

  def genSig(ti: TypeInfo.Sig): Unit = {

    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)
    val leaves: ISZ[AST.Typed.Name] = SlangCheck.sortedTyedNames(th.substLeavesOfType(ti.posOpt, ti.tpe).left)
    val pn = packageName(0)

    var calls: ISZ[String] = ISZ()
    var enumNames: ISZ[String] = ISZ()
    var cases: ISZ[String] = ISZ()

    for (typ <- SlangCheck.sortedTyedNames(leaves)) {
      val ids = typ.ids
      if (ids(0) == pn) {
        calls = calls :+ st"next${(ops.ISZOps(typ.ids).drop(1), "")}".render
        enumNames = enumNames :+ st"${adTypeName}_DataTypeId.${(ops.ISZOps(typ.ids).drop(1), "")}_Id".render
      } else {
        calls = calls :+ st"next_${(typ.ids, "")}".render
        enumNames = enumNames :+ st"${adTypeName}_DataTypeId._${(typ.ids, "")}_Id".render
      }
    }

    for (i <- 0 to enumNames.size - 1) {
      cases = cases :+ s"case ${enumNames(i)} => (${calls(i)} _).apply()"
    }

    nextConfig = nextConfig :+
      st"""// ============= ${adTypeString} ===================
          |def alwaysTrue_$adTypeName(v: $adTypeString): B = {return T}
          |
          |var config_${adTypeName}: Config_${adTypeName} = Config_$adTypeName(100, F, ISZ(), alwaysTrue_$adTypeName _)
          |
          |def get_Config_${adTypeName}: Config_${adTypeName} = {return config_${adTypeName}}
          |
          |def set_Config_${adTypeName}(config: Config_${adTypeName}): Unit ={
          |  config_${adTypeName} = config
          |}"""

    nextMethods = nextMethods :+
      st"""// ============= ${adTypeString} ===================
          |
          |def get_Config_${adTypeName}: Config_${adTypeName}
          |def set_Config_${adTypeName}(config: Config_${adTypeName}): Unit
          |
          |def nextISZ$adTypeName(): ISZ[$adTypeString] = {
          |  val length: Z = gen.nextZBetween(0, get_Size)
          |  var temp: ISZ[$adTypeString] = ISZ()
          |  for (r <- 0 until length) {
          |    temp = temp :+ next$adTypeName()
          |  }
          |
          |  return temp
          |}
          |
          |def next${adTypeName}(): ${adTypeString} = {
          |  var callEnum: ISZ[${adTypeName}_DataTypeId.Type] = ISZ(${(enumNames, ", ")})
          |
          |  if(get_Config_${adTypeName}.additiveTypeFiltering) {
          |     callEnum = get_Config_${adTypeName}.typeFilter
          |  } else {
          |     for(h <- get_Config_${adTypeName}.typeFilter) {
          |       callEnum = ops.ISZOps(callEnum).filter(h.=!=)
          |     }
          |  }
          |
          |  var c = callEnum(gen.nextZBetween(0, callEnum.size-1))
          |
          |  var v: ${adTypeString} = c match {
          |    ${(cases, "\n")}
          |    case _ => halt("Invalid Child")
          |  }
          |
          |
          |  if(get_Config_${adTypeName}.attempts >= 0) {
          |   for(i <- 0 to get_Config_${adTypeName}.attempts) {
          |     if(get_Config_${adTypeName}.filter(v)) {
          |      return v
          |     }
          |     println(s"Retrying for failing value: $$v")
          |     c = callEnum(gen.nextZBetween(0, callEnum.size-1))
          |
          |     v = c match {
          |       ${(cases, "\n")}
          |       case _ => halt("Invalid Child")
          |     }
          |   }
          |  } else {
          |   while(T) {
          |     if(get_Config_${adTypeName}.filter(v)) {
          |       return v
          |     }
          |     println(s"Retrying for failing value: $$v")
          |     c = callEnum(gen.nextZBetween(0, callEnum.size-1))
          |
          |     v = c match {
          |       ${(cases, "\n")}
          |       case _ => halt("Invalid Child")
          |     }
          |   }
          |  }
          |  assert(F, "Requirements to strict to generate")
          |  halt("Requirements to strict to generate")
          |}
          |
          |def nextOption${adTypeName}(): Option[${adTypeString}] = {
          |  val none: Z = gen.nextZBetween(0,1)
          |
          |  if(none == 0)
          |   return Some(next${adTypeName}())
          |  else
          |   return None()
          |}"""
  }

  //get next methods and base configs for a non-enum type
  def genAdt(ti: TypeInfo.Adt): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    val vars: ISZ[ST] = for (v <- ti.vars.values) yield genVar(v)
    val args: ISZ[ST] = for (v <- ti.vars.values) yield genArgs(v)
    val varsRepeat: ISZ[ST] = for (v <- ti.vars.values) yield genVarRepeat(v)


    nextConfig = nextConfig :+
      st"""// ============= ${adTypeString} ===================
          |def alwaysTrue_$adTypeName(v: $adTypeString): B = {return T}
          |
          |var config_${adTypeName}: Config_${adTypeName} = Config_$adTypeName(100, ${if (ti.invariants.nonEmpty) st"${adTypeString}_GumboX.D_Inv_${ISZOps(ti.name).last} _" else st"alwaysTrue_$adTypeName _"})
          |
          |def get_Config_${adTypeName}: Config_${adTypeName} = {return config_${adTypeName}}
          |
          |def set_Config_${adTypeName}(config: Config_${adTypeName}): Unit ={
          |  config_${adTypeName} = config
          |}"""

    nextMethods = nextMethods :+
      st"""// ============= ${adTypeString} ===================
          |
          |def get_Config_${adTypeName}: Config_${adTypeName}
          |def set_Config_${adTypeName}(config: Config_${adTypeName}): Unit
          |
          |def nextISZ$adTypeName(): ISZ[$adTypeString] = {
          |   val length: Z = gen.nextZBetween(0, 256)
          |      var temp: ISZ[$adTypeString] = ISZ()
          |      for (r <- 0 until length) {
          |        temp = temp :+ next$adTypeName()
          |      }
          |
          |      return temp
          |  }
          |
          |def next${adTypeName}(): ${adTypeString} = {
          |  ${(vars, "\n")}
          |
          |  var v: ${adTypeString} = ${adTypeString}(${(args, ", ")})
          |
          |  if(get_Config_${adTypeName}.attempts >= 0) {
          |   for(i <- 0 to get_Config_${adTypeName}.attempts) {
          |      if(get_Config_${adTypeName}.filter(v)) {
          |        return v
          |      }
          |      println(s"Retrying for failing value: $$v")
          |      ${(varsRepeat, "\n")}
          |      v = ${adTypeString}(${(args, ", ")})
          |   }
          |  } else {
          |   while(T) {
          |     if(get_Config_${adTypeName}.filter(v)) {
          |       return v
          |     }
          |     println(s"Retrying for failing value: $$v")
          |     ${(varsRepeat, "\n")}
          |     v = ${adTypeString}(${(args, ", ")})
          |   }
          |  }
          |
          |  assert(F, "Requirements to strict to generate")
          |  halt("Requirements to strict to generate")
          |}
          |
          |def nextOption${adTypeName}(): Option[${adTypeString}] = {
          |  val none: Z = gen.nextZBetween(0,1)
          |
          |  if(none == 0)
          |   return Some(next${adTypeName}())
          |  else
          |   return None()
          |}"""
  }
}

@record class ConfigGen(val globalNameMap: NameMap,
                        val globalTypeMap: TypeMap,
                        val packageName: QName,
                        val reporter: Reporter,
                        val fileNames: ISZ[String],
                        val th: TypeHierarchy) {

  val globalTypes: ISZ[TypeInfo] = Resolver.sortedGlobalTypes(globalTypeMap)

  var slangTypes: ISZ[String] = ISZ("Z", "B", "C", "R", "F32", "F64", "S8", "S16", "S32", "S64", "U8", "U16", "U32", "U64")
  var slangTypeConf: ISZ[ST] = for (p <- slangTypes) yield genSlangBaseTypes(p)

  var nextConfig: ISZ[ST] = ISZ()

  def gen(): (ISZ[String], ST) = {
    var cleanedTypeMapValues: ISZ[TypeInfo] = ISZ()

    for (v <- th.typeMap.values) {
      val temp = v
      v.posOpt match {
        case Some(v) if ops.ISZOps(fileNames).contains(v.uriOpt.get) =>
          cleanedTypeMapValues = cleanedTypeMapValues :+ temp
        case _ =>
      }
    }

    for (ti <- SlangCheck.sortedTypes(cleanedTypeMapValues)) {
      ti match {
        case ti: TypeInfo.Adt =>
          genAdt(ti)
        case ti: TypeInfo.Enum =>
          genEnum(ti)
        case ti: TypeInfo.Sig =>
          genSig(ti)
        case _ => {}
      }
    }

    return (packageName :+ "SlangCheckConfig.scala",
      st"""// #Sireum
          |
          |package ${packageName}
          |
          |import org.sireum._
          |import org.sireum.Random.Gen64
          |
          |/*
          |GENERATED FROM
          |
          |${(SlangCheck.toSimpleNames(fileNames), "\n\n")}
          |
          |*/
          |
          |${(slangTypeConf, "\n\n")}
          |
          |${(nextConfig, "\n\n")}
          |
          |
          |""")
  }

  //get config datatype for enum
  def genEnum(ti: TypeInfo.Enum): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    nextConfig = nextConfig :+
      st"""@datatype class Config_${adTypeName}(attempts: Z, filter: ${adTypeString} => B) {}"""
  }

  //get config type for a slang type
  def genSlangBaseTypes(typ: String): ST = {
    return if (th.isSubZName(ISZ("org", "sireum", typ)) || typ == "F32" || typ == "F64" || typ == "R" || typ == "Z")
      st"""@datatype class Config_${typ}(low: Option[$typ], high: Option[$typ], attempts: Z, filter: ${typ} => B) {}"""
    else
      st"""@datatype class Config_${typ}(attempts: Z, filter: ${typ} => B) {}"""

  }

  //get config type for everything else
  def genAdt(ti: TypeInfo.Adt): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    nextConfig = nextConfig :+
      st"""@datatype class Config_${adTypeName}(attempts: Z, filter: ${adTypeString} => B) {}"""
  }

  def genSig(ti: TypeInfo.Sig): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    nextConfig = nextConfig :+
      st"""@datatype class Config_${adTypeName}(attempts: Z, additiveTypeFiltering: B, typeFilter: ISZ[${adTypeName}_DataTypeId.Type], filter: ${adTypeString} => B) {}"""
  }
}

@record class GeneratorGen(val globalNameMap: NameMap,
                           val globalTypeMap: TypeMap,
                           val packageName: QName,
                           val reporter: Reporter,
                           val fileNames: ISZ[String], val th: TypeHierarchy) {

  var slangTypes: ISZ[String] = ISZ("Z", "B", "C", "R", "F32", "F64", "S8", "S16", "S32", "S64", "U8", "U16", "U32", "U64")
  var slangTypeGen: ISZ[ST] = for (p <- slangTypes) yield genSlangType(p)

  var nextClass: ISZ[ST] = ISZ()

  def gen(): (ISZ[String], ST) = {
    var cleanedTypeMapValues: ISZ[TypeInfo] = ISZ()

    for (v <- th.typeMap.values) {
      // println(v.posOpt)
      val temp = v
      v.posOpt match {
        case Some(v) if ops.ISZOps(fileNames).contains(v.uriOpt.get) =>
          cleanedTypeMapValues = cleanedTypeMapValues :+ temp
        case _ =>
      }
    }

    for (ti <- SlangCheck.sortedTypes(cleanedTypeMapValues)) {
      ti match {
        case ti: TypeInfo.Adt =>
          genAdt(ti)
        case ti: TypeInfo.Enum =>
          genEnum(ti)
        case ti: TypeInfo.Sig =>
          genSig(ti)
        case _ => {}
      }
    }
    return (packageName :+ "SlangCheckGenerator.scala",
      st"""// #Sireum
          |
          |package ${packageName}
          |
          |import org.sireum._
          |import org.sireum.Random.Gen64
          |
          |/*
          |GENERATED FROM
          |
          |${(SlangCheck.toSimpleNames(fileNames), "\n\n")}
          |
          |*/
          |
          |${(slangTypeGen, "\n\n")}
          |
          |
          |${(nextClass, "\n\n")}
          |
          |
          |""")
  }

  //get generator for an enum
  def genEnum(ti: TypeInfo.Enum): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    nextClass = nextClass :+
      st"""@record class Gen_${adTypeName}(param: RandomLibI) extends MJen[${adTypeString}] {
          |  override def generate(f: ${adTypeString} => Jen.Action): Jen.Action = {
          |    var continue = Jen.Continue
          |    while (T) {
          |
          |      continue = f(param.next${adTypeName}())
          |
          |      if (!continue) {
          |        return Jen.End
          |      }
          |    }
          |    return continue
          |  }
          |
          |  override def string: String = {
          |    return s""
          |  }
          |}"""
  }

  //get a generator for a slang type
  def genSlangType(typ: String): ST = {
    return (
      st"""@record class Gen_${typ}(param: RandomLibI) extends MJen[${typ}] {
          |  override def generate(f: ${typ} => Jen.Action): Jen.Action = {
          |    var continue = Jen.Continue
          |    while (T) {
          |
          |      continue = f(param.next${typ}())
          |
          |      if (!continue) {
          |        return Jen.End
          |      }
          |    }
          |    return continue
          |  }
          |
          |  override def string: String = {
          |    return s""
          |  }
          |}""")
  }

  //get a generator for a everything else
  def genAdt(ti: TypeInfo.Adt): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    nextClass = nextClass :+
      st"""@record class Gen_${adTypeName}(param: RandomLibI) extends MJen[${adTypeString}] {
          |  override def generate(f: ${adTypeString} => Jen.Action): Jen.Action = {
          |    var continue = Jen.Continue
          |    while (T) {
          |
          |      continue = f(param.next${adTypeName}())
          |
          |      if (!continue) {
          |        return Jen.End
          |      }
          |    }
          |    return continue
          |  }
          |
          |  override def string: String = {
          |    return s""
          |  }
          |}"""
  }

  def genSig(ti: TypeInfo.Sig): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    nextClass = nextClass :+
      st"""@record class Gen_${adTypeName}(param: RandomLibI) extends MJen[${adTypeString}] {
          |  override def generate(f: ${adTypeString} => Jen.Action): Jen.Action = {
          |    var continue = Jen.Continue
          |    while (T) {
          |
          |      continue = f(param.next${adTypeName}())
          |
          |      if (!continue) {
          |        return Jen.End
          |      }
          |    }
          |    return continue
          |  }
          |
          |  override def string: String = {
          |    return s""
          |  }
          |}"""
  }
}

@record class TestGen(val globalNameMap: NameMap,
                      val globalTypeMap: TypeMap,
                      val packageName: QName,
                      val reporter: Reporter,
                      val fileNames: ISZ[String],
                      val th: TypeHierarchy) {


  var slangTypes: ISZ[String] = ISZ("Z", "B", "C", "R", "F32", "F64", "S8", "S16", "S32", "S64", "U8", "U16", "U32", "U64")
  var slangTypeGen: ISZ[ST] = for (p <- slangTypes) yield genSlangType(p)

  var nextClass: ISZ[ST] = ISZ()

  def gen(): (ISZ[String], ST) = {
    var cleanedTypeMapValues: ISZ[TypeInfo] = ISZ()

    for (v <- th.typeMap.values) {
      // println(v.posOpt)
      val temp = v
      v.posOpt match {
        case Some(v) if ops.ISZOps(fileNames).contains(v.uriOpt.get) =>
          cleanedTypeMapValues = cleanedTypeMapValues :+ temp
        case _ =>
      }
    }

    for (ti <- SlangCheck.sortedTypes(cleanedTypeMapValues)) {
      ti match {
        case ti: TypeInfo.Adt =>
          genAdt(ti)
        case ti: TypeInfo.Enum =>
          genEnum(ti)
        case ti: TypeInfo.Sig =>
          genSig(ti)
        case _ => {}
      }
    }
    return (packageName :+ "SlangCheckTest.scala",
      st"""package ${packageName}
          |
          |import org.scalatest.funsuite.AnyFunSuite
          |import org.sireum.Random.Impl.Xoshiro256
          |import org.sireum._
          |
          |class autogenTest extends AnyFunSuite{
          |
          |  ${(slangTypeGen, "\n\n")}
          |
          |  ${(nextClass, "\n\n")}
          |
          |}
          |""")
  }

  //get generator for an enum
  def genEnum(ti: TypeInfo.Enum): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    nextClass = nextClass :+
      st"""test("$adTypeString Output") {
          |    val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create))
          |    val gen = Gen_$adTypeName(randomLib)
          |
          |    for(r <- gen.take(100))
          |      println(r)
          |}"""
  }

  //get a generator for a slang type
  def genSlangType(typ: String): ST = {
    return (
      st"""test("$typ Output") {
          |  val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create))
          |  val gen = Gen_$typ(randomLib)
          |
          |  for(r <- gen.take(100))
          |    println(r)
          |}""")
  }

  //get a generator for a everything else
  def genAdt(ti: TypeInfo.Adt): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    nextClass = nextClass :+
      st"""test("$adTypeString Output") {
          |  val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create))
          |  val gen = Gen_$adTypeName(randomLib)
          |
          |  for(r <- gen.take(100))
          |    println(r)
          |}"""
  }

  def genSig(ti: TypeInfo.Sig): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    nextClass = nextClass :+
      st"""test("$adTypeString Output") {
          |  val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create))
          |  val gen = Gen_$adTypeName(randomLib)
          |
          |  for(r <- gen.take(100))
          |    println(r)
          |}"""
  }

}
