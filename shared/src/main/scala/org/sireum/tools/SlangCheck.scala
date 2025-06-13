// #Sireum
package org.sireum.tools

import org.sireum._
import org.sireum.lang.symbol.Resolver.{NameMap, QName, TypeMap}
import org.sireum.lang.symbol.{GlobalDeclarationResolver, Info, Resolver, TypeInfo}
import org.sireum.lang.tipe.TypeHierarchy
import org.sireum.lang.{ast => AST}
import org.sireum.message.Reporter
import org.sireum.ops.ISZOps

//TODO: Datatype Traits (Look at Jason's email)

object SlangCheck {
  val toolName: String = "SlangCheck"

  def gen(packageName: ISZ[String],
          fileUris: ISZ[String],
          programs: ISZ[AST.TopUnit.Program],
          reporter: Reporter,
          typeHierarchy: TypeHierarchy): ISZ[(ISZ[String], ST)] = {

    val packageName_ : ISZ[String] = if (packageName.nonEmpty) packageName else AST.Util.ids2strings(programs(0).packageName.ids)

    // call the various generators
    var ret: ISZ[(ISZ[String], ST)] = ISZ()

    val t = RanGen(
      packageName_,
      fileUris,
      typeHierarchy)

    val c = ConfigGen(
      packageName_,
      fileUris,
      typeHierarchy)

    val g = GeneratorGen(
      packageName,
      fileUris,
      typeHierarchy)

    val h = EnumGen(
      packageName_,
      fileUris,
      typeHierarchy)

    var cleanedTypeMapValues: ISZ[TypeInfo] = ISZ()

    for (v <- typeHierarchy.typeMap.values) {
      val temp = v
      v.posOpt match {
        case Some(v2) if ops.ISZOps(fileUris).contains(v2.uriOpt.get) =>
          cleanedTypeMapValues = cleanedTypeMapValues :+ temp
        case _ =>
      }
    }

    val sortedTypeInfos = sortedTypes(cleanedTypeMapValues)

    ret = ret :+ t.gen(sortedTypeInfos)
    reporter.reports(t.reporter.messages)

    ret = ret :+ c.gen(sortedTypeInfos)
    reporter.reports(c.reporter.messages)

    ret = ret :+ g.gen(sortedTypeInfos)
    reporter.reports(g.reporter.messages)

    ret = ret :+ h.gen(sortedTypeInfos)
    reporter.reports(h.reporter.messages)

    return ret
  }

  @strictpure def toSimpleNames(fileUris: ISZ[String]): ISZ[String] = for (uri <- fileUris) yield ops.ISZOps(ops.StringOps(uri).split(c => c == '/')).last

  @pure def sortedTypes(types: ISZ[TypeInfo]): ISZ[TypeInfo] = {
    // see Resolver.sortedGlobalTypes

    @pure def sortURI(a: String, b: String): B = {
      return a < b
    }

    return ISZOps(types).sortWith(Resolver.ltTypeInfo(sortURI _))
  }

  def sortedTyedNames(names: ISZ[AST.Typed.Name]): ISZ[AST.Typed.Name] = {
    return ISZOps(names).sortWith((a: AST.Typed.Name, b: AST.Typed.Name) => s"${a.ids}${a.args}" < s"${b.ids}${b.args}")
  }

  @pure def astTypeNameString(packageName: ISZ[String], t: AST.Type): ST = {
    t match {
      case atn: AST.Type.Named =>
        atn.typedOpt match {
          case Some(typed) if builtIn(typed) => return Resolver.typeNameString(packageName, for (i <- atn.name.ids) yield i.value)
          case Some(typed) => return astTypedNameString(packageName, typed)
          case _ => halt("Infeasible")
        }
      case _ => halt(s"Need to handle $t")
    }
  }

  @pure def astTypeName(packageName: ISZ[String], t: AST.Type): ST = {
    t match {
      case atn: AST.Type.Named =>
        atn.typedOpt match {
          case Some(t2) if builtIn(t2) => return Resolver.typeName(packageName, for (i <- atn.name.ids) yield i.value)
          case Some(t2) => return astTypedName(packageName, t2)
          case _ => halt("infeasible")
        }
      case _ => halt(s"Need to handle $t")
    }
  }

  @pure def astTypedNameString(packageName: ISZ[String], t: AST.Typed): ST = {
    t match {
      case atn: AST.Typed.Name => return Resolver.typeNameString(packageName, atn.ids)
      case _ => halt(s"Need to handle $t")
    }
  }

  @pure def astTypedName(packageName: ISZ[String], t: AST.Typed): ST = {

    t match {
      case atn: AST.Typed.Name => return Resolver.typeName(packageName, atn.ids)
      case _ => halt(s"Need to handle $t")
    }
  }

  def builtIn(typed: AST.Typed): B = {
    typed match {
      case AST.Typed.Name(ISZ("org", "sireum", _), _) => return T
      case _ => return F
    }
  }
}

object SlangCheckTest {
  def gen(packageName: ISZ[String],
          fileUris: ISZ[String],
          programs: ISZ[AST.TopUnit.Program],
          reporter: Reporter,
          typeHierarchy: TypeHierarchy): ISZ[(ISZ[String], ST)] = {

    val gdr = GlobalDeclarationResolver(HashSMap.empty, HashSMap.empty, reporter)
    for (p <- programs) {
      gdr.resolveProgram(p) //get all names and types
    }
    val packageName_ : ISZ[String] = if (packageName.nonEmpty) packageName else AST.Util.ids2strings(programs(0).packageName.ids)

    // call the various generators
    var ret: ISZ[(ISZ[String], ST)] = ISZ()

    val t = TestGen(
      packageName_,
      fileUris,
      typeHierarchy)

    ret = ret :+ t.gen()
    reporter.reports(t.reporter.messages)


    return ret
  }
}

@record class EnumGen(val packageName: QName,
                      val fileNames: ISZ[String],
                      val th: TypeHierarchy) {

  val reporter: Reporter = Reporter.create

  var enums: ISZ[ST] = ISZ()

  def gen(sortedTypeInfos: ISZ[TypeInfo]): (ISZ[String], ST) = {

    for (ti <- sortedTypeInfos) {
      ti match {
        case ti: TypeInfo.Sig =>
          genSig(ti)
        case ti: TypeInfo.Adt =>
          genAdt(ti)
        case _ =>
      }
    }

    return (ISZ("SlangCheckDataTypeId.scala"),
      st"""// #Sireum
          |
          |package ${(packageName, ".")}
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

  def genAdt(ti: TypeInfo.Adt): Unit = {

    if (ti.ast.isRoot) {
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

  def genSig(ti: TypeInfo.Sig): Unit = {
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

@record class RanGen(val packageName: QName,
                     val fileNames: ISZ[String],
                     val th: TypeHierarchy) {

  val reporter: Reporter = Reporter.create

  //list of all nextMethods
  var nextMethods: ISZ[ST] = ISZ()

  var extraNextMethods: ISZ[ST] = ISZ()

  //list of all base configs for each type
  var nextConfig: ISZ[ST] = ISZ()

  //base slang types
  var slangTypes: ISZ[String] = ISZ("Z", "B", "C", "R", "F32", "F64", "S8", "S16", "S32", "S64", "U8", "U16", "U32", "U64")

  //get the next function for the slang types
  var slangTypeRand: ISZ[ST] = for (p <- slangTypes) yield genNextSlangBaseTypes(p)

  //get the base config for the slang type
  var slangTypeConf: ISZ[ST] = for (p <- slangTypes) yield genConfigSlangBaseType(p)


  def gen(sortedTypeInfos: ISZ[TypeInfo]): (ISZ[String], ST) = {

    for (ti <- sortedTypeInfos) {
      ti match {
        case ti: TypeInfo.Adt =>
          genAdt(ti)
        case ti: TypeInfo.Enum =>
          genEnum(ti)
        case ti: TypeInfo.Sig =>
          genSig(ti)
        case _ =>
      }
    }

    return (ISZ("SlangCheckRandom.scala"),
      st"""// #Sireum
          |
          |package ${(packageName, ".")}
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
          |  def get_numElement: Z
          |  def set_numElement(s: Z): Unit
          |
          |  ${(slangTypeRand, "\n\n")}
          |
          |  // ============= String ===================
          |
          |  def get_Config_String: Config_String
          |  def set_Config_String(config: Config_String): RandomLib
          |
          |  def nextString(): String = {
          |
          |    var length: Z = gen.nextZBetween(get_Config_String.minSize, get_Config_String.maxSize)
          |    var str: String = ""
          |    for(r <- 0 until length){
          |      str = s"$${str}$${nextC().string}"
          |    }
          |
          |    if(get_Config_String.attempts >= 0) {
          |      for (i <- 0 to get_Config_String.attempts) {
          |        if(get_Config_String.filter(str)) {
          |          return str
          |        }
          |        if(get_Config_String.verbose) {
          |          println(s"Retrying for failing value: $$str")
          |        }
          |
          |        length = gen.nextZBetween(get_Config_String.minSize, get_Config_String.maxSize)
          |        str = ""
          |        for (r <- 0 until length) {
          |          str = s"$${str}$${nextC().string}"
          |        }
          |      }
          |    } else {
          |      while(T) {
          |        if (get_Config_String.filter(str)) {
          |          return str
          |        }
          |        if (get_Config_String.verbose) {
          |          println(s"Retrying for failing value: $$str")
          |        }
          |
          |        length = gen.nextZBetween(get_Config_String.minSize, get_Config_String.maxSize)
          |        str = ""
          |        for (r <- 0 until length) {
          |          str = s"$${str}$${nextC().string}"
          |        }
          |      }
          |    }
          |    assert(F, "Requirements too strict to generate")
          |    halt("Requirements too strict to generate")
          |  }
          |
          |  ${(nextMethods, "\n\n")}
          |
          |}
          |
          |@record class RandomLib(val gen: org.sireum.Random.Gen) extends RandomLibI {
          |
          |  var numElem: Z = 50
          |
          |  var _verbose: B = F
          |  def verbose: RandomLib = {
          |    _verbose = !_verbose
          |    return this
          |  }
          |
          |  def get_numElement: Z = {return numElem}
          |
          |  def set_numElement(s: Z): Unit ={
          |    numElem = s
          |  }
          |
          |  // ============= String =============
          |
          |  def alwaysTrue_String(v: String): B = {return T}
          |
          |  var config_String: Config_String = Config_String(0, numElem, 100, _verbose, alwaysTrue_String _)
          |
          |  def get_Config_String: Config_String = {return config_String}
          |
          |  def set_Config_String(config: Config_String): RandomLib = {
          |    config_String = config
          |    return this
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
            |  def set_Config_${typ}(config: Config_${typ}): RandomLib
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
            |       if (get_Config_$typ.verbose) {
            |         println(s"Retrying for failing value: $$r")
            |       }
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
            |       if (get_Config_$typ.verbose) {
            |         println(s"Retrying for failing value: $$r")
            |       }
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
            |    assert(F, "Requirements too strict to generate")
            |    halt("Requirements too strict to generate")
            |  }""")
    } else if (typ == "Z") {
      return (
        st"""// ========  ${typ} ==========
            |  def get_Config_${typ}: Config_${typ}
            |  def set_Config_${typ}(config: Config_${typ}): RandomLib
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
            |       if (get_Config_$typ.verbose) {
            |         println(s"Retrying for failing value: $$r")
            |       }
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
            |       if (get_Config_$typ.verbose) {
            |         println(s"Retrying for failing value: $$r")
            |       }
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
            |    assert(F, "Requirements too strict to generate")
            |    halt("Requirements too strict to generate")
            |  }""")
    } else if (typ == "C") {
      return (
        st"""// ========  ${typ} ==========
            |  def get_Config_${typ}: Config_${typ}
            |  def set_Config_${typ}(config: Config_${typ}): RandomLib
            |
            |  def next$typ(): $typ = {
            |    val conf = get_Config_$typ
            |
            |    var r: $typ = if (conf.low.isEmpty) {
            |        if (conf.high.isEmpty)
            |          gen.next$typ()
            |        else
            |          gen.next${typ}Between(C.fromZ(0), conf.high.get)
            |      } else {
            |        if (conf.high.isEmpty)
            |          gen.next${typ}Between(conf.low.get, C.fromZ(1114111))
            |        else
            |          gen.next${typ}Between(conf.low.get, conf.high.get)
            |      }
            |
            |    if(get_Config_$typ.attempts >= 0) {
            |     for (i <- 0 to get_Config_$typ.attempts) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       if (get_Config_$typ.verbose) {
            |         println(s"Retrying for failing value: $$r")
            |       }
            |       r = if (conf.low.isEmpty) {
            |         if (conf.high.isEmpty)
            |           gen.next$typ()
            |         else
            |            gen.next${typ}Between(C.fromZ(0), conf.high.get)
            |        } else {
            |          if (conf.high.isEmpty)
            |            gen.next${typ}Between(conf.low.get, C.fromZ(1114111))
            |          else
            |           gen.next${typ}Between(conf.low.get, conf.high.get)
            |       }
            |     }
            |    } else {
            |     while(T) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       if (get_Config_$typ.verbose) {
            |         println(s"Retrying for failing value: $$r")
            |       }
            |       r = if (conf.low.isEmpty) {
            |         if (conf.high.isEmpty)
            |           gen.next$typ()
            |         else
            |            gen.next${typ}Between(C.fromZ(0), conf.high.get)
            |        } else {
            |          if (conf.high.isEmpty)
            |            gen.next${typ}Between(conf.low.get, C.fromZ(1114111))
            |          else
            |           gen.next${typ}Between(conf.low.get, conf.high.get)
            |       }
            |     }
            |    }
            |    assert(F, "Requirements too strict to generate")
            |    halt("Requirements too strict to generate")
            |  }""")
    } else if (typ == "F32") {
      return (
        st"""// ========  ${typ} ==========
            |  def get_Config_${typ}: Config_${typ}
            |  def set_Config_${typ}(config: Config_${typ}): RandomLib
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
            |       if (get_Config_$typ.verbose) {
            |         println(s"Retrying for failing value: $$r")
            |       }
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
            |       if (get_Config_$typ.verbose) {
            |         println(s"Retrying for failing value: $$r")
            |       }
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
            |    assert(F, "Requirements too strict to generate")
            |    halt("Requirements too strict to generate")
            |  }""")
    } else if (typ == "F64") {
      return (
        st"""// ========  ${typ} ==========
            |  def get_Config_${typ}: Config_${typ}
            |  def set_Config_${typ}(config: Config_${typ}): RandomLib
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
            |       if (get_Config_$typ.verbose) {
            |         println(s"Retrying for failing value: $$r")
            |       }
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
            |       if (get_Config_$typ.verbose) {
            |         println(s"Retrying for failing value: $$r")
            |       }
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
            |    assert(F, "Requirements too strict to generate")
            |    halt("Requirements too strict to generate")
            |  }""")
    } else if (typ == "R") {
      return (
        st"""// ========  ${typ} ==========
            |  def get_Config_${typ}: Config_${typ}
            |  def set_Config_${typ}(config: Config_${typ}): RandomLib
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
            |       if (get_Config_$typ.verbose) {
            |         println(s"Retrying for failing value: $$r")
            |       }
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
            |       if (get_Config_$typ.verbose) {
            |         println(s"Retrying for failing value: $$r")
            |       }
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
            |    assert(F, "Requirements too strict to generate")
            |    halt("Requirements too strict to generate")
            |  }""")
    } else {
      return (
        st"""// ========  ${typ} ==========}
            |  def get_Config_${typ}: Config_${typ}
            |  def set_Config_${typ}(config: Config_${typ}): RandomLib
            |
            |  def next$typ(): $typ = {
            |    var r = gen.next$typ()
            |    if(get_Config_$typ.attempts >= 0) {
            |     for (i <- 0 to get_Config_$typ.attempts) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       if (get_Config_$typ.verbose) {
            |         println(s"Retrying for failing value: $$r")
            |       }
            |       r = gen.next$typ()
            |     }
            |    } else {
            |     while(T) {
            |       if (get_Config_$typ.filter(r)) {
            |         return r
            |       }
            |       if (get_Config_$typ.verbose) {
            |         println(s"Retrying for failing value: $$r")
            |       }
            |       r = gen.next$typ()
            |     }
            |    }
            |    assert(F, "Requirements too strict to generate")
            |    halt("Requirements too strict to generate")
            |  }""")

    }
  }

  // get the base definition for the config for the Slang Base Types
  def genConfigSlangBaseType(typ: String): ST = {
    if (th.isSubZName(ISZ("org", "sireum", typ)) || typ == "F32" || typ == "F64" || typ == "Z" || typ == "R" || typ == "C") {
      return (
        st"""// ============= ${typ} ===================
            |def alwaysTrue_$typ(v: $typ): B = {return T}
            |
            |var config_${typ}: Config_${typ} = Config_$typ(None(), None(), 100, _verbose, alwaysTrue_$typ _)
            |def get_Config_${typ}: Config_${typ} = {return config_${typ}}
            |
            |def set_Config_${typ}(config: Config_${typ}): RandomLib ={
            |  config_${typ} = config
            |  return this
            |}""")
    } else {
      return (
        st"""// ============= ${typ} ===================
            |def alwaysTrue_$typ(v: $typ): B = {return T}
            |
            |var config_${typ}: Config_${typ} = Config_$typ(100, _verbose, alwaysTrue_$typ _)
            |def get_Config_${typ}: Config_${typ} = {return config_${typ}}
            |
            |def set_Config_${typ}(config: Config_${typ}): RandomLib ={
            |  config_${typ} = config
            |  return this
            |}""")

    }
  }

  var seenExtraNextMethods: Set[String] = Set.empty

  //get the initial call to the fields' next functions
  def genVar(v: Info.Var): ST = {
    val typName = SlangCheck.astTypeName(packageName, v.ast.tipeOpt.get)
    val typNameString = SlangCheck.astTypeNameString(packageName, v.ast.tipeOpt.get)

    val rs: ST = v.ast.tipeOpt match {
      case Some(t: AST.Type.Named) =>
        if (t.typeArgs.nonEmpty) {
          val typArgNames: ISZ[ST] = t.typeArgs.map(l => SlangCheck.astTypeName(packageName, l))
          val typArgNameStrings: ISZ[ST] = t.typeArgs.map(l => SlangCheck.astTypeNameString(packageName, l))

          val slangTypArgTypes: ISZ[String] = ISZ("Option", "IS", "MS", "ISZ", "MSZ", "Map")

          val nextName = st"next${typName}${(typArgNames, "")}".render
          if (!seenExtraNextMethods.contains(nextName)) {
            seenExtraNextMethods = seenExtraNextMethods + nextName
            if(ops.ISZOps(slangTypArgTypes).contains(typNameString.render)) {

              var initialize: ST = st""
              var resetVals: ST = st""

              if (typNameString.render == "Option") {
                initialize =
                  st"""var none: Z = gen.nextZBetween(0,1)
                    |var v: $typNameString[${typArgNameStrings(0)}] = if(none == 0) {
                    |  Some(next${typArgNames(0)}())
                    |} else {
                    |  None()
                    |}"""
                resetVals =
                  st"""none = gen.nextZBetween(0,1)
                    |v = if(none == 0) {
                    |   Some(next${typArgNames(0)}())
                    |} else {
                    |   None()
                    |}"""
              }
              else if (typNameString.render == "IS" || typNameString.render == "MS" || typNameString.render == "ISZ" || typNameString.render == "MSZ") {
                initialize =
                  st"""var length: Z = gen.nextZBetween(0, get_numElement)
                    |var v: $typNameString[${(typArgNameStrings, ", ")}] = $typNameString()
                    |for (r <- 0 until length) {
                    |  v = v :+ next${typArgNames(typArgNames.lastIndex)}()
                    |}"""
                resetVals =
                  st"""length = gen.nextZBetween(0, get_numElement)
                    |v = $typNameString()
                    |for (r <- 0 until length) {
                    |   v = v :+ next${typArgNames(typArgNames.lastIndex)}()
                    |}"""
              }
              else if (typNameString.render == "Map") {
                initialize =
                  st"""var length: Z = gen.nextZBetween(0, get_numElement)
                      |var v: $typNameString[${(typArgNameStrings, ", ")}] = Map.empty[${(typArgNameStrings, ", ")}]
                      |for (r <- 0 until length) {
                      |  v = v ++ ISZ((next${typArgNames(0)}(), next${typArgNames(1)}()))
                      |}"""
                resetVals =
                  st"""length = gen.nextZBetween(0, get_numElement)
                      |v = Map.empty[${(typArgNameStrings, ", ")}]
                      |for (r <- 0 until length) {
                      |  v = v ++ ISZ((next${typArgNames(0)}(), next${typArgNames(1)}()))
                      |}"""
              }

              nextMethods = nextMethods :+
                st"""//=================== $typNameString[${(typArgNameStrings, ", ")}] =====================
                    |def get_Config_${typName}${(typArgNames, "")}: Config_${typName}${(typArgNames, "")}
                    |def set_Config_${typName}${(typArgNames, "")}(config: Config_${typName}${(typArgNames, "")}): RandomLib
                    |
                    |def $nextName(): $typNameString[${(typArgNameStrings, ", ")}] = {
                    |
                    |  $initialize
                    |
                    |  if(get_Config_${typName}${(typArgNames, "")}.attempts >= 0) {
                    |   for(i <- 0 to get_Config_${typName}${(typArgNames, "")}.attempts) {
                    |      if(get_Config_${typName}${(typArgNames, "")}.filter(v)) {
                    |        return v
                    |      }
                    |      if (get_Config_${typName}${(typArgNames, "")}.verbose) {
                    |        println(s"Retrying for failing value: $$v")
                    |      }
                    |
                    |      $resetVals
                    |   }
                    |  } else {
                    |   while(T) {
                    |     if(get_Config_${typName}${(typArgNames, "")}.filter(v)) {
                    |       return v
                    |     }
                    |     if (get_Config_${typName}${(typArgNames, "")}.verbose) {
                    |       println(s"Retrying for failing value: $$v")
                    |     }
                    |
                    |     $resetVals
                    |   }
                    |  }
                    |
                    |  assert(F, "Requirements too strict to generate")
                    |  halt("Requirements too strict to generate")
                    |}"""

            } else {
              nextMethods = nextMethods :+
                st"""//=================== $typNameString[${(typArgNameStrings, ", ")}] =====================
                    |
                    |def $nextName(): $typNameString[${(typArgNameStrings, ", ")}] = {
                    |  assert(F, "$nextName needs to be implemented")
                    |  halt("$nextName needs to be implemented")
                    |}"""
            }

            nextConfig = nextConfig :+
              st"""// ============= $typNameString[${typArgNameStrings(0)}] ===================
                  |def alwaysTrue_${typName}${(typArgNames, "")}(v: $typNameString[${(typArgNameStrings, ", ")}]): B = {return T}
                  |
                  |var config_${typName}${(typArgNames, "")}: Config_${typName}${(typArgNames, "")} = Config_${typName}${(typArgNames, "")}(0, 20, 100, _verbose, alwaysTrue_${typName}${(typArgNames, "")} _)
                  |def get_Config_${typName}${(typArgNames, "")}: Config_${typName}${(typArgNames, "")} = {return config_${typName}${(typArgNames, "")}}
                  |
                  |def set_Config_${typName}${(typArgNames, "")}(config: Config_${typName}${(typArgNames, "")}): RandomLib ={
                  |  config_${typName}${(typArgNames, "")} = config
                  |  return this
                  |}"""
          }

          st"var ${v.ast.id.value}: $typNameString[${(typArgNameStrings, ", ")}] = next${typName}${(typArgNames, "")}()"
        }
        else {
          st"var ${v.ast.id.value}: $typNameString = next$typName()"
        }

      case _ => halt("Probably infeasible")
    }

    return rs

  }

  //get the second call to the args' next functions when args don't meeting the config
  def genVarRepeat(v: Info.Var): ST = {

    val typName = SlangCheck.astTypeName(packageName, v.ast.tipeOpt.get)

    val rs: ST = v.ast.tipeOpt match {
      case Some(t: AST.Type.Named) =>
        if (t.typeArgs.nonEmpty) {
          val typArgNames: ISZ[ST] = t.typeArgs.map(l => SlangCheck.astTypeName(packageName, l))
          st"${v.ast.id.value} = next${typName}${(typArgNames, "")}()"
        }
        else {
          st"${v.ast.id.value} = next$typName()"
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
          |var config_${adTypeName}: Config_${adTypeName} = Config_$adTypeName(100, _verbose, alwaysTrue_$adTypeName _)
          |
          |def get_Config_${adTypeName}: Config_${adTypeName} = {return config_${adTypeName}}
          |
          |def set_Config_${adTypeName}(config: Config_${adTypeName}): RandomLib ={
          |  config_${adTypeName} = config
          |  return this
          |}"""


    nextMethods = nextMethods :+
      st"""// ============= ${adTypeString} ===================
          |
          |def get_Config_${adTypeName}: Config_${adTypeName}
          |def set_Config_${adTypeName}(config: Config_${adTypeName}): RandomLib
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
          |     if (get_Config_${adTypeName}.verbose) {
          |       println(s"Retrying for failing value: $$v")
          |     }
          |     ordinal= gen.nextZBetween(0, ${genShortEnumType(ti.name)}.numOfElements-1)
          |     v = ${genShortEnumType(ti.name)}.byOrdinal(ordinal).get
          |   }
          |  } else {
          |   while(T){
          |     if(get_Config_${adTypeName}.filter(v)) {
          |      return v
          |     }
          |     if (get_Config_${adTypeName}.verbose) {
          |       println(s"Retrying for failing value: $$v")
          |     }
          |     ordinal= gen.nextZBetween(0, ${genShortEnumType(ti.name)}.numOfElements-1)
          |     v = ${genShortEnumType(ti.name)}.byOrdinal(ordinal).get
          |   }
          |  }
          |  assert(F, "Requirements too strict to generate")
          |  halt("Requirements too strict to generate")
          |}"""
  }

  def genSig(ti: TypeInfo.Sig): Unit = {

    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)
    val leaves: ISZ[AST.Typed.Name] = SlangCheck.sortedTyedNames(th.substLeavesOfType(ti.posOpt, ti.tpe).left)

    var calls: ISZ[ST] = ISZ()
    var enumNames: ISZ[ST] = ISZ()
    var cases: ISZ[ST] = ISZ()

    for (typ <- SlangCheck.sortedTyedNames(leaves)) {
      val typName = SlangCheck.astTypedName(packageName, typ)
      calls = calls :+ st"next$typName"

      if (typ.ids(0) == packageName(0)) {
        enumNames = enumNames :+ st"${adTypeName}_DataTypeId.${(ops.ISZOps(typ.ids).drop(1), "")}_Id"
      } else {
        enumNames = enumNames :+ st"${adTypeName}_DataTypeId._${(typ.ids, "")}_Id"
      }
    }

    for (i <- 0 to enumNames.size - 1) {
      cases = cases :+ st"case ${enumNames(i)} => (${calls(i)} _).apply()"
    }

    nextConfig = nextConfig :+
      st"""// ============= ${adTypeString} ===================
          |def alwaysTrue_$adTypeName(v: $adTypeString): B = {return T}
          |
          |var config_${adTypeName}: Config_${adTypeName} = Config_$adTypeName(100, _verbose, F, ISZ(), alwaysTrue_$adTypeName _)
          |
          |def get_Config_${adTypeName}: Config_${adTypeName} = {return config_${adTypeName}}
          |
          |def set_Config_${adTypeName}(config: Config_${adTypeName}): RandomLib ={
          |  config_${adTypeName} = config
          |  return this
          |}"""

    nextMethods = nextMethods :+
      st"""// ============= ${adTypeString} ===================
          |
          |def get_Config_${adTypeName}: Config_${adTypeName}
          |def set_Config_${adTypeName}(config: Config_${adTypeName}): RandomLib
          |
          |def next${adTypeName}(): ${adTypeString} = {
          |  var callEnum: ISZ[${adTypeName}_DataTypeId.Type] = ISZ(${(enumNames, ", ")})
          |
          |  if(get_Config_${adTypeName}.additiveTypeFiltering) {
          |     callEnum = get_Config_${adTypeName}.typeFilter
          |  } else {
          |     for(h <- get_Config_${adTypeName}.typeFilter) {
          |       callEnum = ops.ISZOps(callEnum).filter(f => h =!= f)
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
          |     if (get_Config_${adTypeName}.verbose) {
          |       println(s"Retrying for failing value: $$v")
          |     }
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
          |     if (get_Config_${adTypeName}.verbose) {
          |       println(s"Retrying for failing value: $$v")
          |     }
          |     c = callEnum(gen.nextZBetween(0, callEnum.size-1))
          |
          |     v = c match {
          |       ${(cases, "\n")}
          |       case _ => halt("Invalid Child")
          |     }
          |   }
          |  }
          |  assert(F, "Requirements too strict to generate")
          |  halt("Requirements too strict to generate")
          |}"""
  }

  //get next methods and base configs for a non-enum type
  def genAdt(ti: TypeInfo.Adt): Unit = {

    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    if (ti.ast.typeParams.size == 0) {
      if (ti.ast.isRoot) {
        val leaves: ISZ[AST.Typed.Name] = SlangCheck.sortedTyedNames(th.substLeavesOfType(ti.posOpt, ti.tpe).left)

        var calls: ISZ[ST] = ISZ()
        var enumNames: ISZ[ST] = ISZ()
        var cases: ISZ[ST] = ISZ()

        for (typ <- SlangCheck.sortedTyedNames(leaves)) {
          val typName = SlangCheck.astTypedName(packageName, typ)
          calls = calls :+ st"next$typName"

          if (typ.ids(0) == packageName(0)) {
            enumNames = enumNames :+ st"${adTypeName}_DataTypeId.${(ops.ISZOps(typ.ids).drop(1), "")}_Id"
          } else {
            enumNames = enumNames :+ st"${adTypeName}_DataTypeId._${(typ.ids, "")}_Id"
          }
        }

        for (i <- 0 to enumNames.size - 1) {
          cases = cases :+ st"case ${enumNames(i)} => (${calls(i)} _).apply()"
        }

        nextConfig = nextConfig :+
          st"""// ============= ${adTypeString} ===================
              |def alwaysTrue_$adTypeName(v: $adTypeString): B = {return T}
              |
              |var config_${adTypeName}: Config_${adTypeName} = Config_$adTypeName(100, _verbose, F, ISZ(), alwaysTrue_$adTypeName _)
              |
              |def get_Config_${adTypeName}: Config_${adTypeName} = {return config_${adTypeName}}
              |
              |def set_Config_${adTypeName}(config: Config_${adTypeName}): RandomLib ={
              |  config_${adTypeName} = config
              |  return this
              |}"""

        nextMethods = nextMethods :+
          st"""// ============= ${adTypeString} ===================
              |
              |def get_Config_${adTypeName}: Config_${adTypeName}
              |def set_Config_${adTypeName}(config: Config_${adTypeName}): RandomLib
              |
              |def next${adTypeName}(): ${adTypeString} = {
              |  var callEnum: ISZ[${adTypeName}_DataTypeId.Type] = ISZ(${(enumNames, ", ")})
              |
              |  if(get_Config_${adTypeName}.additiveTypeFiltering) {
              |     callEnum = get_Config_${adTypeName}.typeFilter
              |  } else {
              |     for(h <- get_Config_${adTypeName}.typeFilter) {
              |       callEnum = ops.ISZOps(callEnum).filter(f => h =!= f)
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
              |     if (get_Config_${adTypeName}.verbose) {
              |       println(s"Retrying for failing value: $$v")
              |     }
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
              |     if (get_Config_${adTypeName}.verbose) {
              |       println(s"Retrying for failing value: $$v")
              |     }
              |     c = callEnum(gen.nextZBetween(0, callEnum.size-1))
              |
              |     v = c match {
              |       ${(cases, "\n")}
              |       case _ => halt("Invalid Child")
              |     }
              |   }
              |  }
              |  assert(F, "Requirements too strict to generate")
              |  halt("Requirements too strict to generate")
              |}"""
      }
      else {

        val vars: ISZ[ST] = for (v <- ti.vars.values) yield genVar(v)
        val args: ISZ[ST] = for (v <- ti.vars.values) yield genArgs(v)
        val varsRepeat: ISZ[ST] = for (v <- ti.vars.values) yield genVarRepeat(v)

        val filter: ST = {
          var resolvedMethods: ISZ[AST.Stmt.Method] = ISZ()

          th.nameMap.get(ti.name) match {
            case Some(companionObj: Info.Object) =>
              val gumboXMethodName = s"D_Inv_${ops.ISZOps(ti.name).last}"
              for (stmt <- companionObj.ast.stmts) {
                stmt match {
                  case m: AST.Stmt.Method if m.sig.id.value == gumboXMethodName =>
                    m.sig.params match {
                      case ISZ(AST.Param(_, _, typ: AST.Type.Named)) =>
                        typ.attr.typedOpt match {
                          case Some(typed: AST.Typed.Name) if typed.ids == ti.name =>
                            resolvedMethods = resolvedMethods :+ m
                          case _ =>
                        }
                      case _ =>
                    }
                  case _ =>
                }
              }
            case _ =>
          }

          if (resolvedMethods.size == 1) {
            val receiver = st"${(ti.name, ".")}"
            val method = st"${resolvedMethods(0).sig.id.value}"
            st"$receiver.$method _"
          } else {
            if (resolvedMethods.nonEmpty) {
              reporter.warn(None(), SlangCheck.toolName, s"Found ${resolvedMethods.size} companion object methods that are SlangCheck filter compatible. Currently only supporting 1 so default filter will be used instead")
            }
            st"alwaysTrue_$adTypeName _"
          }
        }

        nextConfig = nextConfig :+
          st"""// ============= ${adTypeString} ===================
              |def alwaysTrue_$adTypeName(v: $adTypeString): B = {return T}
              |
              |var config_${adTypeName}: Config_${adTypeName} = Config_$adTypeName(100, _verbose, $filter)
              |
              |def get_Config_${adTypeName}: Config_${adTypeName} = {return config_${adTypeName}}
              |
              |def set_Config_${adTypeName}(config: Config_${adTypeName}): RandomLib ={
              |  config_${adTypeName} = config
              |  return this
              |}"""

        nextMethods = nextMethods :+
          st"""// ============= ${adTypeString} ===================
              |
              |def get_Config_${adTypeName}: Config_${adTypeName}
              |def set_Config_${adTypeName}(config: Config_${adTypeName}): RandomLib
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
              |      if (get_Config_${adTypeName}.verbose) {
              |        println(s"Retrying for failing value: $$v")
              |      }
              |      ${(varsRepeat, "\n")}
              |      v = ${adTypeString}(${(args, ", ")})
              |   }
              |  } else {
              |   while(T) {
              |     if(get_Config_${adTypeName}.filter(v)) {
              |       return v
              |     }
              |     if (get_Config_${adTypeName}.verbose) {
              |       println(s"Retrying for failing value: $$v")
              |     }
              |     ${(varsRepeat, "\n")}
              |     v = ${adTypeString}(${(args, ", ")})
              |   }
              |  }
              |
              |  assert(F, "Requirements too strict to generate")
              |  halt("Requirements too strict to generate")
              |}"""
      }
    }
  }
}

@record class ConfigGen(val packageName: QName,
                        val fileNames: ISZ[String],
                        val th: TypeHierarchy) {

  val reporter: Reporter = Reporter.create

  var slangTypes: ISZ[String] = ISZ("Z", "B", "C", "R", "F32", "F64", "S8", "S16", "S32", "S64", "U8", "U16", "U32", "U64")
  var slangTypeConf: ISZ[ST] = for (p <- slangTypes) yield genSlangBaseTypes(p)

  var nextConfig: ISZ[ST] = ISZ()

  def gen(sortedTypeInfos: ISZ[TypeInfo]): (ISZ[String], ST) = {

    for (ti <- sortedTypeInfos) {
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

    return (ISZ("SlangCheckConfig.scala"),
      st"""// #Sireum
          |
          |package ${(packageName, ".")}
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
          |@datatype class Config_String(minSize: Z, maxSize: Z, attempts: Z, verbose: B, filter: String => B) {}
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
      st"""@datatype class Config_${adTypeName}(attempts: Z, verbose: B, filter: ${adTypeString} => B) {}"""
  }

  //get config type for a slang type
  def genSlangBaseTypes(typ: String): ST = {
    return if (th.isSubZName(ISZ("org", "sireum", typ)) || typ == "F32" || typ == "F64" || typ == "R" || typ == "Z" || typ =="C")
      st"""@datatype class Config_${typ}(low: Option[$typ], high: Option[$typ], attempts: Z, verbose: B, filter: ${typ} => B) {}"""
    else
      st"""@datatype class Config_${typ}(attempts: Z, verbose: B, filter: ${typ} => B) {}"""

  }

  var seenExtraConfigs: Set[String] = Set.empty

  //get config type for everything else
  def genAdt(ti: TypeInfo.Adt): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    for (v <- ti.vars.values) {
      val typName = SlangCheck.astTypeName(packageName, v.ast.tipeOpt.get)
      val typNameString = SlangCheck.astTypeNameString(packageName, v.ast.tipeOpt.get)

      v.ast.tipeOpt match {
        case Some(t: AST.Type.Named) =>
          if(t.typeArgs.nonEmpty) {
            val typArgNames: ISZ[ST] = t.typeArgs.map(l => SlangCheck.astTypeName(packageName, l))
            val typArgNameStrings: ISZ[ST] = t.typeArgs.map(l => SlangCheck.astTypeNameString(packageName, l))

            val conf: ST = st"@datatype class Config_${typName}${(typArgNames, "")}(minSize: Z, maxSize: Z, attempts: Z, verbose: B, filter: $typNameString[${(typArgNameStrings, ", ")}] => B) {}"

            if (!seenExtraConfigs.contains(conf.render)) {
              seenExtraConfigs = seenExtraConfigs + conf.render
              nextConfig = nextConfig :+ conf
            }

          }
        case _ => halt("Probably infeasible")
      }
    }

    if (ti.ast.isRoot) {
      nextConfig = nextConfig :+
        st"""@datatype class Config_${adTypeName}(attempts: Z, verbose: B, additiveTypeFiltering: B, typeFilter: ISZ[${adTypeName}_DataTypeId.Type], filter: ${adTypeString} => B) {}"""
    }
    else {
      nextConfig = nextConfig :+
        st"""@datatype class Config_${adTypeName}(attempts: Z, verbose: B, filter: ${adTypeString} => B) {}"""
    }
  }

  def genSig(ti: TypeInfo.Sig): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    nextConfig = nextConfig :+
      st"""@datatype class Config_${adTypeName}(attempts: Z, verbose: B, additiveTypeFiltering: B, typeFilter: ISZ[${adTypeName}_DataTypeId.Type], filter: ${adTypeString} => B) {}"""
  }
}

@record class GeneratorGen(val packageName: QName,
                           val fileNames: ISZ[String], val th: TypeHierarchy) {

  val reporter: Reporter = Reporter.create

  var slangTypes: ISZ[String] = ISZ("String", "Z", "B", "C", "R", "F32", "F64", "S8", "S16", "S32", "S64", "U8", "U16", "U32", "U64")
  var slangTypeGen: ISZ[ST] = for (p <- slangTypes) yield genSlangType(p)

  var nextClass: ISZ[ST] = ISZ()

  def gen(sortedTypeInfos: ISZ[TypeInfo]): (ISZ[String], ST) = {

    for (ti <- sortedTypeInfos) {
      ti match {
        case ti: TypeInfo.Adt =>
          genAdt(ti)
        case ti: TypeInfo.Enum =>
          genEnum(ti)
        case ti: TypeInfo.Sig =>
          genSig(ti)
        case _ =>
      }
    }
    return (ISZ("SlangCheckGenerator.scala"),
      st"""// #Sireum
          |
          |package ${(packageName, ".")}
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

  var seenExtraGenerators: Set[String] = Set.empty

  //get a generator for a everything else
  def genAdt(ti: TypeInfo.Adt): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    for (v <- ti.vars.values) {
      val typName = SlangCheck.astTypeName(packageName, v.ast.tipeOpt.get)
      val typNameString = SlangCheck.astTypeNameString(packageName, v.ast.tipeOpt.get)

      v.ast.tipeOpt match {
        case Some(t: AST.Type.Named) =>
          if (t.typeArgs.nonEmpty) {
            val typArgNames: ISZ[ST] = t.typeArgs.map(l => SlangCheck.astTypeName(packageName, l))
            val typArgNameStrings: ISZ[ST] = t.typeArgs.map(l => SlangCheck.astTypeNameString(packageName, l))

            val genName: String = st"Gen_${typName}${(typArgNames, "")}".render

            if (!seenExtraGenerators.contains(genName)) {
              seenExtraGenerators = seenExtraGenerators + genName
              nextClass = nextClass :+
                st"""@record class $genName(param: RandomLibI) extends MJen[${typNameString}[${(typArgNameStrings, ", ")}]] {
                    |  override def generate(f: ${typNameString}[${(typArgNameStrings, ", ")}] => Jen.Action): Jen.Action = {
                    |    var continue = Jen.Continue
                    |    while (T) {
                    |
                    |      continue = f(param.next${typName}${(typArgNames, "")}())
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
        case _ => halt("Probably infeasible")
      }
    }

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

@record class TestGen(val packageName: QName,
                      val fileNames: ISZ[String],
                      val th: TypeHierarchy) {

  val reporter: Reporter = Reporter.create

  var slangTypes: ISZ[String] = ISZ("String", "Z", "B", "C", "R", "F32", "F64", "S8", "S16", "S32", "S64", "U8", "U16", "U32", "U64")
  var slangTypeGen: ISZ[ST] = for (p <- slangTypes) yield genSlangType(p)

  var nextClass: ISZ[ST] = ISZ()

  def gen(): (ISZ[String], ST) = {
    var cleanedTypeMapValues: Set[TypeInfo] = Set.empty

    for (v <- th.typeMap.values) {
      val temp = v
      v.posOpt match {
        case Some(pos) if ops.ISZOps(fileNames).contains(pos.uriOpt.get) =>
          cleanedTypeMapValues = cleanedTypeMapValues + temp
        case _ =>
      }
    }

    for (ti <- SlangCheck.sortedTypes(cleanedTypeMapValues.elements)) {
      ti match {
        case ti: TypeInfo.Adt =>
          genAdt(ti)
        case ti: TypeInfo.Enum =>
          genEnum(ti)
        case ti: TypeInfo.Sig =>
          genSig(ti)
        case _ =>
      }
    }
    return (ISZ("SlangCheckTest.scala"),
      st"""package ${(packageName, ".")}
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
          |  val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
          |  val gen = Gen_$adTypeName(randomLib)
          |
          |  for(r <- gen.take(100))
          |    println(r)
          |}"""
  }

  //get a generator for a slang type
  def genSlangType(typ: String): ST = {
    return (
      st"""test("$typ Output") {
          |  val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
          |  val gen = Gen_$typ(randomLib)
          |
          |  for(r <- gen.take(100))
          |    println(r)
          |}""")
  }

  //get a generator for a everything else

  var seenExtraTest: Set[String] = Set.empty

  def genAdt(ti: TypeInfo.Adt): Unit = {
    val adTypeString = Resolver.typeNameString(packageName, ti.name)
    val adTypeName = Resolver.typeName(packageName, ti.name)

    for (v <- ti.vars.values) {
      val typName = SlangCheck.astTypeName(packageName, v.ast.tipeOpt.get)
      val typNameString = SlangCheck.astTypeNameString(packageName, v.ast.tipeOpt.get)

      v.ast.tipeOpt match {
        case Some(t: AST.Type.Named) =>
          if (t.typeArgs.nonEmpty) {
            val typArgNames: ISZ[ST] = t.typeArgs.map(l => SlangCheck.astTypeName(packageName, l))
            val typArgNameStrings: ISZ[ST] = t.typeArgs.map(l => SlangCheck.astTypeNameString(packageName, l))

            val genName: String = st"Gen_${typName}${(typArgNames, "")}".render

            if (!seenExtraTest.contains(genName)) {
              seenExtraTest = seenExtraTest + genName
              nextClass = nextClass :+
                st"""test("$typNameString[${(typArgNameStrings, ", ")}] Output") {
                    |  val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
                    |  val gen = Gen_${typName}${(typArgNames, "")}(randomLib)
                    |
                    |  for(r <- gen.take(100))
                    |    println(r)
                    |}"""
            }

          }
        case _ => halt("Probably infeasible")
      }
    }


    nextClass = nextClass :+
      st"""test("$adTypeString Output") {
          |  val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
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
          |  val randomLib: RandomLib = new RandomLib(new Random.Gen64Impl(Xoshiro256.create)).verbose
          |  val gen = Gen_$adTypeName(randomLib)
          |
          |  for(r <- gen.take(100))
          |    println(r)
          |}"""
  }

}
