// #Sireum
/*
 Copyright (c) 2019, Robby, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.sireum.tools

import org.sireum._
import org.sireum.bitcodec.Spec
import org.sireum.lang.{ast => AST}
import org.sireum.message.Reporter

object BitCodecGen {
  val kind: String = "bcgen"
  val beginCodeMarker: String = "// BEGIN USER CODE:"
  val endCodeMarker: String = "// END USER CODE:"
  val specName: ISZ[String] = ISZ("org", "sireum", "bitcodec", "Spec")
  val unionName: ISZ[String] = specName :+ "Union"
  val repeatName: ISZ[String] = specName :+ "Repeat"
  val rawName: ISZ[String] = specName :+ "Raw"

  val funOwners: HashSet[ISZ[String]] = HashSet.empty[ISZ[String]] ++ ISZ(unionName, repeatName, rawName)
  val funName: HashMap[ISZ[String], String] = HashMap.empty[ISZ[String], String] ++ ISZ(
    unionName ~> "choice",
    repeatName ~> "size",
    rawName ~> "size"
  )

  def gen(isProgram: B,
          isBigEndian: B,
          licenseOpt: Option[String],
          filename: String,
          packageNames: ISZ[String],
          name: String,
          text: String,
          spec: Spec,
          program: AST.TopUnit.Program,
          prevGen: String,
          reporter: Reporter): ST = {

    val normText = ops.StringOps(text).replaceAllLiterally("\r\n", "\n")

    val collector: EnumFunCollector = EnumFunCollector(Reporter.create)
    collector.transformTopUnit(program)
    reporter.reports(collector.reporter.messages)

    val codeSectionMap = ops.StringOps(ops.StringOps(ops.StringOps(prevGen).
      replaceAllLiterally("\r\n", "\n")).replaceAllLiterally("\n", " \n")).
      collectCodeSections(kind, beginCodeMarker, endCodeMarker, reporter)

    if (!spec.isComposite) {
      reporter.error(None(), kind,
        "Top-level Spec has to be a composite (i.e., Union, Repeat, Raw, GenUnion, GenRepeat, or GenRaw)")
    }

    if (reporter.hasIssue) {
      return st"$prevGen"
    }

    val bcGen = BitCodecGen(isProgram, isBigEndian, licenseOpt, filename, packageNames, name, normText, program,
      ops.StringOps(prevGen).replaceAllLiterally("/r/n", "/n"), collector.enums, collector.funs, codeSectionMap)
    return bcGen.gen(spec, reporter)
  }

  @record class EnumFunCollector(reporter: Reporter) extends AST.MTransformer {
    var enums: HashMap[String, AST.Stmt.Enum] = HashMap.empty
    var funs: HashMap[String, (AST.Exp.Fun, AST.Type)] = HashMap.empty

    override def preExpInvoke(o: AST.Exp.Invoke): AST.MTransformer.PreResult[AST.Exp] = {
      o.attr.resOpt match {
        case Some(res) =>
          res match {
            case m: AST.ResolvedInfo.Method if m.mode == AST.MethodMode.Constructor =>
              val className = m.owner :+ m.id
              if (funOwners.contains(className)) {
                val ownerSimpleName = m.id
                val targ = o.targs(0)
                (o.args(0), o.args(2)) match {
                  case (name: AST.Exp.LitString, fun: AST.Exp.Fun) if fun.params.size == 1 =>
                    funs = funs + name.value ~> ((fun, targ))
                  case (_: AST.Exp.LitString, fun) =>
                    reporter.error(fun.posOpt, kind,
                      s"Invalid ${funName.get(className).get} form for $ownerSimpleName; it has to be a function with a single parameter")
                  case (name, _) =>
                    reporter.error(name.posOpt, kind,
                      s"Invalid name form for $ownerSimpleName; it has to be a String literal")
                }
              }
            case _ =>
          }
        case _ =>
      }
      return super.preExpInvoke(o)
    }

    override def preStmtEnum(o: AST.Stmt.Enum): AST.MTransformer.PreResult[AST.Stmt] = {
      enums = enums + o.id.value ~> o
      return super.preStmtEnum(o)
    }
  }

  object Context {
    def create: Context = {
      return Context(ISZ(), 2, ISZ.create(65, F), ISZ(), ISZ(),
        "", "Runtime.Composite", ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), None())
    }
  }

  @datatype class Context(path: ISZ[String],
                          errNum: Z,
                          imports: ISZ[B],
                          mainDecl: ISZ[ST],
                          main: ISZ[ST],
                          owner: String,
                          supr: String,
                          fields: ISZ[ST],
                          inits: ISZ[ST],
                          wellFormed: ISZ[ST],
                          decoding: ISZ[ST],
                          encoding: ISZ[ST],
                          members: ISZ[ST],
                          accessOpt: Option[ST])

}

import BitCodecGen._

@datatype class BitCodecGen(isProgram: B,
                            isBigEndian: B,
                            licenseOpt: Option[String],
                            filename: String,
                            packageNames: ISZ[String],
                            name: String,
                            text: String,
                            program: AST.TopUnit.Program,
                            prevGen: String,
                            enums: HashMap[String, AST.Stmt.Enum],
                            funs: HashMap[String, (AST.Exp.Fun, AST.Type)],
                            codeSectionMap: HashSMap[String, String]) {

  val endianPrefix: String = if (isBigEndian) "be" else "le"

  def gen(spec: Spec, reporter: Reporter): ST = {
    val context = genSpec(Context.create, spec, reporter)
    if (reporter.hasIssue) {
      return st"$prevGen"
    }
    val packageOpt: Option[ST] =
      if (isProgram) if (packageNames.nonEmpty) Some(st"package ${(packageNames, ".")}") else None()
      else None()
    val testOpt: Option[ST] =
      if (isProgram) None()
      else Some(
        st"""
            |$beginCodeMarker Test
            |${codeSectionMap.get("Test").getOrElse("")}
            |$endCodeMarker Test""")

    val r =
      st"""// #Sireum
          |$licenseOpt
          |$packageOpt
          |
          |import org.sireum._
          |${(for (i <- 1 until 64 if context.imports(i)) yield st"import org.sireum.U$i._", "\n")}
          |import org.sireum.ops.Bits.{Context, Reader, Writer}
          |import org.sireum.bitcodec.Runtime
          |
          |object ${if (isProgram) name else "BitCodec"} {
          |
          |  ${(context.mainDecl, "\n\n")}
          |
          |  ${(context.main, "\n\n")}
          |
          |}
          |$testOpt"""
    return r
  }

  def genSpec(context: Context, o: Spec, reporter: Reporter): Context = {
    o match {
      case o: Spec.Boolean => return genSpecBoolean(context, o, reporter)
      case o: Spec.Bits => return genSpecBits(context, o, reporter)
      case o: Spec.Bytes => return genSpecBytes(context, o, reporter)
      case o: Spec.Shorts => return genSpecShorts(context, o, reporter)
      case o: Spec.Ints => return genSpecInts(context, o, reporter)
      case o: Spec.Longs => return genSpecLongs(context, o, reporter)
      case o: Spec.Enum => return genSpecEnum(context, o, reporter)
      case o: Spec.Concat => return genSpecConcat(context, o, reporter)
      case o: Spec.GenUnion => return genSpecGenUnion(context, o, reporter)
      case o: Spec.GenRepeat => return genSpecGenRepeat(context, o, reporter)
      case o: Spec.GenRaw => return genSpecGenRaw(context, o, reporter)
      case o: Spec.Pads => return genSpecPads(context, o, reporter)
      case _ =>
        val p = o.polyDescOpt.get
        p.compName match {
          case string"Union" => return genSpecUnion(context, p.name, p.dependsOn, p.elementsOpt.get, reporter)
          case string"Repeat" => return genSpecRepeat(context, p.name, p.dependsOn, p.elementsOpt.get(0), reporter)
          case string"Raw" => return genSpecRaw(context, p.name, p.dependsOn, reporter)
        }
    }
  }

  def genSpecBoolean(context: BitCodecGen.Context, spec: Spec.Boolean, reporter: Reporter): Context = {
    val name = spec.name
    val access = context.accessOpt.getOrElse(st"$name")
    return context(
      fields = context.fields :+ st"var $name: B",
      inits = context.inits :+ st"F",
      decoding = context.decoding :+ st"$access = Reader.MS.bleB(input, context)",
      encoding = context.encoding :+ st"Writer.bleB(output, context, $access)")
  }

  def genSpecBits(context: BitCodecGen.Context, spec: Spec.Bits, reporter: Reporter): Context = {
    val name = spec.name
    val size = spec.size
    val prefix: String = if (size == 1) "ble" else endianPrefix
    val access = context.accessOpt.getOrElse(st"$name")
    if (size < 1) {
      reporter.error(None(), kind, st"Size must be >= 1 for Spec.Bits ${(context.path :+ name, ".")}".render)
      return context
    } else if (size <= 64) {
      return context(
        imports = context.imports(size ~> T),
        fields = context.fields :+ st"var $name: U$size",
        inits = context.inits :+ st"""u$size"0"""",
        decoding = context.decoding :+ st"$access = Reader.MS.${prefix}U$size(input, context)",
        encoding = context.encoding :+ st"Writer.${prefix}U$size(output, context, $access)")
    } else {
      return context(
        fields = context.fields :+ st"var $name: MSZ[B]",
        inits = context.inits :+ st"MSZ.create($size, F)",
        wellFormed = context.wellFormed :+
          st"""if ($access.size != $size) {
              |  return ERROR_${context.owner}
              |}""",
        decoding = context.decoding :+ st"Reader.MS.${prefix}BS(input, context, $access, $size)",
        encoding = context.encoding :+ st"Writer.${prefix}BS(output, context, $access)")
    }
  }

  def genSpecBytes(context: BitCodecGen.Context, spec: Spec.Bytes, reporter: Reporter): Context = {
    val name = spec.name
    val size = spec.size
    val access = context.accessOpt.getOrElse(st"$name")
    return context(
      imports = context.imports(8 ~> T),
      fields = context.fields :+ st"var $name: MSZ[U8]",
      inits = context.inits :+ st"""MSZ.create($size, u8"0")""",
      wellFormed = context.wellFormed :+
        st"""if ($access.size != $size) {
            |  return ERROR_${context.owner}
            |}""",
      decoding = context.decoding :+ st"Reader.MS.${endianPrefix}U8S(input, context, $access, $size)",
      encoding = context.encoding :+ st"Writer.${endianPrefix}U8S(output, context, $access)")
  }

  def genSpecShorts(context: BitCodecGen.Context, spec: Spec.Shorts, reporter: Reporter): Context = {
    val name = spec.name
    val size = spec.size
    val access = context.accessOpt.getOrElse(st"$name")
    return context(
      imports = context.imports(16 ~> T),
      fields = context.fields :+ st"var $name: MSZ[U16]",
      inits = context.inits :+ st"""MSZ.create($size, u16"0")""",
      wellFormed = context.wellFormed :+
        st"""if ($access.size != $size) {
            |  return ERROR_${context.owner}
            |}""",
      decoding = context.decoding :+ st"Reader.MS.${endianPrefix}U16S(input, context, $access, $size)",
      encoding = context.encoding :+ st"Writer.${endianPrefix}U16S(output, context, $access)")
  }

  def genSpecInts(context: BitCodecGen.Context, spec: Spec.Ints, reporter: Reporter): Context = {
    val name = spec.name
    val size = spec.size
    val access = context.accessOpt.getOrElse(st"$name")
    return context(
      imports = context.imports(32 ~> T),
      fields = context.fields :+ st"var $name: MSZ[U32]",
      inits = context.inits :+ st"""MSZ.create($size, u32"0")""",
      wellFormed = context.wellFormed :+
        st"""if ($access.size != $size) {
            |  return ERROR_${context.owner}
            |}""",
      decoding = context.decoding :+ st"Reader.MS.${endianPrefix}U32S(input, context, $access, $size)",
      encoding = context.encoding :+ st"Writer.${endianPrefix}U32S(output, context, $access)")
  }

  def genSpecLongs(context: BitCodecGen.Context, spec: Spec.Longs, reporter: Reporter): Context = {
    val name = spec.name
    val size = spec.size
    val access = context.accessOpt.getOrElse(st"$name")
    return context(
      imports = context.imports(64 ~> T),
      fields = context.fields :+ st"var $name: MSZ[U64]",
      inits = context.inits :+ st"""MSZ.create($size, u64"0")""",
      wellFormed = context.wellFormed :+
        st"""if ($access.size != $size) {
            |  return ERROR_${context.owner}
            |}""",
      decoding = context.decoding :+ st"Reader.MS.${endianPrefix}U64S(input, context, $access, $size)",
      encoding = context.encoding :+ st"Writer.${endianPrefix}U64S(output, context, $access)")
  }

  def genSpecEnum(context: BitCodecGen.Context, spec: Spec.Enum, reporter: Reporter): Context = {
    val name = spec.name
    val objectName = spec.objectName
    val enum: AST.Stmt.Enum = enums.get(objectName) match {
      case Some(e) => e
      case _ =>
        reporter.error(None(), kind,
          st"Could not find enum $name for ${(context.path :+ name, ".")}".render)
        return context
    }
    val size = bitWidth(enum.elements.size)
    val first = enum.elements(0).value
    val access = context.accessOpt.getOrElse(st"$name")
    return context(
      imports = context.imports(size ~> T),
      errNum = context.errNum + 1,
      mainDecl = context.mainDecl :+ st"val ERROR_$objectName: Z = ${context.errNum}",
      main = context.main :+
        st"""@enum object $objectName {
            |  ${(for (element <- enum.elements) yield st"'${element.value}", "\n")}
            |}
            |
            |def decode$objectName(input: MSZ[B], context: Context): $objectName.Type = {
            |  if (context.offset + $size > input.size) {
            |    context.signalError(ERROR_$objectName)
            |  }
            |  if (context.hasError) {
            |    return $objectName.$first
            |  }
            |  val r: $objectName.Type = Reader.MS.beU$size(input, context) match {
            |    ${(for (i <- 0 until enum.elements.size) yield st"""case u$size"$i" => $objectName.${enum.elements(i).value}""", "\n")}
            |    case _ =>
            |      context.signalError(ERROR_$objectName)
            |      $objectName.$first
            |  }
            |  return r
            |}
            |
            |def encode$objectName(output: MSZ[B], context: Context, $name: $objectName.Type): Unit = {
            |  if (context.offset + $size > output.size) {
            |    context.signalError(ERROR_$objectName)
            |  }
            |  if (context.hasError) {
            |    return
            |  }
            |  $name match {
            |    ${(for (i <- 0 until enum.elements.size) yield st"""case $objectName.${enum.elements(i).value} => Writer.beU$size(output, context, u$size"$i")""", "\n")}
            |  }
            |}""",
      fields = context.fields :+ st"var $name: $objectName.Type",
      inits = context.inits :+ st"""$objectName.$first""",
      decoding = context.decoding :+ st"$access = decode$objectName(input, context)",
      encoding = context.encoding :+ st"encode$objectName(output, context, $access)")
  }

  def genSpecConcat(context: BitCodecGen.Context, spec: Spec.Concat, reporter: Reporter): Context = {
    val name = spec.name
    val fname = ops.StringOps(name).firstToLower
    var elementContext = context(path = context.path :+ fname, owner = name, supr = "Runtime.Composite",
      fields = ISZ(), inits = ISZ(), wellFormed = ISZ(), decoding = ISZ(), encoding = ISZ(), members = ISZ())
    for (element <- spec.elements) {
      elementContext = genSpec(elementContext, element, reporter)
    }
    val access = context.accessOpt.getOrElse(st"$fname")
    return Context(
      path = context.path,
      errNum = elementContext.errNum + 1,
      imports = elementContext.imports,
      mainDecl = elementContext.mainDecl :+ st"val ERROR_$name: Z = ${elementContext.errNum}",
      main = elementContext.main :+
        st"""object $name {
            |  def empty: $name = {
            |    return $name(${(elementContext.inits, ", ")})
            |  }
            |}
            |
            |@record class $name(
            |  ${(elementContext.fields, ",\n")}
            |) extends ${context.supr} {
            |
            |  def wellFormed: Z = {
            |
            |    ${(elementContext.wellFormed, "\n\n")}
            |
            |    $beginCodeMarker $name.wellFormed
            |    ${(prevs(s"$name.wellFormed"), "\n")}
            |    $endCodeMarker $name.wellFormed
            |
            |    return 0
            |  }
            |
            |  def decode(input: MSZ[B], context: Context): Unit = {
            |    ${(elementContext.decoding, "\n")}
            |
            |    val wf = wellFormed
            |    if (wf != 0) {
            |      context.signalError(wf)
            |    }
            |  }
            |
            |  def encode(output: MSZ[B], context: Context): Unit = {
            |    ${(elementContext.encoding, "\n")}
            |
            |    if (context.errorCode == Writer.INSUFFICIENT_BUFFER_SIZE) {
            |      context.updateErrorCode(ERROR_$name)
            |    }
            |  }
            |
            |  ${(elementContext.members, "\n\n")}
            |}""",
      owner = context.owner,
      supr = context.supr,
      fields = context.fields :+ st"var $fname: $name",
      inits = context.inits :+ st"$name.empty",
      wellFormed = context.wellFormed :+
        st"""val wf$name = $access.wellFormed
            |if (wf$name != 0) {
            |  return wf$name
            |}""",
      decoding = context.decoding :+ st"$access.decode(input, context)",
      encoding = context.encoding :+ st"$access.encode(output, context)",
      members = context.members,
      accessOpt = context.accessOpt)
  }

  def genSpecUnion(context: BitCodecGen.Context,
                   name: String,
                   dependsOn: ISZ[String],
                   subs: ISZ[Spec],
                   reporter: Reporter): Context = {
    val normSubs: ISZ[Spec] =
      for (sub <- subs) yield if (sub.isComposite) sub else Spec.Concat(ops.StringOps(sub.name).firstToUpper, ISZ(sub))
    val (pName, pType, pBody): (String, String, String) =
      funNameTypeBody(context.path :+ name, dependsOn.size, name, "Spec.Union.choice", reporter) match {
        case Some((pn, ptpe, pt)) => (pn, ptpe, pt)
        case _ => return context
      }
    val fname = ops.StringOps(name).firstToLower
    var subContext = context(path = context.path :+ fname, main = ISZ(), owner = name, supr = name,
      fields = ISZ(), inits = ISZ(), wellFormed = ISZ(), decoding = ISZ(), encoding = ISZ(), members = ISZ())
    for (sub <- normSubs) {
      subContext = genSpec(subContext, sub, reporter)
    }
    val access = context.accessOpt.getOrElse(st"$fname")
    val deps: ST = if (dependsOn.size == 1) st"${dependsOn(0)}" else st"(${(dependsOn, ", ")})"
    return Context(
      path = context.path,
      errNum = subContext.errNum + 1,
      imports = subContext.imports,
      mainDecl = subContext.mainDecl :+ st"val ERROR_$name: Z = ${subContext.errNum}",
      main = context.main :+
        st"""@record trait $name extends ${context.supr}
            |
            |object $name {
            |
            |  ${(subContext.main, "\n\n")}
            |
            |  @enum object Choice {
            |     ${(for (sub <- normSubs) yield st"'${sub.name}", "\n")}
            |  }
            |
            |  def choose($pName: $pType, context: Context): Choice.Type = {
            |    val r: Z = $pBody
            |    r match {
            |      ${(for (i <- 0 until normSubs.size) yield st"""case z"$i" => return Choice.${normSubs(i).name}""", "\n")}
            |      case _ =>
            |        context.signalError(ERROR_HeaderAddress)
            |        return Choice.${normSubs(0).name}
            |    }
            |  }
            |}""",
      owner = context.owner,
      supr = context.supr,
      fields = context.fields :+ st"var $fname: $name",
      inits = context.inits :+ st"$name.${normSubs(0).name}.empty",
      wellFormed = context.wellFormed :+
        st"""val wf$name = $access.wellFormed
            |if (wf$name != 0) {
            |  return wf$name
            |}""",
      decoding = context.decoding :+
        st"""$name.choose($deps, context) match {
            |  ${(for (sub <- normSubs) yield st"case $name.Choice.${sub.name} => $access = $name.${sub.name}.empty", "\n")}
            |}
            |$access.decode(input, context)""",
      encoding = context.encoding :+ st"$access.encode(output, context)",
      members = context.members,
      accessOpt = context.accessOpt)
  }

  def genSpecRepeat(context: BitCodecGen.Context,
                    name: String,
                    dependsOn: ISZ[String],
                    element: Spec,
                    reporter: Reporter): Context = {
    halt("TODO")
  }

  def genSpecRaw(context: BitCodecGen.Context, name: String, dependsOn: ISZ[String], reporter: Reporter): Context = {
    val (pName, pType, pBody): (String, String, String) =
      funNameTypeBody(context.path :+ name, dependsOn.size, name, "Spec.Raw.size", reporter) match {
      case Some((pn, ptpe, pt)) => (pn, ptpe, pt)
      case _ => return context
    }
    val access = context.accessOpt.getOrElse(st"$name")
    val mname = ops.StringOps(name).firstToUpper
    val owner = context.owner
    val deps: ST = if (dependsOn.size == 1) st"${dependsOn(0)}" else st"(${(dependsOn, ", ")})"
    return context(
      errNum = context.errNum + 1,
      mainDecl = context.mainDecl :+ st"val ERROR_${owner}_$name: Z = ${context.errNum}",
      fields = context.fields :+ st"var $name: MSZ[B]",
      inits = context.inits :+ st"MSZ[B]()",
      wellFormed = context.wellFormed :+
        st"""val ${name}Size = sizeOf$mname($deps)
            |if ($name.size != ${name}Size) {
            |  return ERROR_${owner}_$name
            |}""",
      decoding = context.decoding :+
        st"""val ${name}Size = sizeOf$mname($deps)
            |$access = MSZ.create(${name}Size, F)
            |Reader.MS.bleRaw(input, context, $access, ${name}Size)""",
      encoding = context.encoding :+ st"Writer.bleRaw(output, context, $name, $name.size)",
      members = context.members :+
        st"""def sizeOf$mname($pName: $pType): Z = {
            |  val r: Z = $pBody
            |  if (r >= 0) {
            |    return r
            |  } else {
            |    return -1
            |  }
            |}""")
  }

  def genSpecGenUnion(context: BitCodecGen.Context, spec: Spec.GenUnion, reporter: Reporter): Context = {
    halt("TODO")
  }

  def genSpecGenRepeat(context: BitCodecGen.Context, spec: Spec.GenRepeat, reporter: Reporter): Context = {
    halt("TODO")
  }

  def genSpecGenRaw(context: BitCodecGen.Context, spec: Spec.GenRaw, reporter: Reporter): Context = {
    halt("TODO")
  }

  def genSpecPads(context: BitCodecGen.Context, spec: Spec.Pads, reporter: Reporter): Context = {
    val size = spec.size
    val owner = context.owner
    return context(
      decoding = context.decoding :+ st"context.skip(input.size, $size, ERROR_$owner)",
      encoding = context.encoding :+ st"context.skip(output.size, $size, ERROR_$owner)")
  }

  def bitWidth(size: Z): Z = {
    var n = size
    var r = -1
    while (n > 0) {
      n = n / 2
      r = r + 1
    }
    return r
  }

  def funNameTypeBody(path: ISZ[String],
                      dependsOnSize: Z,
                      name: String,
                      spec: String,
                      reporter: Reporter): Option[(String, String, String)] = {
    @pure def isAllNamed(ts: ISZ[AST.Type]): B = {
      for (t <- ts) {
        t match {
          case t: AST.Type.Named if t.typeArgs.isEmpty =>
          case _ => return F
        }
      }
      return T
    }
    funs.get(name) match {
      case Some((f, t)) =>
        val tsize: Z = t match {
          case t: AST.Type.Tuple if isAllNamed(t.args) => t.args.size
          case t: AST.Type.Named if t.typeArgs.isEmpty => 1
          case _ =>
            reporter.error(t.posOpt, kind, s"Invalid $spec function argument type")
            return None()
        }
        if (tsize != dependsOnSize) {
          reporter.error(t.posOpt, kind,
            s"Mismatch $spec function argument type with arity of .dependsOn; expecting $tsize, but found $dependsOnSize")
          return None()
        }
        if (f.params.size != 1) {
          reporter.error(f.posOpt, kind,
            s"Expecting one argument for $spec function, but found ${f.params.size}")
          return None()
        }
        val tpos = t.posOpt.get
        val epos = f.exp.asStmt.posOpt.get
        return Some((f.params(0).id.value, ops.StringOps(text).substring(tpos.offset, tpos.offset + tpos.length),
          ops.StringOps(text).substring(epos.offset, epos.offset + epos.length)))
      case _ =>
        reporter.error(None(), kind, st"Could not find $spec function for ${(path, ".")}".render)
        return None()
    }
  }

  def prevs(name: String): String = {
    return codeSectionMap.get(name).getOrElse("")
  }
}
