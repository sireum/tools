// #Sireum
/*
 Copyright (c) 2020, Robby, Kansas State University
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
import org.sireum.lang.ast.{Exp, ResolvedInfo, Transformer}
import org.sireum.lang.ast.Transformer.PreResult
import org.sireum.message.Reporter

object BitCodecGen {

  @enum object Output {
    'Program
    'Script
    'Json
    'Dot
  }

  @datatype class PosOptTransformer extends Transformer.PrePost[B] {
    val emptyAttr: Option[AST.Attr] = Some(AST.Attr(None()))

    @pure override def preAttr(ctx: B, o: AST.Attr): PreResult[B, AST.Attr] = {
      return PreResult(ctx, F, emptyAttr)
    }

    @pure override def preTypedAttr(ctx: B, o: AST.TypedAttr): PreResult[B, AST.TypedAttr] = {
      return PreResult(ctx, T, Some(o(posOpt = None())))
    }

    @pure override def preResolvedAttr(ctx: B, o: AST.ResolvedAttr): PreResult[B, AST.ResolvedAttr] = {
      return PreResult(ctx, T, Some(o(posOpt = None())))
    }

    @pure override def preExpFun(ctx: B, o: Exp.Fun): PreResult[B, AST.Exp] = {
      return PreResult(ctx, T, Some(o(context = ISZ())))
    }

    @pure override def preResolvedInfoLocalVar(ctx: B, o: ResolvedInfo.LocalVar): PreResult[B, ResolvedInfo] = {
      return PreResult(ctx, T, Some(o(context = ISZ())))
    }
  }

  val kind: String = "bcgen"
  val beginCodeMarker: String = "// BEGIN USER CODE:"
  val endCodeMarker: String = "// END USER CODE:"
  val specName: ISZ[String] = ISZ("org", "sireum", "bitcodec", "Spec")
  val unionName: ISZ[String] = specName :+ "Union"
  val repeatName: ISZ[String] = specName :+ "Repeat"
  val rawName: ISZ[String] = specName :+ "Raw"
  val boundedRepeatName: ISZ[String] = specName :+ "BoundedRepeat"
  val boundedRawName: ISZ[String] = specName :+ "BoundedRaw"

  val funOwners: HashSet[ISZ[String]] = HashSet.empty[ISZ[String]] ++ ISZ(unionName, repeatName, boundedRepeatName, rawName, boundedRawName)
  val funName: HashMap[ISZ[String], String] = HashMap.empty[ISZ[String], String] ++ ISZ(
    unionName ~> "choice",
    repeatName ~> "size",
    boundedRepeatName ~> "size",
    rawName ~> "size",
    boundedRawName ~> "size"
  )
  val notImplemented: String = """halt("Not implemented yet") // TODO"""

  def check(spec: Spec, reporter: Reporter): Unit = {
    def checkNameFirstLower(desc: String, o: Spec): Unit = {
      if (ops.StringOps(o.name).firstToLower != o.name) {
        reporter.error(None(), kind, s"$desc spec should have a name starting with a lower case, but found '${o.name}'.")
      }
    }
    def checkNameFirstUpper(desc: String, o: Spec): Unit = {
      if (ops.StringOps(o.name).firstToUpper != o.name) {
        reporter.error(None(), kind, s"$desc spec should have a name starting with an upper case, but found '${o.name}'.")
      }
    }
    var seenSpecs = HashMap.empty[String, Spec]

    def checkKind(o: Spec): Unit = {
      val name = ops.StringOps(o.name).firstToUpper
      seenSpecs.get(name) match {
        case Some(other) =>
          if (other != o) {
            reporter.error(None(), kind, s"Name '${o.name}' is used for different specs.")
          }
        case _ => seenSpecs = seenSpecs + name ~> o
      }
    }

    def checkSpec(o: Spec): Unit = {
      o match {
        case o: Spec.Boolean => checkNameFirstLower("Boolean", o)
        case o: Spec.Bits => checkNameFirstLower("Bits", o)
        case o: Spec.BytesImpl => checkNameFirstLower("Bytes", o)
        case o: Spec.ShortsImpl => checkNameFirstLower("Shorts", o)
        case o: Spec.IntsImpl => checkNameFirstLower("Ints", o)
        case o: Spec.LongsImpl => checkNameFirstLower("Longs", o)
        case o: Spec.FloatsImpl => checkNameFirstLower("Floats", o)
        case o: Spec.DoublesImpl => checkNameFirstLower("Doubles", o)
        case o: Spec.Enum =>
          checkNameFirstLower("Enum", o)
          checkKind(o)
        case o: Spec.ConcatImpl =>
          checkNameFirstUpper("Concat", o)
          checkKind(o)
          for (e <- o.elements) {
            checkSpec(e)
          }
        case o: Spec.PredUnionImpl =>
          checkNameFirstUpper("PredUnion", o)
          checkKind(o)
          for (p <- o.subs) {
            checkSpec(p.spec)
          }
        case o: Spec.PredRepeatWhileImpl =>
          checkNameFirstLower("PredRepeatWhile", o)
          if (!(o.element.isScalar || o.element.isInstanceOf[Spec.Composite])) {
            checkKind(o.element)
          }
          if (o.preds.isEmpty && o.maxElements < 0) {
            reporter.error(None(), kind, s"${o.name} should either be bounded or is a non-empty predictive spec.")
          }
          checkSpec(o.element)
        case o: Spec.PredRepeatUntilImpl =>
          checkNameFirstLower("PredRepeatUntil", o)
          if (!(o.element.isScalar || o.element.isInstanceOf[Spec.Composite])) {
            checkKind(o.element)
          }
          if (o.preds.isEmpty && o.maxElements < 0) {
            reporter.error(None(), kind, s"${o.name} should either be bounded or is a non-empty predictive spec.")
          }
          checkSpec(o.element)
        case o: Spec.GenUnionImpl =>
          checkNameFirstUpper("GenUnion", o)
          checkKind(o)
          for (e <- o.subs) {
            checkSpec(e)
          }
        case o: Spec.GenRepeatImpl =>
          checkNameFirstLower("GenRepeat", o)
          checkKind(o)
          checkSpec(o.element)
        case o: Spec.GenRawImpl =>
          checkNameFirstLower("GenRaw", o)
          checkKind(o)
        case _: Spec.Pads =>
        case poly: Spec.Poly =>
          val p = poly.polyDesc
          p.compName match {
            case string"Union" =>
              checkNameFirstUpper("Union", o)
              checkKind(o)
            case string"Repeat" =>
              checkNameFirstLower("Repeat", o)
              val element = p.elementsOpt.get(0)
              if (!(element.isScalar || element.isInstanceOf[Spec.Composite])) {
                checkKind(element)
              }
            case string"Raw" => checkNameFirstLower("Raw", o)
          }
          if (p.elementsOpt.nonEmpty) {
            for (e <- p.elementsOpt.get) {
              checkSpec(e)
            }
          }
      }
    }

    checkSpec(spec)
  }

  def gen(output: Output.Type,
          isBigEndian: B,
          licenseOpt: Option[String],
          filename: String,
          packageNames: ISZ[String],
          name: String,
          text: String,
          traits: ISZ[String],
          spec: Spec,
          specJson: ST,
          program: AST.TopUnit.Program,
          prevGen: String,
          reporter: Reporter): ST = {

    val normText = ops.StringOps(text).replaceAllLiterally("\r\n", "\n")

    val collector: EnumFunCollector = EnumFunCollector(normText, Reporter.create)
    collector.transformTopUnit(program)
    reporter.reports(collector.reporter.messages)

    val codeSectionMap = ops.StringOps(ops.StringOps(ops.StringOps(prevGen).
      replaceAllLiterally("\r\n", "\n")).replaceAllLiterally("\n", " \n")).
      collectSections(kind, beginCodeMarker, endCodeMarker, reporter)

    if (!spec.isInstanceOf[Spec.Composite]) {
      reporter.error(None(), kind,
        "Top-level Spec has to be a composite (i.e., Concat, Union or GenUnion)")
    }

    check(spec, reporter)

    if (reporter.hasIssue) {
      return st"$prevGen"
    }

    output match {
      case Output.Dot =>
        return BitCodecGraphGen.specDot(spec, text, collector.enums, collector.funs)
      case Output.Json =>
        var enumMap = Map.empty[String, ISZ[String]]
        for (enum <- collector.enums.values) {
          enumMap = enumMap + enum.id.value ~> enum.elements.map((id: AST.Id) => id.value)
        }

        var funMap = Map.empty[String, AST.Exp.Fun]
        for (p <- collector.funs.entries) {
          funMap = funMap + p._1 ~> p._2._1
        }

        var funTextMap = Map.empty[String, String]
        for (p <- collector.funTexts.entries) {
          funTextMap = funTextMap + p._1 ~> p._2
        }

        val ft = Transformer(PosOptTransformer())

        def printEnumElements(elements: ISZ[String]): ST = {
          return Json.Printer.printISZ(F, elements, Json.Printer.printString _)
        }

        def printFun(o: AST.Exp.Fun): ST = {
          org.sireum.lang.tipe.JSON.Printer.print_astExp(ft.transformExp(T, o).resultOpt.getOrElse(o))
        }

        val maxSize: Z = spec.computeMaxSizeOpt((id: String) => bitWidth(enumMap.get(id).get.size)) match {
          case Some(sz) => sz
          case _ => -1
        }

        return Json.Printer.printObject(ISZ(
          "type" ~> Json.Printer.printString("BitCodecGenSpec"),
          "maxSize" ~> Json.Printer.printZ(maxSize),
          "spec" ~> specJson,
          "enums" ~> Json.Printer.printMap(F, enumMap, Json.Printer.printString _, printEnumElements _),
          "funs" ~> Json.Printer.printMap(F, funMap, Json.Printer.printString _, printFun _),
          "funTexts" ~> Json.Printer.printMap(F, funTextMap, Json.Printer.printString _, Json.Printer.printString _)
        ))
      case _ =>
        val bcGen = BitCodecGen(output == Output.Program, isBigEndian, licenseOpt, filename, packageNames, name,
          normText, traits, spec, program, ops.StringOps(prevGen).replaceAllLiterally("/r/n", "/n"), collector.enums,
          collector.funs, codeSectionMap)

        return bcGen.gen(reporter)
    }
  }

  @record class EnumFunCollector(text: String, reporter: Reporter) extends AST.MTransformer {
    var enums: HashSMap[String, AST.Stmt.Enum] = HashSMap.empty
    var funs: HashSMap[String, (AST.Exp.Fun, AST.Type)] = HashSMap.empty
    var funTexts: HashSMap[String, String] = HashSMap.empty

    override def preExpInvoke(o: AST.Exp.Invoke): AST.MTransformer.PreResult[AST.Exp] = {
      o.attr.resOpt match {
        case Some(res) =>
          res match {
            case m: AST.ResolvedInfo.Method if m.mode == AST.MethodMode.Constructor || m.mode == AST.MethodMode.Method =>
              val className = m.owner :+ m.id
              if (funOwners.contains(className)) {
                val ownerSimpleName = m.id
                val targ = o.targs(0)
                (o.args(0), o.args(2)) match {
                  case (name: AST.Exp.LitString, fun: AST.Exp.Fun) if fun.params.size == 1 =>
                    funs = funs + name.value ~> ((fun, targ))
                    val fpos = fun.posOpt.get
                    funTexts = funTexts + name.value ~> ops.StringOps(text).substring(fpos.offset, fpos.offset + fpos.length)
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

    override def preExpInvokeNamed(o: AST.Exp.InvokeNamed): AST.MTransformer.PreResult[AST.Exp] = {
      o.attr.resOpt match {
        case Some(res) =>
          res match {
            case m: AST.ResolvedInfo.Method if m.mode == AST.MethodMode.Constructor || m.mode == AST.MethodMode.Method =>
              val className = m.owner :+ m.id
              if (funOwners.contains(className)) {
                val ownerSimpleName = m.id
                val targ = o.targs(0)
                def nameOpt: Option[AST.Exp.LitString] = {
                  for (arg <- o.args if arg.id.value == "name") {
                    arg.arg match {
                      case n: AST.Exp.LitString => return Some(n)
                      case _ =>
                        reporter.error(arg.arg.posOpt, kind,
                          s"Invalid name form for $ownerSimpleName; it has to be a String literal")
                    }
                  }
                  return None()
                }
                def funOpt: Option[AST.Exp.Fun] = {
                  for (arg <- o.args) {
                    arg.arg match {
                      case fun: AST.Exp.Fun => return Some(fun)
                      case _ =>
                    }
                  }
                  reporter.error(o.posOpt, kind,
                    s"Invalid ${funName.get(className).get} form for $ownerSimpleName; it has to be a function with a single parameter")
                  return None()
                }
                (nameOpt, funOpt) match {
                  case (Some(name), Some(fun)) =>
                    funs = funs + name.value ~> ((fun, targ))
                    val fpos = fun.posOpt.get
                    funTexts = funTexts + name.value ~> ops.StringOps(text).substring(fpos.offset, fpos.offset + fpos.length)
                  case _ =>
                }
              }
            case _ =>
          }
        case _ =>
      }
      return super.preExpInvokeNamed(o)
    }

    override def preStmtEnum(o: AST.Stmt.Enum): AST.MTransformer.PreResult[AST.Stmt] = {
      enums = enums + o.id.value ~> o
      return super.preStmtEnum(o)
    }
  }

  object Context {
    def create(isupers: ISZ[String]): Context = {
      return Context(ISZ(), 2, ISZ.create(65, F), ISZ.create(65, F), ISZ(), ISZ(), "", "Runtime.Composite",
        isupers, ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), HashMap.empty, 0, HashSet.empty)
    }
  }

  @datatype class Context(path: ISZ[String],
                          errNum: Z,
                          imports: ISZ[B],
                          simports: ISZ[B],
                          mainDecl: ISZ[ST],
                          main: ISZ[ST],
                          owner: String,
                          supr: String,
                          isupers: ISZ[String],
                          fields: ISZ[ST],
                          ifields: ISZ[ST],
                          i2m: ISZ[ST],
                          m2i: ISZ[ST],
                          tpeInits: ISZ[(ST, ST)],
                          wellFormed: ISZ[ST],
                          decoding: ISZ[ST],
                          encoding: ISZ[ST],
                          members: ISZ[ST],
                          omembers: ISZ[ST],
                          fieldMap: HashMap[ISZ[String], Z],
                          nextFound: Z,
                          seenSpecs: HashSet[String]) {
    def freshFound: String = {
      return if (nextFound == 0) "" else s"$nextFound"
    }

    def fieldName(container: ISZ[String], name: String): (Context, String) = {
      val fname = ops.StringOps(name).firstToLower
      val key = container :+ fname
      fieldMap.get(key) match {
        case Some(n) => return (this (fieldMap = fieldMap + key ~> (n + 1)), s"$fname$n")
        case _ => return (this (fieldMap = fieldMap + key ~> 2), fname)
      }
    }
  }

  def bitWidth(size: Z): Z = {
    var n = size - 1
    var r = 0
    while (n > 0) {
      n = n / 2
      r = r + 1
    }
    return r
  }

  def reorientLines(text: String, column: Z): String = {
    def removeFirstLastWhitespaces(line: String, size: Z): String = {
      val cs = conversions.String.toCis(line)
      var ok = T
      var i = 0
      while (i < size && i < line.size && ok) {
        if (!cs(i).isWhitespace) {
          ok = F
        } else {
          i = i + 1
        }
      }
      var j = line.size - 1
      while (j >= 0 && cs(j).isWhitespace) {
        j = j - 1
      }
      j = j + 1
      return ops.StringOps(line).substring(if (ok) size else i, j)
    }

    val lines = ops.StringOps(ops.StringOps(text).replaceAllLiterally("\n", " \n")).
      split((c: C) => c == '\n')
    var firstLineIndex: Z = -1
    for (i <- 0 until lines.size if firstLineIndex < 0 && ops.StringOps(lines(i)).trim.size > 0) {
      firstLineIndex = i
    }
    if (firstLineIndex < 0) {
      return text
    }
    val firstLineChars = conversions.String.toCis(lines(firstLineIndex))
    var offset: Z = 0
    if (column >= 0) {
      offset = column
    } else {
      while (offset < firstLineChars.size && firstLineChars(offset).isWhitespace) {
        offset = offset + 1
      }
    }
    return st"${(for (line <- lines) yield removeFirstLastWhitespaces(line, offset), "\n")}".render
  }
}

import BitCodecGen._

@datatype class BitCodecGen(isProgram: B,
                            isBigEndian: B,
                            licenseOpt: Option[String],
                            filename: String,
                            packageNames: ISZ[String],
                            name: String,
                            text: String,
                            traits: ISZ[String],
                            topSpec: Spec,
                            program: AST.TopUnit.Program,
                            prevGen: String,
                            enums: HashSMap[String, AST.Stmt.Enum],
                            funs: HashSMap[String, (AST.Exp.Fun, AST.Type)],
                            codeSectionMap: HashSMap[String, String]) {

  val endianPrefix: String = if (isBigEndian) "be" else "le"
  val enumMaxSize: String => Z@pure = (id: String) => bitWidth(enums.get(id).get.elements.size)
  val isBounded: B = topSpec.computeMaxSizeOpt(enumMaxSize).nonEmpty

  def gen(reporter: Reporter): ST = {
    val context = genSpec(Context.create(traits), topSpec, reporter)
    if (reporter.hasIssue) {
      return st"$prevGen"
    }
    val packageOpt: Option[ST] =
      if (isProgram) if (packageNames.nonEmpty) Some(st"package ${(packageNames, ".")}") else None()
      else None()
    val lOpt = licenseOpt.map((s: String) =>
      st"""/*
          | ${ops.StringOps(s).trim}
          | */
          |""")
    val testOpt: Option[ST] =
      if (isProgram) None()
      else Some(
        st"""
            |$beginCodeMarker Test
            |${prevText("Test", "")}
            |$endCodeMarker Test""")

    val r =
      st"""// #Sireum
          |$lOpt
          |$packageOpt
          |
          |import org.sireum._
          |${(for (i <- 1 to 64 if context.imports(i)) yield st"import org.sireum.U$i._", "\n")}
          |${(for (i <- 1 to 64 if context.simports(i)) yield st"import org.sireum.S$i._", "\n")}
          |import org.sireum.ops.Bits.{Context, Reader, Writer}
          |import org.sireum.bitcodec.Runtime
          |
          |$beginCodeMarker Imports
          |${prevText("Imports", "")}
          |$endCodeMarker Imports
          |
          |object ${if (isProgram) name else "BitCodec"} {
          |
          |  ${(context.mainDecl, "\n\n")}
          |
          |  $beginCodeMarker Members
          |  ${prevText("Members", "")}
          |  $endCodeMarker Members
          |
          |  ${(context.main, "\n\n")}
          |
          |}
          |$testOpt"""
    return r
  }

  def genSpec(context: Context, o: Spec, reporter: Reporter): Context = {
    def firstContext(name: String): (B, Context) = {
      if (context.seenSpecs.contains(name))  {
        return (F, context)
      } else {
        return (T, context(seenSpecs = context.seenSpecs + name))
      }
    }
    def repeatFirstContext(rname: String): (B, Context) = {
      val name = s"${context.owner}_$rname"
      if (context.seenSpecs.contains(name))  {
        return (F, context)
      } else {
        return (T, context(seenSpecs = context.seenSpecs + name))
      }
    }
    o match {
      case o: Spec.Boolean => return genSpecBoolean(context, o, reporter)
      case o: Spec.Bits => return genSpecBits(context, o, reporter)
      case o: Spec.BytesImpl => return genSpecEights(context, o.name, 8, o.size, o.signed, o.minOpt, o.maxOpt, reporter)
      case o: Spec.ShortsImpl => return genSpecEights(context, o.name, 16, o.size, o.signed, o.minOpt, o.maxOpt, reporter)
      case o: Spec.IntsImpl => return genSpecEights(context, o.name, 32, o.size, o.signed, o.minOpt, o.maxOpt, reporter)
      case o: Spec.LongsImpl => return genSpecEights(context, o.name, 64, o.size, o.signed, o.minOpt, o.maxOpt, reporter)
      case o: Spec.FloatsImpl =>
        return genSpecFPs(context, o.name, T, o.size, o.minOpt.map((v: F32) => s"$v"),
          o.maxOpt.map((v: F32) => s"$v"), reporter)
      case o: Spec.DoublesImpl =>
        return genSpecFPs(context, o.name, F, o.size, o.minOpt.map((v: F64) => s"$v"),
          o.maxOpt.map((v: F64) => s"$v"), reporter)
      case o: Spec.Enum =>
        val (first, ctx) = firstContext(o.objectName)
        return genSpecEnum(first, ctx, o, reporter)
      case o: Spec.ConcatImpl =>
        val (first, ctx) = firstContext(o.name)
        return genSpecConcat(first, ctx, o, reporter)
      case o: Spec.PredUnionImpl =>
        val (first, ctx) = firstContext(o.name)
        return genSpecPredUnion(first, ctx, o, reporter)
      case o: Spec.PredRepeatWhileImpl =>
        val (first, ctx) = repeatFirstContext(o.name)
        genSpecPredRepeat(first, ctx, o.name, o.maxElements, T, o.preds, o.element, reporter)
      case o: Spec.PredRepeatUntilImpl =>
        val (first, ctx) = repeatFirstContext(o.name)
        genSpecPredRepeat(first, ctx, o.name, o.maxElements, F, o.preds, o.element, reporter)
      case o: Spec.GenUnionImpl =>
        val (first, ctx) = firstContext(o.name)
        return genSpecGenUnion(first, ctx, o, reporter)
      case o: Spec.GenRepeatImpl =>
        val (first, ctx) = firstContext(o.name)
        return genSpecGenRepeat(first, ctx, o, reporter)
      case o: Spec.GenRawImpl =>
        val (first, ctx) = firstContext(o.name)
        return genSpecGenRaw(first, ctx, o, reporter)
      case o: Spec.Pads => return genSpecPads(context, o, reporter)
      case poly: Spec.Poly =>
        val p = poly.polyDesc
        p.compName match {
          case string"Union" =>
            val (first, ctx) = firstContext(o.name)
            return genSpecUnion(first, ctx, p.name, o.maxSizeOpt(enumMaxSize), p.dependsOn, p.elementsOpt.get, p.asOpt, reporter)
          case string"Repeat" =>
            val (first, ctx) = repeatFirstContext(o.name)
            return genSpecRepeat(first, ctx, p.name, p.max, p.dependsOn, p.elementsOpt.get(0), reporter)
          case string"Raw" => return genSpecRaw(context, p.name, p.max, p.dependsOn, reporter)
        }
    }
  }

  def genSpecBoolean(context: Context, spec: Spec.Boolean, reporter: Reporter): Context = {
    val name = spec.name
    val tpe = st"B"
    return context(
      fields = context.fields :+ st"var $name: $tpe",
      ifields = context.ifields :+ st"val $name: $tpe",
      i2m = context.i2m :+ st"$name",
      m2i = context.m2i :+ st"$name",
      tpeInits = context.tpeInits :+ ((tpe, st"F")),
      decoding = context.decoding :+ st"$name = Reader.IS.bleB(input, context)",
      encoding = context.encoding :+ st"Writer.bleB(output, context, $name)")
  }

  def genSpecBits(context: Context, spec: Spec.Bits, reporter: Reporter): Context = {
    val name = spec.name
    val size = spec.size
    val prefix: String = if (size <= 8) "ble" else endianPrefix
    if (size < 1) {
      reporter.error(None(), kind, st"Size must be >= 1 for Spec.Bits ${(context.path :+ name, ".")}".render)
      return context
    } else if (size <= 64) {
      val tpe = st"U$size"
      return context(
        imports = context.imports(size ~> T),
        fields = context.fields :+ st"var $name: $tpe",
        ifields = context.ifields :+ st"val $name: $tpe",
        i2m = context.i2m :+ st"$name",
        m2i = context.m2i :+ st"$name",
        tpeInits = context.tpeInits :+ ((tpe, st"""u$size"0"""")),
        decoding = context.decoding :+ st"$name = Reader.IS.${prefix}U$size(input, context)",
        encoding = context.encoding :+ st"Writer.${prefix}U$size(output, context, $name)")
    } else {
      val tpe = st"MSZ[B]"
      val itpe = st"ISZ[B]"
      return context(
        fields = context.fields :+ st"var $name: $tpe",
        ifields = context.ifields :+ st"val $name: $itpe",
        i2m = context.i2m :+ st"$name.toMS",
        m2i = context.m2i :+ st"$name.toIS",
        tpeInits = context.tpeInits :+ ((tpe, st"MSZ.create($size, F)")),
        wellFormed = context.wellFormed :+
          st"""if ($name.size != $size) {
              |  return ERROR_${context.owner}
              |}""",
        decoding = context.decoding :+ st"Reader.IS.${prefix}BS(input, context, $name, $size)",
        encoding = context.encoding :+ st"Writer.${prefix}BS(output, context, $name)")
    }
  }

  def genSpecEights(context: Context, name: String, n: Z, size: Z, signed: B, minOpt: Option[Z], maxOpt: Option[Z],
                    reporter: Reporter): Context = {
    val us: String = if (signed) "s" else "u"
    val US: String = if (signed) "S" else "U"
    if (size == 1) {
      val tpe = st"$US$n"
      val wfs: ISZ[ST] = (minOpt, maxOpt) match {
        case (Some(min), Some(max)) =>
          if (min == max) {
            ISZ(
              st"""if ($name != $us$n"${min}") {
                  |  return ERROR_${context.owner}
                  |}""")
          } else {
            ISZ(
              st"""if ($name < $us$n"$min" || $name > $us$n"$max") {
                  |  return ERROR_${context.owner}
                  |}""")
          }
        case _ => ISZ[ST]()
      }
      val prefix: String = if (n <= 8) "ble" else endianPrefix
      return context(
        imports = if (signed) context.imports else context.imports(n ~> T),
        simports = if (signed) context.simports(n ~> T) else context.simports,
        fields = context.fields :+ st"var $name: $tpe",
        ifields = context.ifields :+ st"val $name: $tpe",
        i2m = context.i2m :+ st"$name",
        m2i = context.m2i :+ st"$name",
        tpeInits = context.tpeInits :+ ((tpe, st"""$us$n"0"""")),
        wellFormed = context.wellFormed ++ wfs,
        decoding = context.decoding :+ st"$name = Reader.IS.${prefix}$US$n(input, context)",
        encoding = context.encoding :+ st"Writer.${prefix}$US$n(output, context, $name)")
    } else {
      val tpe = st"MSZ[$US$n]"
      val itpe = st"ISZ[$US$n]"
      var wfs: ISZ[ST] = ISZ(
        st"""if ($name.size != $size) {
            |  return ERROR_${context.owner}
            |}"""
      )
      (minOpt, maxOpt) match {
        case (Some(min), Some(max)) =>
          if (min == max) {
            wfs = wfs :+
              st"""for (${name}Element <- $name) {
                  |  if (${name}Element != $us$n"${min}") {
                  |    return ERROR_${context.owner}
                  |  }
                  |}"""
          } else {
            wfs = wfs :+
              st"""for (${name}Element <- $name) {
                  |  if (${name}Element < $us$n"$min" || ${name}Element > $us$n"$max") {
                  |    return ERROR_${context.owner}
                  |  }
                  |}"""
          }
        case _ =>
      }
      return context(
        imports = if (signed) context.imports else context.imports(n ~> T),
        simports = if (signed) context.simports(n ~> T) else context.simports,
        fields = context.fields :+ st"var $name: $tpe",
        ifields = context.ifields :+ st"val $name: $itpe",
        i2m = context.i2m :+ st"$name.toMS",
        m2i = context.m2i :+ st"$name.toIS",
        tpeInits = context.tpeInits :+ ((tpe, st"""MSZ.create($size, $us$n"0")""")),
        wellFormed = context.wellFormed ++ wfs,
        decoding = context.decoding :+ st"Reader.IS.${endianPrefix}$US${n}S(input, context, $name, $size)",
        encoding = context.encoding :+ st"Writer.${endianPrefix}$US${n}S(output, context, $name)")
    }
  }

  def genSpecFPs(context: Context, name: String, isSingle: B, size: Z, minOpt: Option[String], maxOpt: Option[String],
                 reporter: Reporter): Context = {
    val (n, suffix): (ST, ST) = if (isSingle) (st"32", st"f") else (st"64", st"d")
    val init: ST = if (isSingle) st"0.0f" else st"0.0d"
    if (size == 1) {
      val tpe = st"F$n"
      val wfs: ISZ[ST] = (minOpt, maxOpt) match {
        case (Some(min), Some(max)) =>
          ISZ(
            st"""if ($name < $min$suffix || $name > $max$suffix) {
                |  return ERROR_${context.owner}
                |}""")
        case _ => ISZ[ST]()
      }
      return context(
        imports = context.imports,
        simports = context.simports,
        fields = context.fields :+ st"var $name: $tpe",
        ifields = context.ifields :+ st"val $name: $tpe",
        i2m = context.i2m :+ st"$name",
        m2i = context.m2i :+ st"$name",
        tpeInits = context.tpeInits :+ ((tpe, init)),
        wellFormed = context.wellFormed ++ wfs,
        decoding = context.decoding :+ st"$name = Reader.IS.${endianPrefix}F$n(input, context)",
        encoding = context.encoding :+ st"Writer.${endianPrefix}F$n(output, context, $name)")
    } else {
      val tpe = st"MSZ[F$n]"
      val itpe = st"ISZ[F$n]"
      var wfs: ISZ[ST] = ISZ(
        st"""if ($name.size != $size) {
            |  return ERROR_${context.owner}
            |}"""
      )
      (minOpt, maxOpt) match {
        case (Some(min), Some(max)) =>
          wfs = wfs :+
            st"""for (${name}Element <- $name) {
                |  if (${name}Element < $min$suffix || ${name}Element > $max$suffix) {
                |    return ERROR_${context.owner}
                |  }
                |}"""
        case _ =>
      }
      return context(
        imports = context.imports,
        simports = context.simports,
        fields = context.fields :+ st"var $name: $tpe",
        ifields = context.ifields :+ st"val $name: $itpe",
        i2m = context.i2m :+ st"$name.toMS",
        m2i = context.m2i :+ st"$name.toIS",
        tpeInits = context.tpeInits :+ ((tpe, st"""MSZ.create($size, $init)""")),
        wellFormed = context.wellFormed ++ wfs,
        decoding = context.decoding :+ st"Reader.IS.${endianPrefix}F${n}S(input, context, $name, $size)",
        encoding = context.encoding :+ st"Writer.${endianPrefix}F${n}S(output, context, $name)")
    }
  }

  def genSpecEnum(first: B, context: Context, spec: Spec.Enum, reporter: Reporter): Context = {
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
    val firstElem = enum.elements(0).value
    val tpe = st"$objectName.Type"
    val prefix: String = if (size <= 8) "ble" else endianPrefix
    return context(
      imports = context.imports(size ~> T),
      simports = context.simports,
      errNum = context.errNum + 1,
      mainDecl = if (!first) context.mainDecl else context.mainDecl :+ st"val ERROR_$objectName: Z = ${context.errNum}",
      main = if (!first) context.main else context.main :+
        st"""@enum object $objectName {
            |  ${(for (element <- enum.elements) yield st"'${element.value}", "\n")}
            |}
            |
            |def decode$objectName(input: ISZ[B], context: Context): $tpe = {
            |  if (context.offset + $size > input.size) {
            |    context.signalError(ERROR_$objectName)
            |  }
            |  if (context.hasError) {
            |    return $objectName.$firstElem
            |  }
            |  val r: $tpe = Reader.IS.${prefix}U$size(input, context) match {
            |    ${(for (i <- 0 until enum.elements.size) yield st"""case u$size"$i" => $objectName.${enum.elements(i).value}""", "\n")}
            |    case _ =>
            |      context.signalError(ERROR_$objectName)
            |      $objectName.$firstElem
            |  }
            |  return r
            |}
            |
            |def encode$objectName(output: MSZ[B], context: Context, $name: $tpe): Unit = {
            |  if (context.offset + $size > output.size) {
            |    context.signalError(ERROR_$objectName)
            |  }
            |  if (context.hasError) {
            |    return
            |  }
            |  $name match {
            |    ${(for (i <- 0 until enum.elements.size) yield st"""case $objectName.${enum.elements(i).value} => Writer.${if (size <= 8) "ble" else endianPrefix}U$size(output, context, u$size"$i")""", "\n")}
            |  }
            |}""",
      fields = context.fields :+ st"var $name: $tpe",
      ifields = context.ifields :+ st"val $name: $tpe",
      i2m = context.i2m :+ st"$name",
      m2i = context.m2i :+ st"$name",
      tpeInits = context.tpeInits :+ ((tpe, st"""$objectName.$firstElem""")),
      decoding = context.decoding :+ st"$name = decode$objectName(input, context)",
      encoding = context.encoding :+ st"encode$objectName(output, context, $name)")
  }

  @pure def isupers(context: Context): ST = {
    if (context.isupers.nonEmpty) {
      return st" extends ${(context.isupers, " with ")}"
    } else {
      return st""
    }
  }

  def genSpecConcat(first: B, context: Context, spec: Spec.ConcatImpl, reporter: Reporter): Context = {
    val name = spec.name
    val (ctx, fname) = context.fieldName(context.path, spec.asOpt.getOrElse(name))
    var elementContext = ctx(path = context.path :+ fname, owner = name, supr = "Runtime.Composite", isupers = traits,
      fields = ISZ(), ifields = ISZ(), i2m = ISZ(), m2i = ISZ(), tpeInits = ISZ(), wellFormed = ISZ(), decoding = ISZ(),
      encoding = ISZ(), members = ISZ(), omembers = ISZ())
    for (element <- spec.elements) {
      elementContext = genSpec(elementContext, element, reporter)
    }
    val encode: ST = spec.maxSizeOpt(enumMaxSize) match {
      case Some(n) if isBounded =>
        st"""def encode(context: Context): Option[ISZ[B]] = {
            |  val buffer = MSZ.create($n, F)
            |  toMutable.encode(buffer, context)
            |  return if (context.hasError) None[ISZ[B]]() else Some(buffer.toIS)
            |}"""
      case _ =>
        st"""def encode(buffSize: Z, context: Context): Option[ISZ[B]] = {
            |  val buffer = MSZ.create(buffSize, F)
            |  toMutable.encode(buffer, context)
            |  return if (context.hasError) None[ISZ[B]]() else Some(buffer.toIS)
            |}"""
    }
    val wfName = s"wf${spec.asOpt.getOrElse(name)}"
    return Context(
      path = context.path,
      errNum = elementContext.errNum + 1,
      imports = elementContext.imports,
      simports = elementContext.simports,
      mainDecl = if (!first) elementContext.mainDecl else elementContext.mainDecl :+ st"val ERROR_$name: Z = ${elementContext.errNum}",
      main = if (!first) elementContext.main else elementContext.main :+
        st"""object $name {
            |
            |  val maxSize: Z = z"${spec.computeMaxSizeOpt(enumMaxSize).getOrElse(-1)}"
            |
            |  def empty: M$name = {
            |    return M$name(${(elementContext.tpeInits.map((p: (ST, ST)) => p._2), ", ")})
            |  }
            |
            |  def decode(input: ISZ[B], context: Context): Option[$name] = {
            |    val r = empty
            |    r.decode(input, context)
            |    return if (context.hasError) None[$name]() else Some(r.toImmutable)
            |  }
            |
            |  ${(elementContext.omembers, "\n\n")}
            |}
            |
            |@datatype class $name(
            |  ${(elementContext.ifields, ",\n")}
            |)${isupers(context)} {
            |
            |  @strictpure def toMutable: M$name = M$name(${(elementContext.i2m, ", ")})
            |
            |  $encode
            |
            |  def wellFormed: Z = {
            |    return toMutable.wellFormed
            |  }
            |}
            |
            |@record class M$name(
            |  ${(elementContext.fields, ",\n")}
            |) extends ${context.supr} {
            |
            |  @strictpure def toImmutable: $name = $name(${(elementContext.m2i, ", ")})
            |
            |  def wellFormed: Z = {
            |
            |    ${(elementContext.wellFormed, "\n\n")}
            |
            |    $beginCodeMarker $name.wellFormed
            |    ${prevText(s"$name.wellFormed", "")}
            |    $endCodeMarker $name.wellFormed
            |
            |    return 0
            |  }
            |
            |  def decode(input: ISZ[B], context: Context): Unit = {
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
      isupers = context.isupers,
      fields = context.fields :+ st"var $fname: M$name",
      ifields = context.ifields :+ st"val $fname: $name",
      i2m = context.i2m :+ st"$fname.toMutable",
      m2i = context.m2i :+ st"$fname.toImmutable",
      tpeInits = context.tpeInits :+ ((st"M$name", st"$name.empty")),
      wellFormed = context.wellFormed :+
        st"""val $wfName = $fname.wellFormed
            |if ($wfName != 0) {
            |  return $wfName
            |}""",
      decoding = context.decoding :+ st"$fname.decode(input, context)",
      encoding = context.encoding :+ st"$fname.encode(output, context)",
      members = context.members,
      omembers = context.omembers,
      fieldMap = elementContext.fieldMap,
      nextFound = 0,
      seenSpecs = elementContext.seenSpecs
    )
  }

  def genSpecUnion(first: B,
                   context: Context,
                   name: String,
                   maxSizeOpt: Option[Z],
                   dependsOn: ISZ[String],
                   subs: ISZ[Spec],
                   asOpt: Option[String],
                   reporter: Reporter): Context = {
    val normSubs: ISZ[Spec] = for (sub <- subs) yield
      if (sub.isInstanceOf[Spec.Composite]) sub else Spec.Concat(ops.StringOps(sub.name).firstToUpper, ISZ(sub))
    val (pName, pType, pBody): (String, String, String) =
      funNameTypeBody(context.path :+ name, dependsOn.size, name, "Spec.Union.choice", reporter) match {
        case Some((pn, ptpe, pt)) => (pn, ptpe, pt)
        case _ => return context
      }
    val (ctx, fname) = context.fieldName(context.path, asOpt.getOrElse(name))
    var subContext = ctx(path = ctx.path :+ fname, main = ctx.main, owner = name, supr = s"M$name", isupers = ISZ(name),
      fields = ISZ(), ifields = ISZ(), i2m = ISZ(), m2i = ISZ(), tpeInits = ISZ(), wellFormed = ISZ(), decoding = ISZ(),
      encoding = ISZ(), members = ISZ(), omembers = ISZ())
    for (sub <- normSubs) {
      subContext = genSpec(subContext, sub, reporter)
    }
    val deps: ST = if (dependsOn.size == 1) st"${dependsOn(0)}" else st"(${(dependsOn, ", ")})"
    val encode: ST =
      if (isBounded) st"def encode(context: Context): Option[ISZ[B]]"
      else st"def encode(buffSize: Z, context: Context): Option[ISZ[B]]"
    val wfName = s"wf${asOpt.getOrElse(name)}"
    return Context(
      path = context.path,
      errNum = subContext.errNum + 1,
      imports = subContext.imports,
      simports = subContext.simports,
      mainDecl = if (!first) subContext.mainDecl else subContext.mainDecl :+ st"val ERROR_$name: Z = ${subContext.errNum}",
      main = if (!first) subContext.main else subContext.main :+
        st"""@datatype trait $name${isupers(context)} {
            |  @strictpure def toMutable: M$name
            |  $encode
            |  def wellFormed: Z
            |}
            |
            |@record trait M$name extends ${context.supr} {
            |  @strictpure def toImmutable: $name
            |}
            |
            |object $name {
            |
            |  val maxSize: Z = z"${maxSizeOpt.getOrElse(-1)}"
            |
            |  def empty: M$name = {
            |    return ${normSubs(0).name}.empty
            |  }
            |
            |  def decode(input: ISZ[B], context: Context): Option[$name] = {
            |    val r = empty
            |    r.decode(input, context)
            |    return if (context.hasError) None[$name]() else Some(r.toImmutable)
            |  }
            |
            |  @enum object Choice {
            |     ${(for (sub <- normSubs) yield st"'${sub.name}", "\n")}
            |     'Error
            |  }
            |
            |  def choose($pName: $pType): Choice.Type = {
            |    val r: Z = {
            |      $pBody
            |    }
            |    r match {
            |      ${(for (i <- 0 until normSubs.size) yield st"""case z"$i" => return Choice.${normSubs(i).name}""", "\n")}
            |      case _ =>
            |    }
            |    return Choice.Error
            |  }
            |
            |  ${(subContext.omembers, "\n\n")}
            |}""",
      owner = context.owner,
      supr = context.supr,
      isupers = context.isupers,
      fields = context.fields :+ st"var $fname: M$name",
      ifields = context.ifields :+ st"val $fname: $name",
      i2m = context.i2m :+ st"$fname.toMutable",
      m2i = context.m2i :+ st"$fname.toImmutable",
      tpeInits = context.tpeInits :+ ((st"M$name", st"${normSubs(0).name}.empty")),
      wellFormed = context.wellFormed :+
        st"""($name.choose($deps), $fname) match {
            |  ${(for (sub <- normSubs) yield st"case ($name.Choice.${sub.name}, _: M${sub.name}) =>", "\n")}
            |  case _ => return ERROR_$name
            |}
            |
            |val $wfName = $fname.wellFormed
            |if ($wfName != 0) {
            |  return $wfName
            |}""",
      decoding = context.decoding :+
        st"""$name.choose($deps) match {
            |  ${(for (sub <- normSubs) yield st"case $name.Choice.${sub.name} => $fname = ${sub.name}.empty", "\n")}
            |  case _ => context.signalError(ERROR_$name)
            |}
            |$fname.decode(input, context)""",
      encoding = context.encoding :+ st"$fname.encode(output, context)",
      members = context.members,
      omembers = context.omembers,
      fieldMap = subContext.fieldMap,
      nextFound = 0,
      seenSpecs = subContext.seenSpecs
    )
  }

  def genSpecPredUnion(first: B,
                       context: Context,
                       o: Spec.PredUnionImpl,
                       reporter: Reporter): Context = {
    val name = o.name
    val subs = o.subs
    val normSubs: ISZ[Spec.PredSpec] = for (sub <- subs) yield
      if (sub.spec.isInstanceOf[Spec.Composite]) sub
      else sub(spec = Spec.Concat(ops.StringOps(sub.spec.name).firstToUpper, ISZ(sub.spec)))
    val (ctx, fname) = context.fieldName(context.path, o.asOpt.getOrElse(name))
    val path = ctx.path :+ fname
    var subContext = ctx(path = path, main = ctx.main, owner = name, supr = s"M$name", isupers = ISZ(name),
      fields = ISZ(), ifields = ISZ(), i2m = ISZ(), m2i = ISZ(), tpeInits = ISZ(), wellFormed = ISZ(), decoding = ISZ(),
      encoding = ISZ(), members = ISZ(), omembers = ISZ())
    var choose = ISZ[ST]()
    for (i <- 0 until normSubs.size) {
      val sub = normSubs(i)
      var preds = ISZ[ST]()
      for (pred <- sub.preds) {
        val p = genSpecPred(subContext, pred, reporter)
        subContext = p._1
        preds = preds :+ p._2
      }
      choose = choose :+
        st"""{
            |  var ctx = context
            |  var hasError = F
            |  ${(preds, "\n")}
            |  if (!hasError && ctx.errorCode == 0) {
            |    return Choice.${sub.spec.name}
            |  }
            |}"""
      subContext = genSpec(subContext, sub.spec, reporter)
    }
    val encode: ST =
      if (isBounded) st"def encode(context: Context): Option[ISZ[B]]"
      else st"def encode(buffSize: Z, context: Context): Option[ISZ[B]]"
    return Context(
      path = context.path,
      errNum = subContext.errNum + 1,
      imports = subContext.imports,
      simports = subContext.simports,
      mainDecl = if (!first) subContext.mainDecl else subContext.mainDecl :+ st"val ERROR_$name: Z = ${subContext.errNum}",
      main = if (!first) subContext.main else subContext.main :+
        st"""@datatype trait $name${isupers(context)} {
            |  @strictpure def toMutable: M$name
            |  $encode
            |  def wellFormed: Z
            |}
            |
            |@record trait M$name extends ${context.supr} {
            |  @strictpure def toImmutable: $name
            |}
            |
            |object $name {
            |
            |  val maxSize: Z = z"${o.maxSizeOpt(enumMaxSize).getOrElse(-1)}"
            |
            |  def empty: M$name = {
            |    return ${normSubs(0).spec.name}.empty
            |  }
            |
            |  def decode(input: ISZ[B], context: Context): Option[$name] = {
            |    val r = empty
            |    r.decode(input, context)
            |    return if (context.hasError) None[$name]() else Some(r.toImmutable)
            |  }
            |
            |  @enum object Choice {
            |     ${(for (sub <- normSubs) yield st"'${sub.spec.name}", "\n")}
            |     'Error
            |  }
            |
            |  def choose(input: ISZ[B], context: Context): Choice.Type = {
            |    ${(choose, "\n;")}
            |    return Choice.Error
            |  }
            |
            |  ${(subContext.omembers, "\n\n")}
            |}""",
      owner = context.owner,
      supr = context.supr,
      isupers = context.isupers,
      fields = context.fields :+ st"var $fname: M$name",
      ifields = context.ifields :+ st"val $fname: $name",
      i2m = context.i2m :+ st"$fname.toMutable",
      m2i = context.m2i :+ st"$fname.toImmutable",
      tpeInits = context.tpeInits :+ ((st"M$name", st"${normSubs(0).spec.name}.empty")),
      wellFormed = context.wellFormed,
      decoding = context.decoding :+
        st"""$name.choose(input, context) match {
            |  ${(for (sub <- normSubs) yield st"case $name.Choice.${sub.spec.name} => $fname = ${sub.spec.name}.empty", "\n")}
            |  case _ => context.signalError(ERROR_$name)
            |}
            |$fname.decode(input, context)""",
      encoding = context.encoding :+ st"$fname.encode(output, context)",
      members = context.members,
      omembers = context.omembers,
      fieldMap = subContext.fieldMap,
      nextFound = 0,
      seenSpecs = subContext.seenSpecs
    )
  }

  def genSpecRepeat(first: B,
                    context: Context,
                    name: String,
                    maxElements: Z,
                    dependsOn: ISZ[String],
                    element: Spec,
                    reporter: Reporter): Context = {
    val normElement: Spec =
      if (element.isScalar || element.isInstanceOf[Spec.Composite]) element
      else Spec.Concat(ops.StringOps(element.name).firstToUpper, ISZ(element))
    val (pName, pType, pBody): (String, String, String) =
      funNameTypeBody(context.path :+ name, dependsOn.size, name,
        if (maxElements > 0) "Spec.BoundedRepeat.size" else "Spec.Repeat.size", reporter) match {
        case Some((pn, ptpe, pt)) => (pn, ptpe, pt)
        case _ => return context
      }
    val mname = ops.StringOps(name).firstToUpper
    val owner = context.owner
    val deps: ST = if (dependsOn.size == 1) st"${dependsOn(0)}" else st"(${(dependsOn, ", ")})"
    var elementContext = context(path = context.path :+ name, main = context.main, owner = st"${owner}_$name".render,
      supr = "Runtime.Composite", isupers = traits, fields = ISZ(), ifields = ISZ(), i2m = ISZ(), m2i = ISZ(),
      tpeInits = ISZ(), wellFormed = ISZ(), decoding = ISZ(),
      encoding = ISZ(), members = ISZ())
    elementContext = genSpec(elementContext, normElement, reporter)
    val (tpe, itpe): (ST, ST) =
      if (element.isScalar) (st"MSZ[${elementContext.tpeInits(0)._1}]", st"ISZ[${elementContext.tpeInits(0)._1}]")
      else (st"MSZ[M${normElement.name}]", st"ISZ[${normElement.name}]")
    var wf = ISZ(
      st"""val ${name}Sz = sizeOf$mname($deps)
          |if ($name.size != ${name}Sz) {
          |  return ERROR_${owner}_$name
          |}"""
    )
    if (maxElements > 0) {
      wf =
        st"""if ($name.size > $maxElements) {
            |  return ERROR_${owner}_$name
            |}""" +: wf
    }
    val elementTpe = elementContext.tpeInits(0)._2
    val (decode, encode): (ST, ST) =
      if (element.isScalar) {
        (
          st"""val ${elementContext.decoding(0)}
              |$name(i) = ${element.name}""",
          st"""val ${element.name} = $name(i)
              |${elementContext.encoding(0)}"""
        )
      } else {
        (st"$name(i).decode(input, context)", st"$name(i).encode(output, context)")
      }
    return Context(
      path = context.path,
      errNum = elementContext.errNum + 1,
      imports = elementContext.imports,
      simports = elementContext.simports,
      mainDecl = if (!first) elementContext.mainDecl else elementContext.mainDecl :+ st"val ERROR_${owner}_$name: Z = ${elementContext.errNum}",
      main = elementContext.main,
      owner = context.owner,
      supr = context.supr,
      isupers = context.isupers,
      fields = context.fields :+ st"var $name: $tpe",
      ifields = context.ifields :+ st"val $name: $itpe",
      i2m = context.i2m :+ st"$owner.toMutable$mname($name)",
      m2i = context.m2i :+ st"$owner.toImmutable$mname($name)",
      tpeInits = context.tpeInits :+ ((tpe, st"$tpe()")),
      wellFormed = context.wellFormed ++ wf,
      decoding = context.decoding :+
        st"""val ${name}Sz = sizeOf$mname($deps)
            |if (${name}Sz >= 0) {
            |  $name = MSZ.create(${name}Sz, $elementTpe)
            |  for (i <- 0 until ${name}Sz) {
            |    $decode
            |  }
            |} else {
            |  context.signalError(ERROR_${owner}_$name)
            |}""",
      encoding = context.encoding :+
        st"""val ${name}Sz = sizeOf$mname($deps)
            |if (${name}Sz >= 0) {
            |  for (i <- 0 until ${name}Sz) {
            |    $encode
            |  }
            |} else {
            |  context.signalError(ERROR_${owner}_$name)
            |}""",
      members = context.members :+
        st"""def sizeOf$mname($pName: $pType): Z = {
            |  val r: Z = {
            |    $pBody
            |  }
            |  return r
            |}""",
      omembers = context.omembers :+
        st"""def toMutable$mname(s: $itpe): $tpe = {
            |  var r = $tpe()
            |  for (e <- s) {
            |    r = r :+ e${if (element.isScalar) "" else ".toMutable"}
            |  }
            |  return r
            |}
            |
            |def toImmutable$mname(s: $tpe): $itpe = {
            |  var r = $itpe()
            |  for (e <- s) {
            |    r = r :+ e${if (element.isScalar) "" else ".toImmutable"}
            |  }
            |  return r
            |}""",
      fieldMap = elementContext.fieldMap,
      nextFound = 0,
      seenSpecs = elementContext.seenSpecs
    )
  }

  def genSpecPredRepeat(first: B,
                        context: Context,
                        name: String,
                        maxElements: Z,
                        isWhile: B,
                        preds: ISZ[Spec.Pred],
                        element: Spec,
                        reporter: Reporter): Context = {
    val normElement: Spec =
      if (element.isScalar || element.isInstanceOf[Spec.Composite]) element
      else Spec.Concat(ops.StringOps(element.name).firstToUpper, ISZ(element))
    val mname = ops.StringOps(name).firstToUpper
    val owner = context.owner
    var elementContext = context(path = context.path :+ name, main = context.main, owner = st"${owner}_$name".render,
      supr = "Runtime.Composite", isupers = traits, fields = ISZ(), ifields = ISZ(), i2m = ISZ(), m2i = ISZ(),
      tpeInits = ISZ(), wellFormed = ISZ(), decoding = ISZ(),
      encoding = ISZ(), members = ISZ(), omembers = ISZ())
    var predSTs = ISZ[ST]()
    for (pred <- preds) {
      val p = genSpecPred(elementContext, pred, reporter)
      predSTs = predSTs :+ p._2
      elementContext = p._1
    }
    elementContext = genSpec(elementContext, normElement, reporter)
    val (tpe, itpe): (ST, ST) =
      if (element.isScalar) (st"MSZ[${elementContext.tpeInits(0)._1}]", st"ISZ[${elementContext.tpeInits(0)._1}]")
      else (st"MSZ[M${normElement.name}]", st"ISZ[${normElement.name}]")
    var wf = ISZ[ST]()
    if (maxElements >= 0) {
      wf = wf :+
        st"""if ($name.size ${if (predSTs.isEmpty) "!=" else ">"} $maxElements) {
            |  return ERROR_${owner}_$name
            |}"""
    }
    val whileCondOpt: Option[ST] =
      if (maxElements >= 0) Some(st"$name.size < $maxElements")
      else None()
    val matchOpt: Option[ST] =
      if (predSTs.isEmpty) None()
      else Some(st"${if (whileCondOpt.isEmpty) "" else " && "}${if (isWhile) "" else "!"}match$mname(input, context)")
    val elementTpe = elementContext.tpeInits(0)._2
    val (decode, encode): (ST, ST) =
      if (element.isScalar) {
        (
          st"""val ${elementContext.decoding(0)}
              |$name = $name :+ ${element.name}""",
          st"""val ${element.name} = $name(i)
              |${elementContext.encoding(0)}"""
        )
      } else {
        (st"""$name = $name :+ $elementTpe
             |$name($name.size - 1).decode(input, context)""", st"$name(i).encode(output, context)")
      }
    return Context(
      path = context.path,
      errNum = elementContext.errNum + 1,
      imports = elementContext.imports,
      simports = elementContext.simports,
      mainDecl = if (!first) elementContext.mainDecl else elementContext.mainDecl :+ st"val ERROR_${owner}_$name: Z = ${elementContext.errNum}",
      main = elementContext.main,
      owner = context.owner,
      supr = context.supr,
      isupers = context.isupers,
      fields = context.fields :+ st"var $name: $tpe",
      ifields = context.ifields :+ st"val $name: $itpe",
      i2m = context.i2m :+ st"$owner.toMutable$mname($name)",
      m2i = context.m2i :+ st"$owner.toImmutable$mname($name)",
      tpeInits = context.tpeInits :+ ((tpe, st"$tpe()")),
      wellFormed = context.wellFormed ++ wf,
      decoding = context.decoding :+
        st"""$name = MSZ()
            |while ($whileCondOpt$matchOpt) {
            |  $decode
            |}""",
      encoding = context.encoding :+
        st"""for (i <- 0 until $name.size) {
            |  $encode
            |}""",
      members = if (predSTs.isEmpty) context.members else context.members :+
        st"""def match$mname(input: ISZ[B], context: Context): B = {
            |  var ctx = context
            |  var hasError = F
            |  ${(predSTs, "\n")}
            |  return !hasError
            |}""",
      omembers = context.omembers :+
        st"""def toMutable$mname(s: $itpe): $tpe = {
            |  var r = $tpe()
            |  for (e <- s) {
            |    r = r :+ e${if (element.isScalar) "" else ".toMutable"}
            |  }
            |  return r
            |}
            |
            |def toImmutable$mname(s: $tpe): $itpe = {
            |  var r = $itpe()
            |  for (e <- s) {
            |    r = r :+ e${if (element.isScalar) "" else ".toImmutable"}
            |  }
            |  return r
            |}""",
      fieldMap = elementContext.fieldMap,
      nextFound = 0,
      seenSpecs = elementContext.seenSpecs
    )
  }

  def genSpecRaw(context: Context, name: String, maxSize: Z, dependsOn: ISZ[String], reporter: Reporter): Context = {
    val (pName, pType, pBody): (String, String, String) =
      funNameTypeBody(context.path :+ name, dependsOn.size, name,
        if (maxSize > 0) "Spec.BoundedRaw.size" else "Spec.Raw.size", reporter) match {
        case Some((pn, ptpe, pt)) => (pn, ptpe, pt)
        case _ => return context
      }
    val mname = ops.StringOps(name).firstToUpper
    val owner = context.owner
    val deps: ST = if (dependsOn.size == 1) st"${dependsOn(0)}" else st"(${(dependsOn, ", ")})"
    val tpe = st"MSZ[B]"
    val itpe = st"ISZ[B]"
    var wf = ISZ(
      st"""val ${name}Sz = sizeOf$mname($deps)
          |if ($name.size != ${name}Sz) {
          |  return ERROR_${owner}_$name
          |}"""
    )
    if (maxSize >= 0) {
      wf =
        st"""if ($name.size > $maxSize) {
            |  return ERROR_${owner}_$name
            |}""" +: wf
    }
    return Context(
      path = context.path,
      errNum = context.errNum + 1,
      imports = context.imports,
      simports = context.simports,
      mainDecl = context.mainDecl :+ st"val ERROR_${owner}_$name: Z = ${context.errNum}",
      main = context.main,
      owner = context.owner,
      supr = context.supr,
      isupers = context.isupers,
      fields = context.fields :+ st"var $name: $tpe",
      ifields = context.ifields :+ st"val $name: $itpe",
      i2m = context.i2m :+ st"$name.toMS",
      m2i = context.m2i :+ st"$name.toIS",
      tpeInits = context.tpeInits :+ ((tpe, st"$tpe()")),
      wellFormed = context.wellFormed ++ wf,
      decoding = context.decoding :+
        st"""val ${name}Sz = sizeOf$mname($deps)
            |if (${name}Sz >= 0) {
            |  $name = MSZ.create(${name}Sz, F)
            |  Reader.IS.bleRaw(input, context, $name, ${name}Sz)
            |} else {
            |  context.signalError(ERROR_${owner}_$name)
            |}""",
      encoding = context.encoding :+
        st"""val ${name}Sz = sizeOf$mname($deps)
            |if (${name}Sz >= 0) {
            |  Writer.bleRaw(output, context, $name, ${name}Sz)
            |} else {
            |  context.signalError(ERROR_${owner}_$name)
            |}""",
      members = context.members :+
        st"""def sizeOf$mname($pName: $pType): Z = {
            |  val r: Z = {
            |    $pBody
            |  }
            |  return r
            |}""",
      omembers = context.omembers,
      fieldMap = context.fieldMap,
      nextFound = 0,
      seenSpecs = context.seenSpecs
    )
  }

  def genSpecGenUnion(first: B, context: Context, spec: Spec.GenUnionImpl, reporter: Reporter): Context = {
    val name = spec.name
    val subs = spec.subs
    val normSubs: ISZ[Spec] = for (sub <- subs) yield
      if (sub.isInstanceOf[Spec.Composite]) sub else Spec.Concat(ops.StringOps(sub.name).firstToUpper, ISZ(sub))
    val (ctx, fname) = context.fieldName(context.path, spec.asOpt.getOrElse(name))
    var subContext = ctx(path = context.path :+ fname, main = ctx.main, owner = name, supr = s"M$name",
      isupers = ISZ(name), fields = ISZ(), ifields = ISZ(), i2m = ISZ(), m2i = ISZ(), tpeInits = ISZ(), wellFormed = ISZ(),
      decoding = ISZ(), encoding = ISZ(), members = ISZ(), omembers = ISZ())
    for (sub <- normSubs) {
      subContext = genSpec(subContext, sub, reporter)
    }
    val encode: ST =
      if (isBounded) st"def encode(context: Context): Option[ISZ[B]]"
      else st"def encode(buffSize: Z, context: Context): Option[ISZ[B]]"
    val wfName = s"wf${spec.asOpt.getOrElse(name)}"
    return Context(
      path = context.path,
      errNum = subContext.errNum + 1,
      imports = subContext.imports,
      simports = subContext.simports,
      mainDecl = if (!first) subContext.mainDecl else subContext.mainDecl :+ st"val ERROR_$name: Z = ${subContext.errNum}",
      main = if (!first) subContext.main else subContext.main :+
        st"""@datatype trait $name${isupers(context)} {
            |  @strictpure def toMutable: M$name
            |  $encode
            |  def wellFormed: Z
            |}
            |
            |@record trait M$name extends ${context.supr} {
            |  @strictpure def toImmutable: $name
            |}
            |
            |object $name {
            |
            |  val maxSize: Z = z"${spec.maxSizeOpt(enumMaxSize).getOrElse(-1)}"
            |
            |  def empty: M$name = {
            |    return ${normSubs(0).name}.empty
            |  }
            |
            |  def decode(input: ISZ[B], context: Context): Option[$name] = {
            |    val r = empty
            |    r.decode(input, context)
            |    return if (context.hasError) None[$name]() else Some(r.toImmutable)
            |  }
            |
            |  object ChoiceContext {
            |    def empty: ChoiceContext = {
            |      // BEGIN USER CODE: $name.ChoiceContext.empty
            |      ${prevText(s"$name.ChoiceContext.empty", notImplemented)}
            |      // END USER CODE: $name.ChoiceContext.empty
            |    }
            |  }
            |
            |  @record class ChoiceContext(
            |     // BEGIN USER CODE: $name.ChoiceContext
            |     ${prevText(s"$name.ChoiceContext", "")}
            |     // END USER CODE: $name.ChoiceContext
            |  )
            |
            |  @enum object Choice {
            |     ${(for (sub <- normSubs) yield st"'${sub.name}", "\n")}
            |     'Error
            |  }
            |
            |  def choose(input: ISZ[B], context: Context, choiceContext: $name.ChoiceContext): Choice.Type = {
            |    // BEGIN USER CODE: $name.choose
            |    ${prevText(s"$name.choose", notImplemented)}
            |    // END USER CODE: $name.choose
            |  }
            |
            |  ${(subContext.omembers, "\n\n")}
            |}""",
      owner = context.owner,
      supr = context.supr,
      isupers = context.isupers,
      fields = context.fields :+ st"var $fname: M$name",
      ifields = context.ifields :+ st"val $fname: $name",
      i2m = context.i2m :+ st"$fname.toMutable",
      m2i = context.m2i :+ st"$fname.toImmutable",
      tpeInits = context.tpeInits :+ ((st"M$name", st"${normSubs(0).name}.empty")),
      wellFormed = context.wellFormed :+
        st"""val $wfName = $fname.wellFormed
            |if ($wfName != 0) {
            |  return $wfName
            |}""",
      decoding = context.decoding :+
        st"""val ${fname}ChoiceContext = $name.ChoiceContext.empty
            |// BEGIN USER CODE: $name.ChoiceContext.init
            |${prevText(s"$name.ChoiceContext.init", "")}
            |// END USER CODE: $name.ChoiceContext.init
            |$name.choose(input, context, ${fname}ChoiceContext) match {
            |  ${(for (sub <- normSubs) yield st"case $name.Choice.${sub.name} => $fname = ${sub.name}.empty", "\n")}
            |  case _ => context.signalError(ERROR_$name)
            |}
            |$fname.decode(input, context)""",
      encoding = context.encoding :+ st"$fname.encode(output, context)",
      members = context.members,
      omembers = context.omembers,
      fieldMap = context.fieldMap,
      nextFound = 0,
      seenSpecs = subContext.seenSpecs
    )
  }

  def genSpecGenRepeat(first: B, context: Context, spec: Spec.GenRepeatImpl, reporter: Reporter): Context = {
    val name = spec.name
    val element = spec.element
    val normElement: Spec =
      if (element.isScalar || element.isInstanceOf[Spec.Composite]) element
      else Spec.Concat(ops.StringOps(element.name).firstToUpper, ISZ(element))
    val mname = ops.StringOps(name).firstToUpper
    val owner = context.owner
    var elementContext = context(path = context.path :+ name, main = context.main, owner = st"${owner}_$name".render,
      supr = "Runtime.Composite", isupers = traits, fields = ISZ(), ifields = ISZ(), i2m = ISZ(), m2i = ISZ(),
      tpeInits = ISZ(), wellFormed = ISZ(), decoding = ISZ(), encoding = ISZ(), members = ISZ(), omembers = ISZ())
    elementContext = genSpec(elementContext, normElement, reporter)
    val (tpe, itpe): (ST, ST) =
      if (element.isScalar) (st"MSZ[${elementContext.tpeInits(0)._1}]", st"ISZ[${elementContext.tpeInits(0)._1}]")
      else (st"MSZ[M${normElement.name}]", st"ISZ[${normElement.name}]")
    val maxElements = spec.maxElements
    var wf = ISZ[ST]()
    if (maxElements > 0) {
      wf = wf :+
        st"""if ($name.size > $maxElements) {
            |  return ERROR_${owner}_$name
            |}"""
    }
    val elementTpe = elementContext.tpeInits(0)._2
    val (decode, encode): (ST, ST) =
      if (element.isScalar) {
        (
          st"""val ${elementContext.decoding(0)}
              |$name = $name :+ ${element.name}""",
          st"""val ${element.name} = $name(i)
              |${elementContext.encoding(0)}"""
        )
      } else {
        (st"""val o = $elementTpe
             |o.decode(input, context)
             |$name = $name :+ o""", st"$name(i).encode(output, context)")
      }
    return Context(
      path = context.path,
      errNum = elementContext.errNum + 1,
      imports = elementContext.imports,
      simports = elementContext.simports,
      mainDecl = if (!first) elementContext.mainDecl else elementContext.mainDecl :+ st"val ERROR_${owner}_$name: Z = ${elementContext.errNum}",
      main = if (!first) elementContext.main else elementContext.main :+
        st"""object $owner${mname}Context {
            |  def empty: $owner${mname}Context = {
            |    // BEGIN USER CODE: $owner${mname}Context.empty
            |    ${prevText(s"$owner${mname}Context.empty", if (maxElements > 0) s"return $owner${mname}Context(0)" else notImplemented)}
            |    // END USER CODE: $owner${mname}Context.empty
            |  }
            |}
            |
            |@record class $owner${mname}Context(
            |  // BEGIN USER CODE: $owner${mname}Context
            |  ${prevText(s"$owner${mname}Context", if (maxElements > 0) "var i: Z" else "")}
            |  // END USER CODE: $owner${mname}Context
            |)""",
      owner = context.owner,
      supr = context.supr,
      isupers = context.isupers,
      fields = context.fields :+ st"var $name: $tpe",
      ifields = context.ifields :+ st"val $name: $itpe",
      i2m = context.i2m :+ st"$owner.toMutable$mname($name)",
      m2i = context.m2i :+ st"$owner.toImmutable$mname($name)",
      tpeInits = context.tpeInits :+ ((tpe, st"$tpe()")),
      wellFormed = context.wellFormed ++ wf,
      decoding = context.decoding :+
        st"""$name = MSZ()
            |val ${name}Context = $owner${mname}Context.empty
            |// BEGIN USER CODE: $owner${mname}Context.init
            |${prevText(s"$owner${mname}Context.init", "")}
            |// END USER CODE: $owner${mname}Context.init
            |while (${name}Continue(input, context, ${name}Context)) {
            |  $decode
            |  ${name}Update(input, context, ${name}Context)
            |}""",
      encoding = context.encoding :+
        st"""for (i <- 0 until $name.size) {
            |  $encode
            |}""",
      members = context.members :+
        st"""def ${name}Continue(input: ISZ[B], context: Context, ${name}Context: $owner${mname}Context): B = {
            |  // BEGIN USER CODE: $owner.${name}Continue
            |  ${prevText(s"$owner.${name}Continue", if (maxElements > 0) s"return ${name}Context.i < $maxElements" else notImplemented)}
            |  // END USER CODE: $owner.${name}Continue
            |}
            |
            |def ${name}Update(input: ISZ[B], context: Context, ${name}Context: $owner${mname}Context): Unit = {
            |  // BEGIN USER CODE: $owner.${name}Update
            |  ${prevText(s"$owner.${name}Update", if (maxElements > 0) s"${name}Context.i = ${name}Context.i + 1" else notImplemented)}
            |  // END USER CODE: $owner.${name}Update
            |}""",
      omembers = context.omembers :+
        st"""def toMutable$mname(s: $itpe): $tpe = {
            |  var r = $tpe()
            |  for (e <- s) {
            |    r = r :+ e${if (element.isScalar) "" else ".toMutable"}
            |  }
            |  return r
            |}
            |
            |def toImmutable$mname(s: $tpe): $itpe = {
            |  var r = $itpe()
            |  for (e <- s) {
            |    r = r :+ e${if (element.isScalar) "" else ".toImmutable"}
            |  }
            |  return r
            |}""",
      fieldMap = elementContext.fieldMap,
      nextFound = 0,
      seenSpecs = elementContext.seenSpecs
    )
  }

  def genSpecGenRaw(first: B, context: Context, spec: Spec.GenRawImpl, reporter: Reporter): Context = {
    val name = spec.name
    val mname = ops.StringOps(name).firstToUpper
    val owner = context.owner
    val tpe = st"MSZ[B]"
    val itpe = st"ISZ[B]"
    val maxSize = spec.maxSize
    var wf = ISZ[ST]()
    if (maxSize > 0) {
      wf = wf :+
        st"""if ($name.size > $maxSize) {
            |  return ERROR_${owner}_$name
            |}"""
    }
    return Context(
      path = context.path,
      errNum = context.errNum + 1,
      imports = context.imports,
      simports = context.simports,
      mainDecl = if (!first) context.mainDecl else context.mainDecl :+ st"val ERROR_${owner}_$name: Z = ${context.errNum}",
      main = if (!first) context.main else context.main :+
        st"""object $owner${mname}Context {
            |  def empty: $owner${mname}Context = {
            |    // BEGIN USER CODE: $owner${mname}Context.empty
            |    ${prevText(s"$owner${mname}Context.empty", notImplemented)}
            |    // END USER CODE: $owner${mname}Context.empty
            |  }
            |}
            |
            |@record class $owner${mname}Context(
            |  // BEGIN USER CODE: $owner${mname}Context
            |  ${prevText(s"$owner${mname}Context", "")}
            |  // END USER CODE: $owner${mname}Context
            |)""",
      owner = context.owner,
      supr = context.supr,
      isupers = context.isupers,
      fields = context.fields :+ st"var $name: $tpe",
      ifields = context.ifields :+ st"val $name: $itpe",
      i2m = context.i2m :+ st"$name.toMS",
      m2i = context.m2i :+ st"$name.toIS",
      tpeInits = context.tpeInits :+ ((tpe, st"$tpe()")),
      wellFormed = context.wellFormed ++ wf,
      decoding = context.decoding :+
        st"""$name = MSZ()
            |val ${name}Context = $owner${mname}Context.empty
            |// BEGIN USER CODE: $owner${mname}Context.init
            |${prevText(s"$owner${mname}Context.init", "")}
            |// END USER CODE: $owner${mname}Context.init
            |while (${name}Continue(input, context, ${name}Context)) {
            |  $name = $name :+ Reader.IS.bleB(input, context)
            |  ${name}Update(input, context, ${name}Context)
            |}""",
      encoding = context.encoding :+ st"""Writer.bleRaw(output, context, $name, $name.size)""",
      members = context.members :+
        st"""def ${name}Continue(input: ISZ[B], context: Context, ${name}Context: $owner${mname}Context): B = {
            |  // BEGIN USER CODE: $owner.${name}Continue
            |  ${prevText(s"$owner.${name}Continue", notImplemented)}
            |  // END USER CODE: $owner.${name}Continue
            |}
            |
            |def ${name}Update(input: ISZ[B], context: Context, ${name}Context: $owner${mname}Context): Unit = {
            |  // BEGIN USER CODE: $owner.${name}Update
            |  ${prevText(s"$owner.${name}Update", notImplemented)}
            |  // END USER CODE: $owner.${name}Update
            |}""",
      omembers = context.omembers,
      fieldMap = context.fieldMap,
      nextFound = 0,
      seenSpecs = context.seenSpecs
    )
  }

  def genSpecPads(context: Context, spec: Spec.Pads, reporter: Reporter): Context = {
    val size = spec.size
    val owner = context.owner
    return context(
      decoding = context.decoding :+ st"context.skip(input.size, $size, ERROR_$owner)",
      encoding = context.encoding :+ st"context.skip(output.size, $size, ERROR_$owner)")
  }

  def genSpecPred(context: Context, pred: Spec.Pred, reporter: Reporter): (Context, ST) = {
    pred match {
      case pred: Spec.Pred.Boolean =>
        return (
          context,
          st"""if(!hasError) {
              |  hasError = ${if (pred.value) "!" else ""}Reader.IS.bleB(input, ctx)
              |}"""
        )
      case pred: Spec.Pred.Bits =>
        val size = pred.size
        val ctx: Context = if (size <= 0 || size > 64) {
          reporter.error(None(), kind, st"Expecting Pred.Bits.size between 1 .. 64 in ${(context.path, ".")}, but found $size".render)
          context
        } else {
          context(imports = context.imports(size ~> T))
        }
        val prefix: String = if (size <= 8) "ble" else endianPrefix
        return (
          ctx,
          st"""if (!hasError) {
              |  val temp = Reader.IS.${prefix}U$size(input, ctx)
              |  hasError = !(ctx.errorCode == 0 && temp == u$size"${pred.value}")
              |}"""
        )
      case pred: Spec.Pred.Bytes =>
        val size = pred.value.size
        val values: ISZ[ST] = for (value <- pred.value) yield st"""u8"$value""""
        return (
          context,
          st"""if (!hasError) {
              |  val temp = MSZ.create($size, u8"0")
              |  Reader.IS.${endianPrefix}U8S(input, ctx, temp, $size)
              |  hasError = !(ctx.errorCode == 0 && temp == MSZ(${(values, ", ")}))
              |}"""
        )
      case pred: Spec.Pred.Shorts =>
        val size = pred.value.size
        val values: ISZ[ST] = for (value <- pred.value) yield st"""u16"$value""""
        return (
          context,
          st"""if (!hasError) {
              |  val temp = MSZ.create($size, u16"0")
              |  Reader.IS.${endianPrefix}U16S(input, ctx, temp, $size)
              |  hasError = !(ctx.errorCode == 0 && temp == MSZ(${(values, ", ")}))
              |}"""
        )
      case pred: Spec.Pred.Ints =>
        val size = pred.value.size
        val values: ISZ[ST] = for (value <- pred.value) yield st"""u32"$value""""
        return (
          context,
          st"""if (!hasError) {
              |  val temp = MSZ.create($size, u32"0")
              |  Reader.IS.${endianPrefix}U32S(input, ctx, temp, $size)
              |  hasError = !(ctx.errorCode == 0 && temp == MSZ(${(values, ", ")}))
              |}"""
        )
      case pred: Spec.Pred.Longs =>
        val size = pred.value.size
        val values: ISZ[ST] = for (value <- pred.value) yield st"""u64"$value""""
        return (
          context,
          st"""if (!hasError) {
              |  val temp = MSZ.create($size, u64"0")
              |  Reader.IS.${endianPrefix}U64S(input, ctx, temp, $size)
              |  hasError = !(ctx.errorCode == 0 && temp == MSZ(${(values, ", ")}))
              |}"""
        )
      case pred: Spec.Pred.Floats =>
        val size = pred.value.size
        val values: ISZ[ST] = for (value <- pred.value) yield st"${value}f"
        return (
          context,
          st"""if (!hasError) {
              |  val temp = MSZ.create($size, 0.0f)
              |  Reader.IS.${endianPrefix}F32S(input, ctx, temp, $size)
              |  hasError = !(ctx.errorCode == 0 && temp == MSZ(${(values, ", ")}))
              |}"""
        )
      case pred: Spec.Pred.Doubles =>
        val size = pred.value.size
        val values: ISZ[ST] = for (value <- pred.value) yield st"${value}d"
        return (
          context,
          st"""if (!hasError) {
              |  val temp = MSZ.create($size, 0.0d)
              |  Reader.IS.${endianPrefix}F64S(input, ctx, temp, $size)
              |  hasError = !(ctx.errorCode == 0 && temp == MSZ(${(values, ", ")}))
              |}"""
        )
      case pred: Spec.Pred.Skip =>
        val size = pred.size
        return (
          context,
          st"""if (!hasError) {
              |  ctx.skip(input.size, $size, -1)
              |  hasError = !(ctx.errorCode == 0)
              |}"""
        )
      case pred: Spec.Pred.Between =>
        val size = pred.size
        val ctx: Context = if (size <= 0 || size > 64) {
          reporter.error(None(), kind, st"Expecting Pred.Between.size between 1 .. 64 in ${(context.path, ".")}, but found $size".render)
          context
        } else {
          context(imports = context.imports(size ~> T))
        }
        if (pred.lo >= pred.hi) {
          reporter.error(None(), kind, st"Expecting Pred.Between.lo < Pred.Between.hi in ${(context.path, ".")}, but found ${pred.lo} >= ${pred.hi}".render)
        }
        val prefix: String = if (size <= 8) "ble" else endianPrefix
        return (
          ctx,
          st"""if (!hasError) {
              |  val temp = Reader.IS.${prefix}U$size(input, ctx)
              |  hasError = !(ctx.errorCode == 0 && u$size"${pred.lo}" <= temp && temp <= u$size"${pred.hi}")
              |}"""
        )
      case pred: Spec.Pred.Not =>
        val (ctx, predST) = genSpecPred(context, pred.pred, reporter)
        return (
          ctx,
          st"""if (!hasError) {
              |  $predST
              |  hasError = !hasError
              |}"""
        )
      case pred: Spec.Pred.Or =>
        var ctx = context
        var preds = ISZ[ST]()
        for (pr <- pred.preds) {
          val p = genSpecPred(ctx, pr, reporter)
          ctx = p._1
          preds = preds :+
            st"""if (!found${ctx.freshFound}) {
                |  ${p._2}
                |  found${ctx.freshFound} = !hasError
                |  hasError = F
                |  if (!found) {
                |    ctx = orCtx${ctx.freshFound}
                |  }
                |}"""
        }
        return (
          ctx(nextFound = ctx.nextFound + 1),
          st"""if (!hasError) {
              |  val orCtx${ctx.freshFound} = ctx
              |  var found${ctx.freshFound} = F
              |  ${(preds, "\n")}
              |  hasError = !found${ctx.freshFound}
              |}"""
        )

    }
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
            s"Expecting one parameter for $spec function, but found ${f.params.size}")
          return None()
        }
        if (f.params(0).idOpt.isEmpty) {
          reporter.error(f.posOpt, kind,
            s"Expecting one named parameter for $spec function, but found unnamed")
          return None()
        }
        val tpos = t.posOpt.get
        val fpos = f.posOpt.get
        val epos = f.exp.asStmt.posOpt.get
        return Some((f.params(0).idOpt.get.value, ops.StringOps(text).substring(tpos.offset, tpos.offset + tpos.length),
          reorientLines(ops.StringOps(text).substring(epos.offset, epos.offset + epos.length),
            if (fpos.beginLine == epos.beginLine) fpos.beginColumn - 1 else epos.beginColumn - 1)))
      case _ =>
        eprintln(funs)
        reporter.error(None(), kind, st"Could not find $spec function for ${(path, ".")}".render)
        return None()
    }
  }

  def prevText(name: String, default: String): String = {
    codeSectionMap.get(name) match {
      case Some(t) => return reorientLines(t, -1)
      case _ => return default
    }
  }
}
