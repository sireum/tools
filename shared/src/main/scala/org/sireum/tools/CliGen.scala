// #Sireum
/*
 Copyright (c) 2017-2025, Robby, Kansas State University
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
import org.sireum.cli.CliOpt
import org.sireum.cli.CliOpt._

@record class CliGen(val firstColumnLimit: Z, val secondColumnLimit: Z, val reporter: B) {
  var errorPrefix: String = "eprintln("
  var decls: ISZ[ST] = ISZ()
  var parser: ISZ[ST] = ISZ()
  var enumNames: Set[String] = Set.empty

  def gen(
    preambleOpt: Option[String],
    licenseOpt: Option[String],
    fileUriOpt: Option[String],
    packageNames: ISZ[String],
    name: String,
    config: CliOpt,
    beginMarker: String,
    endMarker: String,
    userCodeOpt: Option[String]
  ): ST = {
    val topName = s"${ops.StringOps(config.name).firstToUpper}TopOption"
    if (reporter) {
      errorPrefix = s"""reporter.error(None(), "$name", """
    }
    config match {
      case config: Group => group(topName, ISZ(), config)
      case config: Tool => tool(topName, ISZ(), config)
    }
    val license: Option[ST] = licenseOpt.map((text: String) =>
      st"""/*
          | $text
          | */
          |""")
    val fileUri: Option[ST] = fileUriOpt.map((text: String) =>
      st"""// This file is auto-generated from $text
          |""")
    val packageName: Option[ST] =
      if (packageNames.nonEmpty) Some(
        st"""package ${(packageNames, ".")}
            |""")
      else None[ST]()
    val r =
      st"""$preambleOpt// #Sireum
          |// @formatter:off
          |
          |$license
          |$fileUri
          |$packageName
          |import org.sireum._
          |
          |object $name {
          |
          |  @datatype trait $topName
          |
          |  @datatype class HelpOption extends $topName
          |
          |  ${(decls, "\n\n")}
          |}
          |
          |import $name._
          |
          |@record class $name(val pathSep: C${if (reporter) ", reporter: message.Reporter" else ""}) {
          |
          |  ${(parser, "\n\n")}
          |
          |  def parseArguments(args: ISZ[String], i: Z): ISZ[String] = {
          |    var r = ISZ[String]()
          |    var j = i
          |    while (j < args.size) {
          |      r = r :+ args(j)
          |      j = j + 1
          |    }
          |    return r
          |  }
          |
          |  def parsePaths(args: ISZ[String], i: Z): Option[ISZ[String]] = {
          |    return tokenize(args, i, "path", pathSep, F)
          |  }
          |
          |  def parsePath(args: ISZ[String], i: Z): Option[Option[String]] = {
          |    if (i >= args.size) {
          |      ${errorPrefix}"Expecting a path, but none found.")
          |    }
          |    return Some(Some(args(i)))
          |  }
          |
          |  def parseStrings(args: ISZ[String], i: Z, sep: C): Option[ISZ[String]] = {
          |    tokenize(args, i, "string", sep, F) match {
          |      case r@Some(_) => return r
          |      case _ => return None()
          |    }
          |  }
          |
          |  def parseString(args: ISZ[String], i: Z): Option[Option[String]] = {
          |    if (i >= args.size) {
          |      ${errorPrefix}"Expecting a string, but none found.")
          |      return None()
          |    }
          |    return Some(Some(args(i)))
          |  }
          |
          |  def parseNums(args: ISZ[String], i: Z, sep: C, minOpt: Option[Z], maxOpt: Option[Z]): Option[ISZ[Z]] = {
          |    tokenize(args, i, "integer", sep, T) match {
          |      case Some(sargs) =>
          |        var r = ISZ[Z]()
          |        for (arg <- sargs) {
          |          parseNumH(F, arg, minOpt, maxOpt)._2 match {
          |            case Some(n) => r = r :+ n
          |            case _ => return None()
          |          }
          |        }
          |        return Some(r)
          |      case _ => return None()
          |    }
          |  }
          |
          |  def tokenize(args: ISZ[String], i: Z, tpe: String, sep: C, removeWhitespace: B): Option[ISZ[String]] = {
          |    if (i >= args.size) {
          |      ${errorPrefix}s"Expecting a sequence of $$tpe separated by '$$sep', but none found.")
          |      return None()
          |    }
          |    val arg = args(i)
          |    return Some(tokenizeH(arg, sep, removeWhitespace))
          |  }
          |
          |  def tokenizeH(arg: String, sep: C, removeWhitespace: B): ISZ[String] = {
          |    val argCis = conversions.String.toCis(arg)
          |    var r = ISZ[String]()
          |    var cis = ISZ[C]()
          |    var j = 0
          |    while (j < argCis.size) {
          |      val c = argCis(j)
          |      if (c == sep) {
          |        r = r :+ conversions.String.fromCis(cis)
          |        cis = ISZ[C]()
          |      } else {
          |        val allowed: B = c match {
          |          case c"\n" => !removeWhitespace
          |          case c" " => !removeWhitespace
          |          case c"\r" => !removeWhitespace
          |          case c"\t" => !removeWhitespace
          |          case _ => T
          |        }
          |        if (allowed) {
          |          cis = cis :+ c
          |        }
          |      }
          |      j = j + 1
          |    }
          |    if (cis.size > 0) {
          |      r = r :+ conversions.String.fromCis(cis)
          |    }
          |    return r
          |  }
          |
          |  def parseNumChoice(args: ISZ[String], i: Z, choices: ISZ[Z]): Option[Z] = {
          |    val set = HashSet.empty[Z] ++ choices
          |    parseNum(args, i, None(), None()) match {
          |      case r@Some(n) =>
          |        if (set.contains(n)) {
          |          return r
          |        } else {
          |          ${errorPrefix}s"Expecting one of the following: $$set, but found $$n.")
          |          return None()
          |        }
          |      case r => return r
          |    }
          |  }
          |
          |  def parseNum(args: ISZ[String], i: Z, minOpt: Option[Z], maxOpt: Option[Z]): Option[Z] = {
          |    if (i >= args.size) {
          |      ${errorPrefix}s"Expecting an integer, but none found.")
          |      return None()
          |    }
          |    return parseNumH(F, args(i), minOpt, maxOpt)._2
          |  }
          |
          |  def parseNumFlag(args: ISZ[String], i: Z, minOpt: Option[Z], maxOpt: Option[Z]): Option[Option[Z]] = {
          |    if (i >= args.size) {
          |      return Some(None())
          |    }
          |    parseNumH(T, args(i), minOpt, maxOpt) match {
          |      case (T, vOpt) => return Some(vOpt)
          |      case _ => return None()
          |    }
          |  }
          |
          |  def parseNumH(optArg: B, arg: String, minOpt: Option[Z], maxOpt: Option[Z]): (B, Option[Z]) = {
          |    Z(arg) match {
          |      case Some(n) =>
          |        minOpt match {
          |          case Some(min) =>
          |            if (n < min) {
          |              ${errorPrefix}s"Expecting an integer at least $$min, but found $$n.")
          |              return (F, None())
          |            }
          |          case _ =>
          |        }
          |        maxOpt match {
          |          case Some(max) =>
          |            if (n > max) {
          |              ${errorPrefix}s"Expecting an integer at most $$max, but found $$n.")
          |              return (F, None())
          |            }
          |          case _ =>
          |        }
          |        return (T, Some(n))
          |      case _ =>
          |        if (!optArg) {
          |          ${errorPrefix}s"Expecting an integer, but found '$$arg'.")
          |          return (F, None())
          |        } else {
          |          return (T, None())
          |       }
          |    }
          |  }
          |
          |  def select(mode: String, args: ISZ[String], i: Z, choices: ISZ[String]): Option[String] = {
          |    val arg = args(i)
          |    var cs = ISZ[String]()
          |    for (c <- choices) {
          |      if (ops.StringOps(c).startsWith(arg)) {
          |        cs = cs :+ c
          |      }
          |    }
          |    cs.size match {
          |      case z"0" =>
          |        ${errorPrefix}s"$$arg is not a mode of $$mode.")
          |        return None()
          |      case z"1" => return Some(cs(0))
          |      case _ =>
          |        ${errorPrefix}
          |          st${tqs}Which one of the following modes did you mean by '$$arg'?
          |              |$${(cs, "\n")}$tqs.render)
          |        return None()
          |    }
          |  }
          |}
          |// @formatter:on
          |
          |$beginMarker
          |$userCodeOpt
          |$endMarker
          |"""
    return r
  }

  def tool(topName: String, path: ISZ[String], c: Tool): Unit = {
    var applyArgs = ISZ[String]("help", "parseArguments(args, j)")
    var params = ISZ[ST](st"val help: String", st"val args: ISZ[String]")
    var vars = ISZ[ST]()
    for (opt <- c.opts) {
      val (t, init) = tpe(path :+ c.command, opt.tpe)
      val p = st"${opt.name}: $t"
      applyArgs = applyArgs :+ opt.name
      params = params :+ st"val $p"
      vars = vars :+ st"var $p = $init"
    }
    for (optg <- c.groups) {
      for (opt <- optg.opts) {
        val (t, init) = tpe(path :+ c.command, opt.tpe)
        val p = st"${opt.name}: $t"
        applyArgs = applyArgs :+ opt.name
        params = params :+ st"val $p"
        vars = vars :+ st"var $p = $init"
      }
    }

    val name = parseName(path, c.command)

    decls = decls :+
      st"""@datatype class ${name}Option(
      |  ${(params, ",\n")}
      |) extends $topName"""

    val optss = (("|Available Options:", c.opts)) +:
      c.groups.map[(String, ISZ[Opt])]((g: OptGroup) => (s"|${g.name} Options:", g.opts))

    var options = ISZ[ST]()
    c.usageDescOpt match {
      case Some(usageDesc) => options = options :+ st"$usageDesc"
      case _ =>
    }
    var first = T
    for (topts <- optss) {

      var triples = ISZ[(String, String, String)]()
      for (opt <- topts._2) {
        val shortKey: String = if (opt.shortKey.isEmpty) "" else s"-${opt.shortKey.get}"
        val longKey: String = s"--${opt.longKey}"
        val desc: String = opt.tpe match {
          case t: Type.Choice =>
            if (t.sep.nonEmpty)
              st"${opt.description} (expects one or more of { ${(t.elements, ", ")} }; default: ${t.elements(0)})".render
            else st"${opt.description} (expects one of { ${(t.elements, ", ")} }; default: ${t.elements(0)})".render
          case _: Type.Flag => opt.description
          case t: Type.Num =>
            val min: ST = t.min match {
              case Some(v) => st"; min is $v"
              case _ => st""
            }
            val max: ST = t.max match {
              case Some(v) => st"; max is $v"
              case _ => st""
            }
            if (t.sep.isEmpty) st"${opt.description} (expects an integer$min$max; default is ${t.default})".render
            else st"${opt.description} (expects an int-list separated by '${t.sep.get}'$min$max)".render
          case t: Type.NumFlag =>
            val min: ST = t.min match {
              case Some(v) => st"; min is $v"
              case _ => st""
            }
            val max: ST = t.max match {
              case Some(v) => st"; max is $v"
              case _ => st""
            }
            st"${opt.description} (accepts an optional integer$min$max; default is ${t.default})".render
          case t: Type.NumChoice => st"${opt.description} (expects one of { ${(t.choices, ", ")} })".render
          case t: Type.Path =>
            if (t.multiple) {
              if (t.default.isEmpty) s"${opt.description} (expects path strings)"
              else s"""${opt.description} (expects path strings; default is "${t.default.get}")"""
            } else {
              if (t.default.isEmpty) s"${opt.description} (expects a path)"
              else s"""${opt.description} (expects a path; default is "${t.default.get}")"""
            }
          case t: Type.Str =>
            if (t.sep.isEmpty)
              if (t.default.isEmpty) s"${opt.description} (expects a string)"
              else s"""${opt.description} (expects a string; default is "${t.default.get}")"""
            else if (t.default.isEmpty) s"""${opt.description} (expects a string separated by "${t.sep.get}")"""
            else s"""${opt.description} (expects a string separated by "${t.sep.get}"; default is "${t.default.get}")"""
        }
        triples = triples :+ ((shortKey, longKey, desc))
      }
      if (first) {
        first = F
        options = options :+
          st"""|
           |${topts._1}
           |${(columnize(triples), "\n")}
           |${columnize(ISZ(("-h", "--help", "Display this information")))}"""
      } else {
        options = options :+
          st"""|
           |${topts._1}
           |${(columnize(triples), "\n")}"""
      }
    }

    var cases = ISZ[ST]()

    def optCase(opt: Opt): Unit = {
      val parse: ST = opt.tpe match {
        case t: Type.Choice =>
          val tname = parseName(path :+ c.command, t.name)
          if (t.sep.nonEmpty) st"parse${tname}s(args, j + 1)"
          else st"parse$tname(args, j + 1)"
        case _: Type.Flag => st"{ j = j - 1; Some(!${opt.name}) }"
        case t: Type.Num =>
          val tMin: ST = t.min match {
            case Some(min) => st"Some($min)"
            case _ => st"None()"
          }
          val tMax: ST = t.max match {
            case Some(max) => st"Some($max)"
            case _ => st"None()"
          }
          t.sep match {
            case Some(sep) => st"""parseNums(args, j + 1, '$sep', $tMin, $tMax)"""
            case _ => st"parseNum(args, j + 1, $tMin, $tMax)"
          }
        case t: Type.NumFlag =>
          val tMin: ST = t.min match {
            case Some(min) => st"Some($min)"
            case _ => st"None()"
          }
          val tMax: ST = t.max match {
            case Some(max) => st"Some($max)"
            case _ => st"None()"
          }
          st"""parseNumFlag(args, j + 1, $tMin, $tMax) match {
              |    case o@Some(None()) => j = j - 1; Some(Some(${t.default}))
              |    case o => o
              |  }"""
        case t: Type.NumChoice => st"""parseNumChoice(args, j + 1, ISZ(z"${(t.choices, "\", z\"")}"))"""
        case t: Type.Path => if (t.multiple) st"parsePaths(args, j + 1)" else st"parsePath(args, j + 1)"
        case t: Type.Str =>
          t.sep match {
            case Some(sep) => st"""parseStrings(args, j + 1, '$sep')"""
            case _ => st"parseString(args, j + 1)"
          }
      }
      val sh: String = if (opt.shortKey.isEmpty) "" else s"""arg == "-${opt.shortKey.get}" || """
      cases = cases :+
        st""" else if (${sh}arg == "--${opt.longKey}") {
        |  val o: Option[${tpe(path :+ c.command, opt.tpe)._1}] = $parse
        |  o match {
        |    case Some(v) => ${opt.name} = v
        |    case _ => return None()
        |  }
        |}"""
    }

    for (opt <- c.opts) {
      optCase(opt)
    }
    for (group <- c.groups) {
      for (opt <- group.opts) {
        optCase(opt)
      }
    }

    val inc: (String, String) = if (cases.nonEmpty) ("var j = i", "j = j + 2") else ("val j = i", "")

    parser = parser :+
      st"""def parse$name(args: ISZ[String], i: Z): Option[$topName] = {
      |  val help =
      |    st$tqs${(tokenizeH(c.header, '\n', F), "\n        |")}
      |        |
      |        |Usage: ${c.usage}
      |        ${(options, "\n")}$tqs.render
      |
      |  ${(vars, "\n")}
      |  ${inc._1}
      |  var isOption = T
      |  while (j < args.size && isOption) {
      |    val arg = args(j)
      |    if (ops.StringOps(arg).first == '-') {
      |      if (args(j) == "-h" || args(j) == "--help") {
      |        println(help)
      |        return Some(HelpOption())
      |      }$cases else {
      |        ${errorPrefix}s"Unrecognized option '$$arg'.")
      |        return None()
      |      }
      |      ${inc._2}
      |    } else {
      |      isOption = F
      |    }
      |  }
      |  return Some(${name}Option(${(applyArgs, ", ")}))
      |}"""
  }

  def choiceEnum(name: String, c: Type.Choice): Unit = {
    if (enumNames.contains(name)) {
      return
    }
    enumNames = enumNames + name
    val elements: ISZ[String] = for (e <- c.elements) yield ops.StringOps(e).firstToUpper
    decls = decls :+
      st"""@enum object $name {
      |  ${(elements.map((e: String) => s"\"$e\""), "\n")}
      |}"""
    val cases: ISZ[String] = for (e <- c.elements)
      yield s"""case "$e" => return Some($name.${ops.StringOps(e).firstToUpper})"""
    parser = parser :+
      st"""def parse${name}H(arg: String): Option[$name.Type] = {
      |  arg.native match {
      |    ${(cases, "\n")}
      |    case s =>
      |      ${errorPrefix}s"Expecting one of the following: { ${(c.elements, ", ")} }, but found '$$s'.")
      |      return None()
      |  }
      |}"""
    parser = parser :+
      st"""def parse$name(args: ISZ[String], i: Z): Option[$name.Type] = {
      |  if (i >= args.size) {
      |    ${errorPrefix}"Expecting one of the following: { ${(c.elements, ", ")} }, but none found.")
      |    return None()
      |  }
      |  val r = parse${name}H(args(i))
      |  return r
      |}"""
    if (c.sep.isEmpty) {
      return
    }
    parser = parser :+
      st"""def parse${name}s(args: ISZ[String], i: Z): Option[ISZ[$name.Type]] = {
      |  val tokensOpt = tokenize(args, i, "$name", '${c.sep.get}', T)
      |  if (tokensOpt.isEmpty) {
      |    return None()
      |  }
      |  var r = ISZ[$name.Type]()
      |  for (token <- tokensOpt.get) {
      |    val e = parse${name}H(token)
      |    e match {
      |      case Some(v) => r = r :+ v
      |      case _ => return None()
      |    }
      |  }
      |  return Some(r)
      |}"""
  }

  @pure def parseName(path: ISZ[String], id: String): ST = {
    return st"${(for (e <- path :+ id) yield ops.StringOps(e).firstToUpper, "")}"
  }

  def group(topName: String, path: ISZ[String], c: Group): Unit = {
    val choices: ISZ[String] = for (sub <- c.subs) yield sub.command
    val choiceCases: ISZ[ST] = for (sub <- c.subs)
      yield st"""case Some(string"${sub.command}") => return parse${parseName(path :+ c.command, sub.command)}(args, i + 1)"""
    val columns: ISZ[(String, String, String)] = for (sub <- c.subs if !sub.unlisted)
      yield (sub.command, "", sub.description)
    val name = parseName(path, c.command)
    parser = parser :+
      st"""def parse$name(args: ISZ[String], i: Z): Option[$topName] = {
      |  if (i >= args.size) {
      |    println(
      |      st$tqs${(tokenizeH(c.header, '\n', F), "\n          |")}
      |          |
      |          |Available modes:
      |          ${(columnize(columns), "\n")}$tqs.render
      |    )
      |    return Some(HelpOption())
      |  }
      |  val opt = select("${c.name}", args, i, ISZ("${(choices, "\", \"")}"))
      |  opt match {
      |    ${(choiceCases, "\n")}
      |    case _ => return None()
      |  }
      |}"""

    for (sub <- c.subs) {
      sub match {
        case sub: Group => group(topName, path :+ c.command, sub)
        case sub: Tool => tool(topName, path :+ c.command, sub)
      }
    }
  }

  @pure def tpe(path: ISZ[String], c: Type): (String, String) = {
    c match {
      case c: Type.Flag => return ("B", c.default.string)
      case c: Type.Num =>
        c.sep match {
          case Some(_) => return ("ISZ[Z]", s"ISZ()")
          case _ => return ("Z", c.default.string)
        }
      case _: Type.NumFlag => return ("Option[Z]", s"None()")
      case c: Type.NumChoice => return ("Z", c.choices(0).string)
      case c: Type.Str =>
        c.sep match {
          case Some(sep) =>
            val defs: ISZ[String] =
              if (c.default.isEmpty) ISZ()
              else for (s <- ops.StringOps(c.default.get).split((char: C) => char == sep)) yield
                s""""${ops.StringOps(s).trim}""""
            return ("ISZ[String]", if (c.default.isEmpty) "ISZ[String]()" else st"""ISZ(${(defs, ", ")})""".render)
          case _ =>
            return ("Option[String]", if (c.default.isEmpty) "None[String]()" else s"""Some("${c.default.get}")""")
        }
      case c: Type.Choice =>
        val name = parseName(path, c.name).render
        choiceEnum(name, c)
        return if (c.sep.nonEmpty) (s"ISZ[$name.Type]", s"ISZ($name.${ops.StringOps(c.elements(0)).firstToUpper})")
        else (s"$name.Type", s"$name.${ops.StringOps(c.elements(0)).firstToUpper}")
      case c: Type.Path =>
        return if (c.multiple)
          ("ISZ[String]", if (c.default.isEmpty) "ISZ[String]()" else s"""ISZ("${c.default.get}")""")
        else ("Option[String]", if (c.default.isEmpty) "None[String]()" else s"""Some("${c.default.get}")""")
    }
  }

  @pure def columnize(args: ISZ[(String, String, String)]): ISZ[String] = {
    var lines = ISZ[String]()

    def line(t: (String, String, String)): Unit = {
      var r: String =
        if (t._1 == "") s"|    ${t._2}"
        else if (t._2 == "") s"|${t._1}   "
        else s"|${t._1}, ${t._2}    "
      if (r.size > firstColumnLimit) {
        lines = lines :+ r
        r = st"|${for (_ <- z"0" until firstColumnLimit) yield " "}".render
      } else {
        r = st"$r${for (_ <- z"0" until (firstColumnLimit - r.size)) yield " "}".render
      }

      for (token <- tokenizeH(t._3, ' ', F)) {
        if (r.size + token.size + 1 > secondColumnLimit) {
          lines = lines :+ r
          r = st"|${for (_ <- z"0" to firstColumnLimit) yield " "}".render
        }
        r = s"$r $token"
      }
      lines = lines :+ r
    }

    for (arg <- args) {
      line(arg)
    }
    return lines
  }

  @pure def tokenizeH(arg: String, sep: C, removeWhitespace: B): ISZ[String] = {
    val argCis = conversions.String.toCis(arg)
    var r = ISZ[String]()
    var cis = ISZ[C]()
    var j = 0
    while (j < argCis.size) {
      val c = argCis(j)
      if (c == sep) {
        r = r :+ conversions.String.fromCis(cis)
        cis = ISZ[C]()
      } else {
        val allowed: B = c match {
          case c"\n" => !removeWhitespace
          case c" " => !removeWhitespace
          case c"\r" => !removeWhitespace
          case c"\t" => !removeWhitespace
          case _ => T
        }
        if (allowed) {
          cis = cis :+ c
        }
      }
      j = j + 1
    }
    if (cis.size > 0) {
      r = r :+ conversions.String.fromCis(cis)
    }
    return r
  }
}
