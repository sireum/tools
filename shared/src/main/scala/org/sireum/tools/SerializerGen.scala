// #Sireum
/*
 Copyright (c) 2017-2024, Robby, Kansas State University
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
import org.sireum.message._
import org.sireum.ops._
import org.sireum.lang._
import org.sireum.lang.{ast => AST}
import org.sireum.lang.symbol._
import org.sireum.lang.symbol.Resolver._

object SerializerGen {

  @enum object Mode {
    "JSON"
    "MessagePack"
  }

  @datatype trait Template {

    @pure def main(
                    licenseOpt: Option[String],
                    fileUriOpt: Option[String],
                    packageNames: ISZ[String],
                    imports: ISZ[ST],
                    name: Option[String],
                    constants: ISZ[ST],
                    printers: ISZ[ST],
                    parsers: ISZ[ST],
                    fromsTos: ISZ[ST]
                  ): ST

    @pure def from(name: ST, tpe: ST): ST

    @pure def to(name: ST, tpe: ST): ST

    @pure def printRoot(name: ST, tpe: ST, printRootCases: ISZ[ST]): ST

    @pure def printRootCase(name: ST, tpe: ST): ST

    @pure def printObject(name: ST, tpe: ST, printFields: ISZ[ST]): ST

    @pure def printField(fieldName: String, fieldValue: ST): ST

    @pure def printValue(name: ST, fieldName: String, isBuiltIn: B): ST

    @pure def printFun(fieldName: String, size: Z, isPure: B): ST

    @pure def printEnum(name: ST, tpe: ST, printEnumCases: ISZ[ST]): ST

    @pure def printEnumCase(elementName: String, tpe: ST): ST

    @pure def printSubZ(name: ST, tpe: ST): ST

    @pure def printSeqValue(isImmutable:  B, isSimple:  B, indexName: ST, isFieldBuiltIn: B, fieldName: String, tpe: ST): ST

    @pure def printSeq(isImmutable: B, indexName: String, indexString: String): Option[ST]

    @pure def printS(isImmutable: B, isSimple: B, name: ST, isBuiltIn: B, fieldName: String, indexType: String): ST

    @pure def printNameOne(isSimple: B, nameOne: String, name: ST, fieldName: String, isBuiltIn: B): ST

    @pure def printNameTwo(
                            isSimple: B,
                            nameTwo: String,
                            name1: ST,
                            name2: ST,
                            fieldName: String,
                            isBuiltIn1: B,
                            isBuiltIn2: B
                          ): ST

    @pure def parseRoot(name: ST, tpe: ST, childrenTpes: ISZ[ST], parseRootCases: ISZ[ST], defaultName: ST): ST

    @pure def parseRootCase(name: ST, tpe: ST): ST

    @pure def parseObject(name: ST, tpe: ST, parseFields: ISZ[ST], fieldNames: ISZ[String]): ST

    @pure def parseField(fieldName: String, parseValue: ST): ST

    @pure def parseValue(suffixes: ISZ[ST], isBuiltIn: B): ST

    @pure def parseFun(size: Z, isPure: B): ST

    @pure def parseEnum(name: ST, tpe: ST, parseEnumCases: ISZ[ST]): ST

    @pure def parseEnumCase(elementName: String, tpe: ST): ST

    @pure def parseSubZ(name: ST, tpe: ST): ST

    @pure def parseSeqValue(isImmutable: B, indexName: ST, isFieldBuiltIn: B, fieldName: ST): ST

    @pure def parseSeq(isImmutable: B, name: String, tpe: String): ST

    @pure def parseS(isImmutable: B, indexType: String, name: ST, isBuiltIn: B): ST

    @pure def parseNameOne(nameOne: String, name: ST, isBuiltIn: B): ST

    @pure def parseNameTwo(prefix: String, name1: ST, name2: ST, isBuiltIn1: B, isBuiltIn2: B): ST
  }

  @datatype class JsonTemplate extends Template {

    @pure def main(
                    licenseOpt: Option[String],
                    fileUriOpt: Option[String],
                    packageNames: ISZ[String],
                    imports: ISZ[ST],
                    name: Option[String],
                    constants: ISZ[ST],
                    printers: ISZ[ST],
                    parsers: ISZ[ST],
                    fromsTos: ISZ[ST]
                  ): ST = {
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
              |""") else None[ST]()
      val r =
        st"""// #Sireum
            |// @formatter:off
            |
            |$license
            |$fileUri
            |$packageName
            |import org.sireum._
            |import org.sireum.Json.Printer._
            |${(imports, "\n")}
            |
            |object ${name.getOrElse("JSON")} {
            |
            |  object Printer {
            |
            |    ${(printers, "\n\n")}
            |
            |  }
            |
            |  @record class Parser(val input: String) {
            |    val parser: Json.Parser = Json.Parser.create(input)
            |
            |    def errorOpt: Option[Json.ErrorMsg] = {
            |      return parser.errorOpt
            |    }
            |
            |    ${(parsers, "\n\n")}
            |
            |    def eof(): B = {
            |      val r = parser.eof()
            |      return r
            |    }
            |
            |  }
            |
            |  def to[T](s: String, f: Parser => T): Either[T, Json.ErrorMsg] = {
            |    val parser = Parser(s)
            |    val r = f(parser)
            |    parser.eof()
            |    parser.errorOpt match {
            |      case Some(e) => return Either.Right(e)
            |      case _ => return Either.Left(r)
            |    }
            |  }
            |
            |  ${(fromsTos, "\n\n")}
            |
            |}"""
      return r
    }

    @pure def from(name: ST, tpe: ST): ST = {
      val r =
        st"""def from$name(o: $tpe, isCompact: B): String = {
            |  val st = Printer.print$name(o)
            |  if (isCompact) {
            |    return st.renderCompact
            |  } else {
            |    return st.render
            |  }
            |}"""
      return r
    }

    @pure def to(name: ST, tpe: ST): ST = {
      val r =
        st"""def to$name(s: String): Either[$tpe, Json.ErrorMsg] = {
            |  def f$name(parser: Parser): $tpe = {
            |    val r = parser.parse$name()
            |    return r
            |  }
            |  val r = to(s, f$name _)
            |  return r
            |}"""
      return r
    }

    @pure def printRoot(name: ST, tpe: ST, printRootCases: ISZ[ST]): ST = {
      val r =
        st"""@pure def print$name(o: $tpe): ST = {
            |  o match {
            |    ${(printRootCases, "\n")}
            |  }
            |}"""
      return r
    }

    @pure def printRootCase(name: ST, tpe: ST): ST = {
      return st"case o: $tpe => return print$name(o)"
    }

    @pure def printObject(name: ST, tpe: ST, printFields: ISZ[ST]): ST = {
      val fqs = "\"\"\"\""
      val r =
        st"""@pure def print$name(o: $tpe): ST = {
            |  return printObject(ISZ(
            |    ${(printField("type", st"st$fqs$tpe$fqs") +: printFields, ",\n")}
            |  ))
            |}"""
      return r
    }

    @pure def printField(fieldName: String, fieldValue: ST): ST = {
      return st"""("$fieldName", $fieldValue)"""
    }

    @pure def printValue(name: ST, fieldName: String, isBuiltIn: B): ST = {
      return st"print$name(o.$fieldName)"
    }

    @pure def printFun(fieldName: String, size: Z, isPure: B): ST = {
      return st"printString(Json.Fun.print${if (isPure) "Pure" else ""}$size(o.$fieldName))"
    }

    @pure def printEnum(name: ST, tpe: ST, printEnumCases: ISZ[ST]): ST = {
      val r =
        st"""@pure def print${name}Type(o: $tpe.Type): ST = {
            |  val value: String = o match {
            |    ${(printEnumCases, "\n")}
            |  }
            |  return printObject(ISZ(
            |    ("type", printString("$tpe")),
            |    ("value", printString(value))
            |  ))
            |}"""
      return r
    }

    @pure override def printSubZ(name:  ST, tpe:  ST): ST = {
      val r =
        st"""@pure def print$name(o: $tpe): ST = {
            |  return printNumber(o.toZ.string)
            |}"""
      return r
    }

    @pure def printEnumCase(elementName: String, tpe: ST): ST = {
      return st"""case $tpe.$elementName => "$elementName""""
    }

    @pure def printSeqValue(isImmutable:  B, isSimple:  B, indexName: ST, isFieldBuiltIn: B, fieldName: String, tpe: ST): ST = {
      val prefix: String = if (isImmutable) "IS" else "MS"
      return st"print$prefix(${if (isSimple) "T" else "F"}, o.${fieldName}.map(print$tpe _))"
    }

    @pure def printSeq(isImmutable: B, indexName: String, indexString: String): Option[ST] = {
      return None()
    }

    @pure def printS(isImmutable: B, isSimple: B, name: ST, isBuiltIn: B, fieldName: String, indexType: String): ST = {
      val sName: String = if (isImmutable) "IS" else "MS"
      return st"print$sName$indexType(${if (isSimple) "T" else "F"}, o.$fieldName, print$name _)"
    }

    @pure def printNameOne(isSimple: B, nameOne: String, name: ST, fieldName: String, isBuiltIn: B): ST = {
      return st"print$nameOne(${if (isSimple) "T" else "F"}, o.$fieldName, print$name _)"
    }

    @pure def printNameTwo(
                            isSimple: B,
                            nameTwo: String,
                            name1: ST,
                            name2: ST,
                            fieldName: String,
                            isBuiltIn1: B,
                            isBuiltIn2: B
                          ): ST = {
      return st"print$nameTwo(${if (isSimple) "T" else "F"}, o.$fieldName, print$name1 _, print$name2 _)"
    }

    @pure def parseRoot(name: ST, tpe: ST, childrenTpes: ISZ[ST], parseRootCases: ISZ[ST], defaultName: ST): ST = {
      val r =
        st"""def parse$name(): $tpe = {
            |  val t = parser.parseObjectTypes(ISZ("${(childrenTpes, "\", \"")}"))
            |  t.native match {
            |    ${(parseRootCases, "\n")}
            |    case _ => val r = parse${defaultName}T(T); return r
            |  }
            |}"""
      return r
    }

    @pure def parseRootCase(name: ST, tpe: ST): ST = {
      return st"""case "$tpe" => val r = parse${name}T(T); return r"""
    }

    @pure def parseObject(name: ST, tpe: ST, parseFields: ISZ[ST], fieldNames: ISZ[String]): ST = {
      val r =
        st"""def parse$name(): $tpe = {
            |  val r = parse${name}T(F)
            |  return r
            |}
            |
            |def parse${name}T(typeParsed: B): $tpe = {
            |  if (!typeParsed) {
            |    parser.parseObjectType("$tpe")
            |  }
            |  ${(parseFields, "\n")}
            |  return $tpe(${(fieldNames, ", ")})
            |}"""
      return r
    }

    @pure def parseField(fieldName: String, parseValue: ST): ST = {
      val r =
        st"""parser.parseObjectKey("$fieldName")
            |val $fieldName = $parseValue
            |parser.parseObjectNext()"""
      return r
    }

    @pure def parseValue(suffixes: ISZ[ST], isBuiltIn: B): ST = {
      val p: String = if (isBuiltIn) "parser." else ""
      return st"${p}parse$suffixes"
    }

    @pure def parseFun(size: Z, isPure: B): ST = {
      return st"Json.Fun.parse${if (isPure) "Pure" else ""}$size(this.parser, parser.parseString())"
    }

    @pure def parseEnum(name: ST, tpe: ST, parseEnumCases: ISZ[ST]): ST = {
      val r =
        st"""def parse${name}Type(): $tpe.Type = {
            |  val r = parse${name}T(F)
            |  return r
            |}
            |
            |def parse${name}T(typeParsed: B): $tpe.Type = {
            |  if (!typeParsed) {
            |    parser.parseObjectType("$tpe")
            |  }
            |  parser.parseObjectKey("value")
            |  var i = parser.offset
            |  val s = parser.parseString()
            |  parser.parseObjectNext()
            |  $tpe.byName(s) match {
            |    case Some(r) => return r
            |    case _ =>
            |      parser.parseException(i, s"Invalid element name '$$s' for $tpe.")
            |      return $tpe.byOrdinal(0).get
            |  }
            |}"""
      return r
    }

    @pure def parseSubZ(name: ST, tpe: ST): ST = {
      val r =
        st"""def parse$name(): $tpe = {
            |  val i = parser.offset
            |  val s = parser.parseNumber()
            |  $tpe(s) match {
            |    case Some(n) => return n
            |    case _ =>
            |      parser.parseException(i, s"Expected a $tpe, but '$$s' found.")
            |      return $tpe.Min
            |  }
            |}"""
      return r
    }

    @pure def parseSeqValue(isImmutable: B, indexName: ST, isFieldBuiltIn: B, fieldName: ST): ST = {
      val prefix: String = if (isImmutable) "IS" else "MS"
      val p: String = if (isFieldBuiltIn) "parser." else ""
      return st"parse$prefix$indexName(${p}parse$fieldName _)"
    }

    @pure def parseSeq(isImmutable: B, name: String, tpe: String): ST = {
      val prefix: String = if (isImmutable) "IS" else "MS"
      val r = st"""def parse$prefix$name[T](f: () => T): $prefix[$tpe, T] = {
          |  if (!parser.parseArrayBegin()) {
          |    return $prefix()
          |  }
          |  var e = f()
          |  var r = $prefix[$tpe, T](e)
          |  var continue = parser.parseArrayNext()
          |  while (continue) {
          |    e = f()
          |    r = r :+ e
          |    continue = parser.parseArrayNext()
          |  }
          |  return r
          |}"""
      return r
    }

    @pure def parseEnumCase(elementName: String, tpe: ST): ST = {
      return st"""case "$elementName" => return $tpe.$elementName"""
    }

    @pure def parseS(isImmutable: B, indexType: String, name: ST, isBuiltIn: B): ST = {
      val sName: String = if (isImmutable) "IS" else "MS"
      val p: String = if (isBuiltIn) "parser." else ""
      return st"parser.parse$sName$indexType(${p}parse$name _)"
    }

    @pure def parseNameOne(nameOne: String, name: ST, isBuiltIn: B): ST = {
      val p: String = if (isBuiltIn) "parser." else ""
      return st"parser.parse$nameOne(${p}parse$name _)"
    }

    @pure def parseNameTwo(prefix: String, name1: ST, name2: ST, isBuiltIn1: B, isBuiltIn2: B): ST = {
      val p1: String = if (isBuiltIn1) "parser." else ""
      val p2: String = if (isBuiltIn2) "parser." else ""
      return st"parser.parse$prefix(${p1}parse$name1 _, ${p2}parse$name2 _)"
    }
  }

  @datatype class MessagePackTemplate extends Template {

    @pure def main(
                    licenseOpt: Option[String],
                    fileUriOpt: Option[String],
                    packageNames: ISZ[String],
                    imports: ISZ[ST],
                    name: Option[String],
                    constants: ISZ[ST],
                    writers: ISZ[ST],
                    readers: ISZ[ST],
                    fromsTos: ISZ[ST]
                  ): ST = {
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
              |""") else None[ST]()
      val r =
        st"""// #Sireum
            |// @formatter:off
            |
            |$license
            |$fileUri
            |$packageName
            |import org.sireum._
            |${(imports, "\n")}
            |
            |object ${name.getOrElse("MsgPack")} {
            |
            |  object Constants {
            |
            |    ${(constants, "\n\n")}
            |
            |  }
            |
            |  object Writer {
            |
            |    @record class Default(val writer: MessagePack.Writer.Impl) extends Writer
            |
            |  }
            |
            |  @msig trait Writer {
            |
            |    def writer: MessagePack.Writer
            |
            |    ${(writers, "\n\n")}
            |
            |    def result: ISZ[U8] = {
            |      return writer.result
            |    }
            |
            |  }
            |
            |  object Reader {
            |
            |    @record class Default(val reader: MessagePack.Reader.Impl) extends Reader {
            |      def errorOpt: Option[MessagePack.ErrorMsg] = {
            |        return reader.errorOpt
            |      }
            |    }
            |
            |  }
            |
            |  @msig trait Reader {
            |
            |    def reader: MessagePack.Reader
            |
            |    ${(readers, "\n\n")}
            |
            |  }
            |
            |  def to[T](data: ISZ[U8], f: Reader => T): Either[T, MessagePack.ErrorMsg] = {
            |    val rd = Reader.Default(MessagePack.reader(data))
            |    rd.reader.init()
            |    val r = f(rd)
            |    rd.errorOpt match {
            |      case Some(e) => return Either.Right(e)
            |      case _ => return Either.Left(r)
            |    }
            |  }
            |
            |  ${(fromsTos, "\n\n")}
            |
            |}"""
      return r
    }

    @pure def from(name: ST, tpe: ST): ST = {
      val r =
        st"""def from$name(o: $tpe, pooling: B): ISZ[U8] = {
            |  val w = Writer.Default(MessagePack.writer(pooling))
            |  w.write$name(o)
            |  return w.result
            |}"""
      return r
    }

    @pure def to(name: ST, tpe: ST): ST = {
      val r =
        st"""def to$name(data: ISZ[U8]): Either[$tpe, MessagePack.ErrorMsg] = {
            |  def f$name(reader: Reader): $tpe = {
            |    val r = reader.read$name()
            |    return r
            |  }
            |  val r = to(data, f$name _)
            |  return r
            |}"""
      return r
    }

    @pure def printRoot(name: ST, tpe: ST, printRootCases: ISZ[ST]): ST = {
      val r =
        st"""def write$name(o: $tpe): Unit = {
            |  o match {
            |    ${(printRootCases, "\n")}
            |  }
            |}"""
      return r
    }

    @pure def printRootCase(name: ST, tpe: ST): ST = {
      return st"case o: $tpe => write$name(o)"
    }

    @pure def printObject(name: ST, tpe: ST, printFields: ISZ[ST]): ST = {
      val r =
        st"""def write$name(o: $tpe): Unit = {
            |  writer.writeZ(Constants.$name)
            |  ${(printFields, "\n")}
            |}"""
      return r
    }

    @pure def printField(fieldName: String, fieldValue: ST): ST = {
      return fieldValue
    }

    @pure def printValue(name: ST, fieldName: String, isBuiltIn: B): ST = {
      val w: String = if (isBuiltIn) "writer." else ""
      return st"${w}write$name(o.$fieldName)"
    }

    @pure def printFun(fieldName: String, size: Z, isPure: B): ST = {
      return st"writer.writeISZ(MessagePack.Fun.write${if (isPure) "Pure" else ""}$size(o.$fieldName), writer.writeU8 _)"
    }

    @pure def printEnum(name: ST, tpe: ST, printEnumCases: ISZ[ST]): ST = {
      val r =
        st"""def write${name}Type(o: $tpe.Type): Unit = {
            |  writer.writeZ(o.ordinal)
            |}"""
      return r
    }

    @pure def printEnumCase(elementName: String, tpe: ST): ST = {
      return st""
    }

    @pure override def printSubZ(name:  ST, tpe:  ST): ST = {
      val r =
        st"""def write${name}(o: $tpe): Unit = {
            |  writer.writeZ(o.toZ)
            |}"""
      return r
    }

    @pure def printSeqValue(isImmutable:  B, isSimple:  B, indexName: ST, isFieldBuiltIn: B, fieldName: String, tpe: ST): ST = {
      val prefix: String = if(isImmutable) "IS" else "MS"
      val w: String = if (isFieldBuiltIn) "writer." else ""
      return st"write$prefix$indexName(o.$fieldName, ${w}write$tpe _)"
    }

    @pure def printSeq(isImmutable: B, indexName: String, indexString: String): Option[ST] = {
      val prefix: String = if(isImmutable) "IS" else "MS"
      val r =
        st"""def write$prefix$indexName[E](s: $prefix[$indexString, E], f: E => Unit) : Unit = {
            |  writer.writeArrayHeader(s.size)
            |  for (e <- s) {
            |    f(e)
            |  }
            |}"""
      return Some(r)
    }

    @pure def printS(isImmutable: B, isSimple: B, name: ST, isBuiltIn: B, fieldName: String, indexType: String): ST = {
      val sName: String = if (isImmutable) "IS" else "MS"
      val w: String = if (isBuiltIn) "writer." else ""
      return st"writer.write$sName$indexType(o.$fieldName, ${w}write$name _)"
    }

    @pure def printNameOne(isSimple: B, nameOne: String, name: ST, fieldName: String, isBuiltIn: B): ST = {
      val w: String = if (isBuiltIn) "writer." else ""
      return st"writer.write$nameOne(o.$fieldName, ${w}write$name _)"
    }

    @pure def printNameTwo(
                            isSImple: B,
                            nameTwo: String,
                            name1: ST,
                            name2: ST,
                            fieldName: String,
                            isBuiltIn1: B,
                            isBuiltIn2: B
                          ): ST = {
      val w1: String = if (isBuiltIn1) "writer." else ""
      val w2: String = if (isBuiltIn2) "writer." else ""
      return st"writer.write$nameTwo(o.$fieldName, ${w1}write$name1 _, ${w2}write$name2 _)"
    }

    @pure def parseRoot(name: ST, tpe: ST, childrenTpes: ISZ[ST], parseRootCases: ISZ[ST], defaultName: ST): ST = {
      val r =
        st"""def read$name(): $tpe = {
            |  val i = reader.curr
            |  val t = reader.readZ()
            |  t match {
            |    ${(parseRootCases, "\n")}
            |    case _ =>
            |      reader.error(i, s"$$t is not a valid type of $tpe.")
            |      val r = read${defaultName}T(T)
            |      return r
            |  }
            |}"""
      return r
    }

    @pure def parseRootCase(name: ST, tpe: ST): ST = {
      return st"""case Constants.$name => val r = read${name}T(T); return r"""
    }

    @pure def parseObject(name: ST, tpe: ST, parseFields: ISZ[ST], fieldNames: ISZ[String]): ST = {
      val r =
        st"""def read$name(): $tpe = {
            |  val r = read${name}T(F)
            |  return r
            |}
            |
            |def read${name}T(typeParsed: B): $tpe = {
            |  if (!typeParsed) {
            |    reader.expectZ(Constants.$name)
            |  }
            |  ${(parseFields, "\n")}
            |  return $tpe(${(fieldNames, ", ")})
            |}"""
      return r
    }

    @pure def parseField(fieldName: String, parseValue: ST): ST = {
      return st"""val $fieldName = $parseValue"""
    }

    @pure def parseValue(suffixes: ISZ[ST], isBuiltIn: B): ST = {
      val p: String = if (isBuiltIn) "reader." else ""
      return st"${p}read$suffixes"
    }

    @pure def parseFun(size: Z, isPure: B): ST = {
      return st"MessagePack.Fun.parse${if (isPure) "Pure" else ""}$size(this.reader, reader.readISZ(reader.readU8 _))"
    }

    @pure def parseEnum(name: ST, tpe: ST, parseEnumCases: ISZ[ST]): ST = {
      val r =
        st"""def read${name}Type(): $tpe.Type = {
            |  val r = reader.readZ()
            |  return $tpe.byOrdinal(r).get
            |}"""
      return r
    }

    @pure def parseEnumCase(elementName: String, tpe: ST): ST = {
      return st""
    }

    @pure override def parseSubZ(name:  ST, tpe:  ST): ST = {
      val r =
        st"""def read$name(): $tpe = {
            |  val n = reader.readZ()
            |  return $tpe.fromZ(n)
            |}"""
      return r
    }

    @pure override def parseSeqValue(isImmutable:  B, indexName:  ST, isFieldBuiltIn: B, fieldName:  ST): ST = {
      val prefix: String = if (isImmutable) "IS" else "MS"
      val r: String = if (isFieldBuiltIn) "reader." else ""
      return st"read$prefix$indexName(${r}read$fieldName _)"
    }

    @pure override def parseSeq(isImmutable:  B, name:  String, tpe:  String): ST = {
      val prefix: String = if (isImmutable) "IS" else "MS"
      val r=
        st"""def read$prefix$name[E](f: () => E): $prefix[$tpe, E] = {
            |  val size = reader.readArrayHeader()
            |  var r = $prefix[$tpe, E]()
            |  var i = 0
            |  while (i < size) {
            |    val o = f()
            |    r = r :+ o
            |    i = i + 1
            |  }
            |  return r
            |}"""
      return r
    }

    @pure def parseS(isImmutable: B, indexType: String, name: ST, isBuiltIn: B): ST = {
      val sName: String = if (isImmutable) "IS" else "MS"
      val p: String = if (isBuiltIn) "reader." else ""
      return st"reader.read$sName$indexType(${p}read$name _)"
    }

    @pure def parseNameOne(nameOne: String, name: ST, isBuiltIn: B): ST = {
      val p: String = if (isBuiltIn) "reader." else ""
      return st"reader.read$nameOne(${p}read$name _)"
    }

    @pure def parseNameTwo(nameTwo: String, name1: ST, name2: ST, isBuiltIn1: B, isBuiltIn2: B): ST = {
      val p1: String = if (isBuiltIn1) "reader." else ""
      val p2: String = if (isBuiltIn2) "reader." else ""
      return st"reader.read$nameTwo(${p1}read$name1 _, ${p2}read$name2 _)"
    }
  }

  val jsonGenKind: String = "JsonGen"

  def gen(
           mode: Mode.Type,
           sources: ISZ[(Option[String], String)],
           packageName: QName,
           reporter: Reporter,
           licenseOpt: Option[String],
           fileUriOpt: Option[String],
           name: Option[String]
         ): ST = {
    var uris = ISZ[String]()
    var missingUri = F
    for (p <- sources) {
      p._1 match {
        case Some(uri) => uris = uris :+ uri
        case _ => missingUri = T
      }
    }
    val tc = FrontEnd.checkedLibraryReporter._1
    val (rep, programs, globalNameMap, globalTypeMap) =
      FrontEnd.parseProgramAndGloballyResolve(0, for (p <- sources) yield FrontEnd.Input(p._2, p._1), tc.nameMap,
        tc.typeMap)
    if (rep.hasError) {
      reporter.reports(rep.messages)
      return st""
    }
    val pName: ISZ[String] = if (packageName.isEmpty) programs(0).packageName.ids.map(id => id.value) else packageName
    val uriOpts = HashSet.empty[Option[String]] ++ (for (p <- sources) yield p._1)
    var typeMap: TypeMap = HashSMap.empty
    for (ti <- globalTypeMap.values if ti.posOpt.nonEmpty && uriOpts.contains(ti.posOpt.get.uriOpt)) {
      typeMap = typeMap + ti.name ~> ti
    }
    val globalTypes: ISZ[TypeInfo] =
      if (missingUri) sortedGlobalTypes(typeMap) else sortedGlobalTypesUriLt(typeMap, uriLtOrder(uris))
    val g = Gen(mode, globalNameMap, typeMap, globalTypes, pName, Reporter.create)
    val r = g.gen(licenseOpt, fileUriOpt, name)
    reporter.reports(g.reporter.messages)
    return r
  }

  @record class Gen(
                     val mode: Mode.Type,
                     val globalNameMap: NameMap,
                     val globalTypeMap: TypeMap,
                     val sortedGlobalTypes: ISZ[TypeInfo],
                     val packageName: QName,
                     val reporter: Reporter
                   ) {

    val poset: Poset[QName] = typePoset(globalTypeMap, sortedGlobalTypes, reporter)

    var constants: ISZ[ST] = ISZ()
    var parsers: ISZ[ST] = ISZ()
    var printers: ISZ[ST] = ISZ()
    var fromsTos: ISZ[ST] = ISZ()

    var handledParseSequences: Map[String, Set[String]] = Map.empty[String, Set[String]] + "IS" ~> Set.empty + "MS" ~> Set.empty
    var handledPrintSequences: Map[String, Set[String]] = Map.empty[String, Set[String]] + "IS" ~> Set.empty + "MS" ~> Set.empty

    val template: Template = mode match {
      case Mode.JSON => JsonTemplate()
      case Mode.MessagePack => MessagePackTemplate()
    }

    def gen(licenseOpt: Option[String], fileUriOpt: Option[String], name: Option[String]): ST = {
      for (ti <- sortedGlobalTypes) {
        ti match {
          case ti: TypeInfo.Adt => genAdt(ti)
          case ti: TypeInfo.Sig => genRoot(ti.name)
          case ti: TypeInfo.Enum => genEnum(ti)
          case ti: TypeInfo.SubZ => genSubZ(ti)
          case _ =>
        }
      }
      // explicitly import top level package members to resolve potential ambiguous imports arising from
      // Sireum's package object (e.g. org.sireum.hamr.ir.Flow and org.sireum.contract.Flow)
      val packageMembers = ops.ISZOps(sortedGlobalTypes).filter((p: TypeInfo) =>
        packageName.size + 1 == p.name.size && packageName == ops.ISZOps(p.name).dropRight(1))
      val imports: ISZ[ST] = for(t <- packageMembers) yield st"import ${(t.name, ".")}"
      return template.main(licenseOpt, fileUriOpt, packageName, imports, name, constants, printers, parsers, fromsTos)
    }

    def genEnum(ti: TypeInfo.Enum): Unit = {
      val enumTypeString = typeNameString(packageName, ti.owner)
      val enumTypeName = typeName(packageName, ti.owner)
      printers = printers :+
        template.printEnum(
          enumTypeName,
          enumTypeString,
          for (e <- ti.elements.keys)
            yield template.printEnumCase(e, enumTypeString)
        )
      parsers = parsers :+
        template.parseEnum(
          enumTypeName,
          enumTypeString,
          for (e <- ti.elements.keys)
            yield template.parseEnumCase(e, enumTypeString)
        )
    }

    def genAdt(ti: TypeInfo.Adt): Unit = {
      if (ti.ast.isRoot) {
        genRoot(ti.name)
      } else {
        val adTypeString = typeNameString(packageName, ti.name)
        val adTypeName = typeName(packageName, ti.name)
        var fieldNames = ISZ[String]()
        var printFields = ISZ[ST]()
        var parseFields = ISZ[ST]()
        for (param <- ti.ast.params) {
          param.tipe match {
            case tipe: AST.Type.Named =>
              val fieldName = param.id.value
              fieldNames = fieldNames :+ fieldName
              printFields = printFields :+ printField(ti, fieldName, tipe)
              parseFields = parseFields :+ parseField(ti, fieldName, tipe)
            case tipe: AST.Type.Fun if !tipe.isByName =>
              val fieldName = param.id.value
              fieldNames = fieldNames :+ fieldName
              printFields = printFields :+ printFunField(ti, fieldName, tipe)
              parseFields = parseFields :+ parseFunField(ti, fieldName, tipe)
            case _ =>
              reporter.error(
                param.id.attr.posOpt,
                jsonGenKind,
                s"Only named or fun types are supported for @datatype/@record fields."
              )
          }
        }
        if (mode == Mode.MessagePack) {
          constants = constants :+ st"""val $adTypeName: Z = ${constants.size - 32}"""
        }
        printers = printers :+ template.printObject(adTypeName, adTypeString, printFields)
        parsers = parsers :+ template.parseObject(adTypeName, adTypeString, parseFields, fieldNames)
        fromsTos = fromsTos :+ template.from(adTypeName, adTypeString) :+ template.to(adTypeName, adTypeString)
      }
    }

    def genRoot(name: QName): Unit = {
      val rootTypeString = typeNameString(packageName, name)
      val rootTypeName = typeName(packageName, name)
      var rootPrintCases = ISZ[ST]()
      var rootParseCases = ISZ[ST]()
      var childrenTypeStrings = ISZ[ST]()
      val descendants = poset.descendantsOf(name).elements
      val sortedDescendants: ISZ[TypeInfo] = {
        var r = ISZ[TypeInfo]()
        for (d <- descendants) {
          globalTypeMap.get(d) match {
            case Some(info) => r = r :+ info
            case _ =>
          }
        }
        ISZOps(r).sortWith(ltTypeInfo(uriLt _))
      }
      var firstChildTypeName = st"?"
      for (child <- sortedDescendants) {
        child match {
          case childTI: TypeInfo.Adt if !childTI.ast.isRoot =>
            val childIds = childTI.name
            val childTypeString = typeNameString(packageName, childIds)
            val childTypeName = typeName(packageName, childIds)
            firstChildTypeName = childTypeName
            childrenTypeStrings = childrenTypeStrings :+ childTypeString
            rootPrintCases = rootPrintCases :+ template.printRootCase(childTypeName, childTypeString)
            rootParseCases = rootParseCases :+ template.parseRootCase(childTypeName, childTypeString)
          case _ =>
        }
      }
      printers = printers :+ template.printRoot(rootTypeName, rootTypeString, rootPrintCases)
      parsers = parsers :+ template
        .parseRoot(rootTypeName, rootTypeString, childrenTypeStrings, rootParseCases, firstChildTypeName)
      fromsTos = fromsTos :+ template.from(rootTypeName, rootTypeString) :+ template.to(rootTypeName, rootTypeString)
    }

    def genSubZ(ti: TypeInfo.SubZ): Unit = {
      val subZString = typeNameString(packageName, ti.name)
      val subZName = typeName(packageName, ti.name)
      printers = printers :+ template.printSubZ(subZName, subZString)
      parsers = parsers :+ template.parseSubZ(subZName, subZString)
    }

    def printField(ti: TypeInfo.Adt, fieldName: String, tipe: AST.Type.Named): ST = {
      val v = printValue(ti, fieldName, tipe)
      return template.printField(fieldName, v)
    }

    def printFunField(ti: TypeInfo.Adt, fieldName: String, tipe: AST.Type.Fun): ST = {
      val v = printFunValue(ti, fieldName, tipe)
      return template.printField(fieldName, v)
    }

    def printFunValue(ti: TypeInfo.Adt, fieldName: String, tipe: AST.Type.Fun): ST = {
      return template.printFun(fieldName, tipe.args.size, tipe.isPure)
    }

    def printValue(ti: TypeInfo.Adt, fieldName: String, tipe: AST.Type.Named): ST = {
      val sOpt = s(ti, tipe)
      sOpt match {
        case Some((isImmutable, indexType, (isBuiltIn, isSimple, elementName))) =>
          return template.printS(isImmutable, isSimple, elementName, isBuiltIn, fieldName, indexType)
        case _ =>
      }
      val optOpt = nameOne(ti, tipe)
      optOpt match {
        case Some((isImmutable, (isBuiltIn, isSimple, elementName))) =>
          return template.printNameOne(isSimple, isImmutable, elementName, fieldName, isBuiltIn)
        case _ =>
      }
      val mapOpt = nameTwo(ti, tipe)
      mapOpt match {
        case Some((prefix, (isBuiltIn1, isSimple1, e1), (isBuiltIn2, isSimple2, e2))) =>
          if (prefix == "IS" || prefix == "MS") {
            val namedIndex = tipe.typeArgs(0).asInstanceOf[AST.Type.Named]
            ti.scope.resolveType(globalTypeMap, AST.Util.ids2strings(namedIndex.name.ids)) match {
              case Some(ti2) =>
                val indexTypeString = typeNameString(packageName, ti2.name)
                printSeqHelper(prefix, e1.render, indexTypeString.render)
              case _ =>
            }
            return template.printSeqValue(prefix == "IS", isSimple2, e1, isBuiltIn2, fieldName, e2)
          } else {
            return template.printNameTwo(isSimple1 && isSimple2, prefix, e1, e2, fieldName, isBuiltIn1, isBuiltIn2)
          }
        case _ =>
      }
      val t = basicOrTypeName(ti, tipe)
      return template.printValue(t._3, fieldName, t._1)
    }

    def parseField(ti: TypeInfo.Adt, fieldName: String, tipe: AST.Type.Named): ST = {
      val v = parseValue(ti, tipe)
      return template.parseField(fieldName, v)
    }

    def parseFunField(ti: TypeInfo.Adt, fieldName: String, tipe: AST.Type.Fun): ST = {
      val v = parseFunValue(ti, tipe)
      return template.parseField(fieldName, v)
    }

    def parseFunValue(ti: TypeInfo.Adt, tipe: AST.Type.Fun): ST = {
      return template.parseFun(tipe.args.size, tipe.isPure)
    }

    def parseValue(ti: TypeInfo.Adt, tipe: AST.Type.Named): ST = {
      val sOpt = s(ti, tipe)
      sOpt match {
        case Some((isImmutable, indexType, (isBuiltIn, _, elementName))) =>
          return template.parseS(isImmutable, indexType, elementName, isBuiltIn)
        case _ =>
      }
      val optOpt = nameOne(ti, tipe)
      optOpt match {
        case Some((isImmutable, (isBuiltIn, _, elementName))) =>
          return template.parseNameOne(isImmutable, elementName, isBuiltIn)
        case _ =>
      }
      val mapOpt = nameTwo(ti, tipe)
      mapOpt match {
        case Some((prefix, (isBuiltIn1, _, indexType), (isBuiltIn2, _, e2))) =>
          if (prefix == "IS" || prefix == "MS") {
            val namedIndex = tipe.typeArgs(0).asInstanceOf[AST.Type.Named]
            ti.scope.resolveType(globalTypeMap, AST.Util.ids2strings(namedIndex.name.ids)) match {
              case Some(ti2) =>
                val indexTypeString = typeNameString(packageName, ti2.name)
                parseSeqHelper(prefix, indexType.render, indexTypeString.render)
              case _ =>
            }
            return template.parseSeqValue(prefix == "IS", indexType, isBuiltIn2, e2)
          } else {
            return template.parseNameTwo(prefix, indexType, e2, isBuiltIn1, isBuiltIn2)
          }
        case _ =>
      }
      val t = basicOrTypeName(ti, tipe)
      return template.parseValue(ISZ(t._3, st"()"), t._1)
    }

    def printSeqHelper(sequenceTypeName: String, indexType: String, indexTypeString: String): Unit = {
      val handledIndices = handledPrintSequences.get(sequenceTypeName).get
      if (!handledIndices.contains(indexType)) {
        template.printSeq(sequenceTypeName == "IS", indexType, indexTypeString) match {
          case Some(s) => printers = printers :+ s
          case _ =>
        }
        handledPrintSequences = handledPrintSequences + sequenceTypeName ~> (handledIndices + indexType)
      }
    }

    def parseSeqHelper(sequenceTypeName: String, indexType: String, indexTypeString: String): Unit = {
      val handledIndices = handledParseSequences.get(sequenceTypeName).get
      if (!handledIndices.contains(indexType)) {
        parsers = parsers :+ template.parseSeq(sequenceTypeName == "IS", indexType, indexTypeString)
        handledParseSequences = handledParseSequences + sequenceTypeName ~> (handledIndices + indexType)
      }
    }

    def nameTwo(ti: TypeInfo.Adt, tipe: AST.Type.Named): Option[(String, (B, B, ST), (B, B, ST))] = {
      if (!(tipe.name.ids.size == 1 && tipe.typeArgs.size == 2)) {
        return None()
      }
      val name = tipe.name.ids(0).value
      val btn0 = basicOrTypeName(ti, tipe.typeArgs(0))
      val btn1 = basicOrTypeName(ti, tipe.typeArgs(1))
      name.native match {
        case "Either" =>
        case "Map" =>
        case "HashMap" =>
        case "HashSMap" =>
        case "Graph" =>
        case "IS" =>
        case "MS" =>
      }
      return Some((name, btn0, btn1))
    }

    def nameOne(ti: TypeInfo.Adt, tipe: AST.Type.Named): Option[(String, (B, B, ST))] = {
      if (!(tipe.name.ids.size == 1 && tipe.typeArgs.size == 1)) {
        return None()
      }
      val name = tipe.name.ids(0).value
      val btn = basicOrTypeName(ti, tipe.typeArgs(0))
      name.native match {
        case "Option" =>
        case "MOption" =>
        case "Set" =>
        case "HashSet" =>
        case "HashSSet" =>
        case "Poset" =>
        case "Stack" =>
        case "Bag" =>
        case "HashBag" =>
        case "UnionFind" =>
      }
      return Some((name, btn))
    }

    def s(ti: TypeInfo.Adt, tipe: AST.Type.Named): Option[(B, String, (B, B, ST))] = {
      if (!(tipe.name.ids.size == 1 && (tipe.typeArgs.size == 1 || tipe.typeArgs.size == 2))) {
        return None()
      }
      val name = tipe.name.ids(0).value
      if (name == "IS" || name == "MS") {
        val isImmutable = name == "IS"
        val et = tipe.typeArgs(1)
        val btn = basicOrTypeName(ti, et)
        (basic(tipe.typeArgs(0)), basic(et)) match {
          case (Some((_, it)), _) => return Some((isImmutable, it, btn))
          case _ => return None()
        }
      }
      val btn = basicOrTypeName(ti, tipe.typeArgs(0))
      name.native match {
        case "ISZ" => return Some((T, "Z", btn))
        case "MSZ" => return Some((F, "Z", btn))
        case _ => return None()
      }
    }

    def basicOrTypeName(ti: TypeInfo.Adt, tipe: AST.Type): (B /* isBuiltIn */ , B /* isSimple */ , ST) = {
      basic(tipe) match {
        case Some((simple, typeName)) => return (T, simple, st"$typeName")
        case _ =>
          tipe match {
            case tipe: AST.Type.Named =>
              ti.scope.resolveType(globalTypeMap, AST.Util.ids2strings(tipe.name.ids)) match {
                case Some(ti2) => return (F, F, typeName(packageName, ti2.name))
                case _ =>
                  reporter.error(
                    tipe.posOpt,
                    jsonGenKind,
                    st"Could not find ${(AST.Util.ids2strings(tipe.name.ids), ".")}.".render
                  )
                  return (F, F, st"")
              }
            case _ =>
              reporter.error(tipe.posOpt, jsonGenKind, s"Only named types are supported for @datatype/@record fields.")
              return (F, F, st"")
          }

      }
    }

    @pure def basic(tipe: AST.Type): Option[(B, String)] = {
      tipe match {
        case tipe: AST.Type.Named =>
          val ids = tipe.name.ids
          if (!(tipe.typeArgs.isEmpty || ids.size == 1 || (ids.size == 3 && ids(0).value == "org" && ids(1).value == "sireum"))) {
            return None()
          }
          val r = tipe.name.ids(tipe.name.ids.size - 1).value
          var isSimple = T
          r.native match {
            case "B" =>
            case "C" =>
            case "Z" =>
            case "Z8" =>
            case "Z16" =>
            case "Z32" =>
            case "Z64" =>
            case "N" =>
            case "N8" =>
            case "N16" =>
            case "N32" =>
            case "N64" =>
            case "S8" =>
            case "S16" =>
            case "S32" =>
            case "S64" =>
            case "U8" =>
            case "U16" =>
            case "U32" =>
            case "U64" =>
            case "F32" =>
            case "F64" =>
            case "R" =>
            case "String" =>
            case "Message" => isSimple = F
            case "Position" => isSimple = F
            case "FlatPos" => isSimple = F
            case "PosInfo" => isSimple = F
            case "DocInfo" => isSimple = F
            case _ => return None()
          }
          return Some((isSimple, r))
        case _ => return None()
      }
    }
  }

}
