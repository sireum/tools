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
import org.sireum.lang.{ast => AST}
import org.sireum.lang.symbol.TypeInfo
import org.sireum.lang.tipe.TypeHierarchy
import org.sireum.message.{Position, Reporter}

object ObjectPrinterGen {
  val messageKind: String = "ObjectPrinterGen"

  val messagePkgName: ISZ[String] = AST.Typed.sireumName :+ "message"
  val messageName: ISZ[String] = messagePkgName :+ "Message"
  val positionName: ISZ[String] = messagePkgName :+ "Position"
  val docInfoName: ISZ[String] = messagePkgName :+ "DocInfo"

  val basicSet: HashSet[ISZ[String]] = HashSet ++ ISZ(
    AST.Typed.bName,
    AST.Typed.cName,
    AST.Typed.zName,
    AST.Typed.stringName,
    messageName,
    positionName,
    docInfoName
  )

  val basicImportSet: HashSet[ISZ[String]] = HashSet ++ ISZ(
    AST.Typed.f32Name,
    AST.Typed.f64Name,
    AST.Typed.rName,
    AST.Typed.z8.ids,
    AST.Typed.z16.ids,
    AST.Typed.z32.ids,
    AST.Typed.z64.ids,
    AST.Typed.n.ids,
    AST.Typed.n8.ids,
    AST.Typed.n16.ids,
    AST.Typed.n32.ids,
    AST.Typed.n64.ids,
    AST.Typed.s8.ids,
    AST.Typed.s16.ids,
    AST.Typed.s32.ids,
    AST.Typed.s64.ids,
    AST.Typed.u8.ids,
    AST.Typed.u16.ids,
    AST.Typed.u32.ids,
    AST.Typed.u64.ids
  )

  val basicOneSet: HashSet[ISZ[String]] = HashSet ++ ISZ(
    AST.Typed.sireumName :+ "Option",
    AST.Typed.sireumName :+ "MOption",
    AST.Typed.sireumName :+ "Set",
    AST.Typed.sireumName :+ "HashSet",
    AST.Typed.sireumName :+ "HashSSet",
    AST.Typed.sireumName :+ "Stack",
    AST.Typed.sireumName :+ "Bag",
    AST.Typed.sireumName :+ "HashBag",
    AST.Typed.sireumName :+ "HashSBag",
    AST.Typed.sireumName :+ "Poset",
    AST.Typed.sireumName :+ "Graph",
    AST.Typed.sireumName :+ "UnionFind"
  )

  val basicTwoSet: HashSet[ISZ[String]] = HashSet ++ ISZ(
    AST.Typed.isName,
    AST.Typed.msName,
    AST.Typed.sireumName :+ "Either",
    AST.Typed.sireumName :+ "MEither",
    AST.Typed.sireumName :+ "Map",
    AST.Typed.sireumName :+ "HashMap",
    AST.Typed.sireumName :+ "HashSMap",
  )

  val tqs: String = "\"\"\""
}

import ObjectPrinterGen._

@record class ObjectPrinterGen(licenseOpt: Option[String],
                               packageOpt: Option[ISZ[String]],
                               name: String,
                               topClassName: ISZ[String],
                               th: TypeHierarchy) {

  var prints: ISZ[ST] = ISZ()
  var imports: HashSSet[String] = HashSSet.empty

  var typeMap: HashMap[AST.Typed, ST] = HashMap.empty

  def addPrint(p: ST): Unit = {
    prints = prints :+ p
  }

  def addSubZImport(t: AST.Typed.Name): Unit = {
    imports = imports + st"|import ${(t.ids, ".")}._".render
  }

  @memoize def printType(t: AST.Typed): ST = {
    def fingerprint(s: String): ST = {
      val sha3 = crypto.SHA3.init256
      sha3.update(conversions.String.toU8is(s))
      return st"${(ops.ISZOps(sha3.finalise()).slice(0, 4), "")}"
    }

    t match {
      case t: AST.Typed.Name => return st"print${t.ids(t.ids.size - 1)}_${fingerprint(t.string)}"
      case t: AST.Typed.Tuple => return st"printTuple${t.args.size}_${fingerprint(t.string)}"
      case _ => halt(s"Unsupported: $t")
    }
  }

  def gen(reporter: Reporter): ST = {
    val className = AST.Typed.Name(topClassName, ISZ())
    val top = genType(AST.Typed.Name(topClassName, ISZ()), None(), reporter)
    val pOpt: Option[ST] = packageOpt match {
      case Some(v) => Some(st"package ${(v, ".")}")
      case _ => None()
    }
    val r =
      st"""// #Sireum
          |$licenseOpt
          |$pOpt
          |
          |import org.sireum._
          |import org.sireum.ObjPrinter._
          |
          |// This file is auto-generated
          |
          |@msig trait $name extends org.sireum.ObjPrinter {
          |
          |  def packageOpt: Option[String]
          |  def licenseOpt: Option[String]
          |  def name: String
          |
          |  def writePrinter(version: String, o: $className): Unit = {
          |    val pOpt: Option[ST] = packageOpt match {
          |      case Some(v) => Some(st"package $$v")
          |      case _ => None()
          |    }
          |    write(
          |      st$tqs// #Sireum
          |          |$$licenseOpt
          |          |$$pOpt
          |          |import org.sireum._
          |          ${(imports.elements, "\n")}
          |          |
          |          |object $$name {
          |          |
          |          |  @strictpure def printerVersion: String = $${printString(version)}
          |          |
          |          |$tqs)
          |    val oST = $top(o)
          |    write(
          |      st$tqs  @strictpure def printObject: $className = $$oST
          |          |
          |          |}$tqs)
          |  }
          |
          |  ${(prints, "\n\n")}
          |
          |}"""
    return r
  }

  def genType(t: AST.Typed, posOpt: Option[Position], reporter: Reporter): ST = {

    typeMap.get(t) match {
      case Some(v) => return v
      case _ =>
    }

    t match {
      case t: AST.Typed.Name =>
        if (basicSet.contains(t.ids)) {
          t.ids match {
            case ObjectPrinterGen.docInfoName => addSubZImport(AST.Typed.u32)
            case ObjectPrinterGen.positionName => addSubZImport(AST.Typed.u32); addSubZImport(AST.Typed.u64)
            case _ =>
          }

          val r = st"print${t.ids(t.ids.size - 1)}"
          typeMap = typeMap + t ~> r
          return r
        }

        if (basicImportSet.contains(t.ids)) {
          addSubZImport(t)
          val r = st"print${t.ids(t.ids.size - 1)}"
          typeMap = typeMap + t ~> r
          return r
        }

        val r = printType(t)
        typeMap = typeMap + t ~> r

        if (basicOneSet.contains(t.ids)) {
          val e = t.args(0)
          addPrint(
            st"""def $r(o: $t): ST = {
                |  return print${t.ids(t.ids.size - 1)}(st"$e", o, ${genType(e, None(), reporter)} _)
                |}""")
          return r
        }

        if (basicTwoSet.contains(t.ids)) {
          val t1 = t.args(0)
          val t2 = t.args(1)
          addPrint(
            st"""def $r(o: $t): ST = {
                |  return print${t.ids(t.ids.size - 1)}(st"$t1", st"$t2", o, ${genType(t1, None(), reporter)} _, ${genType(t2, None(), reporter)} _)
                |}""")
          return r
        }

        th.typeMap.get(t.ids) match {
          case Some(info) =>
            genInfo(info, reporter)
            return r
          case _ =>
            reporter.error(posOpt, messageKind, st"Could not find type information for $t".render)
            return st""
        }
      case t: AST.Typed.Tuple =>
        val r = printType(t)
        typeMap = typeMap + t ~> r
        val tvs = st"${(for (i <- 1 to t.args.size) yield st"${t.args(i)}", ", ")}"
        val elements: ISZ[ST] = for (i <- 1 to t.args.size) yield st"$${${genType(t.args(i - 1), posOpt, reporter)}(o._$i)}"
        addPrint(
          st"""def $r[$tvs](o: ($tvs)): ST = {
              |  return st"(${(elements, ", ")})"
              |}""")
        return r
      case _ =>
    }

    reporter.error(posOpt, messageKind, st"Unsupported type $t".render)
    return st""
  }

  def genRoot(name: ISZ[String], reporter: Reporter): Unit = {
    var cases = ISZ[ST]()
    for (child <- th.poset.childrenOf(name).elements) {
      val t = AST.Typed.Name(child, ISZ())
      val c = genType(t, None(), reporter)
      cases = cases :+ st"case o: $t => return $c(o)"
    }
    val t = AST.Typed.Name(name, ISZ())
    val r = printType(t)
    addPrint(
      st"""def $r(o: $t): ST = {
          |  o match {
          |    ${(cases, "\n")}
          |  }
          |}""")
  }

  def genClass(info: TypeInfo.Adt, reporter: Reporter): Unit = {
    val t = AST.Typed.Name(info.name, ISZ())
    val r = printType(t)
    var args = ISZ[ST]()
    for (param <- info.ast.params) {
      val id = param.id.value
      val pt = genType(param.tipe.typedOpt.get, param.tipe.posOpt, reporter)
      args = args :+ st"""|  $${$pt(o.$id)}"""
    }
    addPrint(
      st"""def $r(o: $t): ST = {
          |  val f = () => addMethod(st"$t", T,
          |    st$tqs$t(
          |        ${(args, ",\n")})$tqs)
          |  return cache(o, f)
          |}""")
  }

  def genEnum(info: TypeInfo.Enum): Unit = {
    val t = AST.Typed.Name(info.owner :+ "Type", ISZ())
    val r = printType(t)
    var cases = ISZ[ST]()
    for (k <- info.elements.keys) {
      val element = AST.Typed.Name(info.owner :+ k, ISZ())
      cases = cases :+ st"""case $element => return st"$element""""
    }
    addPrint(
      st"""def $r(o: $t): ST = {
          |  o match {
          |    ${(cases, "\n")}
          |  }
          |}""")
  }

  def genInfo(info: TypeInfo, reporter: Reporter): Unit = {
    info match {
      case info: TypeInfo.Adt =>
        if (info.ast.typeParams.nonEmpty) {
          reporter.error(info.ast.id.attr.posOpt, messageKind, st"Unsupported generic type ${(info.name, ".")}".render)
        } else if (info.ast.isRoot) {
          genRoot(info.name, reporter)
        } else {
          genClass(info, reporter)
        }
      case info: TypeInfo.Sig =>
        if (info.ast.typeParams.nonEmpty) {
          reporter.error(info.ast.id.attr.posOpt, messageKind, st"Unsupported generic type ${(info.name, ".")}".render)
          return
        }
        genRoot(info.name, reporter)
      case info: TypeInfo.Enum => genEnum(info)
      case _ => reporter.error(None(), messageKind, st"Unexpected type ${(info.name, ".")}".render)
    }
  }
}
