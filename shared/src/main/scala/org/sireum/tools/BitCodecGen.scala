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

  val funOwners: HashSet[ISZ[String]] = HashSet.empty ++ ISZ(unionName, repeatName, rawName)
  val funName: HashMap[ISZ[String], String] = HashMap.empty ++ ISZ(
    unionName ~> "choice",
    repeatName ~> "size",
    rawName ~> "size"
  )

  def gen(licenseOpt: Option[String],
          filename: String,
          packageNames: ISZ[String],
          name: String,
          text: String,
          config: Spec,
          program: AST.TopUnit.Program,
          prevGen: String,
          reporter: Reporter): String = {
    val bcGen = BitCodecGen(licenseOpt, filename, packageNames, name,
      ops.StringOps(text).replaceAllLiterally("/r/n", "/n"), config, program,
      ops.StringOps(prevGen).replaceAllLiterally("/r/n", "/n"), Reporter.create)
    val r = bcGen.gen()
    reporter.reports(bcGen.reporter.messages)
    return r
  }

  @record class EnumFunCollector(reporter: Reporter) extends AST.MTransformer {
    var enums: HashMap[String, AST.Stmt.Enum] = HashMap.empty
    var funs: HashMap[String, (AST.Exp.Fun, AST.Type)] = HashMap.empty

    override def preExpInvoke(o: AST.Exp.Invoke): AST.MTransformer.PreResult[AST.Exp] = {
      o.attr.resOpt match {
        case Some(res: AST.ResolvedInfo) =>
          res match {
            case m: AST.ResolvedInfo.Method if m.mode == AST.MethodMode.Constructor =>
              val className = m.owner :+ m.id
              if (funOwners.contains(className)) {
                val ownerSimpleName = m.id
                if (o.targs.size != 1) {
                  reporter.error(o.posOpt, kind, s"$ownerSimpleName requires 1 type argument")
                } else {
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
}

import BitCodecGen._

@record class BitCodecGen(licenseOpt: Option[String],
                          filename: String,
                          packageNames: ISZ[String],
                          name: String,
                          text: String,
                          config: Spec,
                          program: AST.TopUnit.Program,
                          prevGen: String,
                          reporter: Reporter) {
  val collector: EnumFunCollector = EnumFunCollector(Reporter.create)
  var userCodeMap: HashMap[String, String] = HashMap.empty

  def gen(): String = {
    collectEnumFun()
    collectUserCode()
    if (reporter.hasIssue) {
      return prevGen
    }
    println("Coming soon ...")

    return prevGen
  }

  def collectEnumFun(): Unit = {
    collector.transformTopUnit(program)
    reporter.reports(collector.reporter.messages)
  }

  def collectUserCode(): Unit = {
    val lines = ops.StringOps(prevGen).split(c => c == '\n')
    val size = lines.size
    var i = 0
    while (i < size) {
      val line = lines(i)
      val lOps = ops.StringOps(ops.StringOps(line).trim)
      if (lOps.startsWith(beginCodeMarker)) {
        val name = ops.StringOps(lOps.substring(beginCodeMarker.size, lOps.size)).trim
        val beginLine = i
        i = i + 1
        var found = F
        var code = ISZ[String]()
        while (i < size && !found) {
          val line2 = lines(i)
          val lOps2 = ops.StringOps(ops.StringOps(line2).trim)
          if (lOps2.startsWith(endCodeMarker)) {
            found = T
            val name2 = ops.StringOps(lOps2.substring(endCodeMarker.size, lOps2.size)).trim
            if (name != name2) {
              reporter.error(None(), kind, s"Mismatch code marker at lines $beginLine and $i ($name != $name2)")
              return
            }
            userCodeMap = userCodeMap + name ~> st"${(code, "\n")}".render
          } else {
            code = code :+ line2
          }
          i = i + 1
        }
        if (!found) {
          reporter.error(None(), kind, s"Unclosed code marker at line $beginLine for $name")
        }
      }
      i = i + 1
    }
  }
}
