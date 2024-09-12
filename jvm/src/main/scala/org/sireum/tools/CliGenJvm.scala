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
import org.sireum.cli.CliOpt

object CliGenJvm {
  val beginMarker: String = "// BEGIN USER CODE"
  val endMarker: String = "// END USER CODE"

  def run(
    licenseOpt: Option[Os.Path],
    config: CliOpt,
    src: Os.Path,
    dest: Os.Path,
    packageName: ISZ[String],
    nameOpt: Option[String],
    firstColumnLimit: Z,
    secondColumnLimit: Z,
    useReporter: B
  ): String = {
    val lOpt: Option[String] = licenseOpt match {
      case Some(f) => Some(ops.StringOps(f.read).trim)
      case _ => None()
    }
    val fOpt = Some(src.name)
    val (preambleOpt, userCodeOpt): (Option[String], Option[String]) = if (dest.exists) {
      val destContent = dest.read
      val pOpt: Option[String] = if (dest.ext == "cmd") {
        val destOps = ops.StringOps(destContent)
        val endCmd: String = "::!#*/"
        val i = destOps.stringIndexOf(endCmd)
        if (i > 0) {
          Some(s"${destOps.substring(0, i + endCmd.size)}\n")
        } else {
          Some(
            st"""::/*#! 2> /dev/null                                 #
                |@ 2>/dev/null # 2>nul & echo off & goto BOF         #
                |if [ -z "$${SIREUM_HOME}" ]; then                    #
                |  echo "Please set SIREUM_HOME env var"             #
                |  exit -1                                           #
                |fi                                                  #
                |exec "$${SIREUM_HOME}/bin/sireum slang run" "$$0" "$$@"  #
                |:BOF
                |setlocal
                |if not defined SIREUM_HOME (
                |  echo Please set SIREUM_HOME env var
                |  exit /B -1
                |)
                |"%SIREUM_HOME%\bin\sireum.bat" slang run %0 %*
                |exit /B %errorlevel%
                |::!#*/
                |""".render)
        }
      } else {
        None()
      }
      val reporter = message.Reporter.create
      val m = ops.StringOps(destContent).collectSections("cligen", beginMarker, endMarker, reporter)
      m.size match {
        case z"0" => (pOpt, None())
        case z"1" => (pOpt, Some(m.values(0)))
        case n => halt(s"Expecting at most one user code section, but found $n")
      }
    } else {
      (None(), None())
    }

    var r = CliGen(firstColumnLimit, secondColumnLimit + firstColumnLimit, useReporter)
      .gen(preambleOpt, lOpt, fOpt, packageName, nameOpt.getOrElse("Cli"), config, beginMarker, endMarker, userCodeOpt)
      .render
    if (dest.ext == "cmd") {
      r = ops.StringOps(r).replaceAllLiterally("\n", "\r\n")
    }
    return r
  }
}
