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
import org.sireum.message._
import org.sireum.lang.{ast => AST}

object TransformerGenJvm {
  val messageKind: String = "TransformerGen"

  def run(
    isImmutable: B,
    isReversed: B,
    licenseOpt: Option[Os.Path],
    sources: ISZ[Os.Path],
    nameOpt: Option[String],
    exclude: ISZ[String],
    reporter: Reporter
  ): Option[String] = {
    if (sources.isEmpty) {
      reporter.error(None(), "TransformerGen", "Expecting a program input")
      return None()
    }
    var programs = ISZ[AST.TopUnit.Program]()
    for (src <- sources) {
      val srcText = src.read
      val r = lang.parser.Parser.parseTopUnit[AST.TopUnit](srcText, F, F, Some(src.toUri), reporter)
      r match {
        case Some(p: AST.TopUnit.Program) =>
          programs = programs :+ p
        case _ =>
          reporter.error(None(), "TransformerGen", s"$src is not a Slang program")
          return None()
      }
    }
    val lOpt: Option[String] = licenseOpt match {
      case Some(f) => Some(ops.StringOps(f.read).trim)
      case _ => None[String]()
    }
    return Some(PrePostTransformerGen.gen(isImmutable, isReversed, lOpt, nameOpt,
      for (source <- sources) yield source.name, programs, exclude, reporter).render)
  }
}
