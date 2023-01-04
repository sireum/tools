// #Sireum
/*
 Copyright (c) 2017-2023, Robby, Kansas State University
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

object SerializerGenJvm {
  val messageKind: String = "JsonGen"

  def run(
    mode: SerializerGen.Mode.Type,
    licenseOpt: Option[Os.Path],
    srcs: ISZ[Os.Path],
    packageNameOpt: Option[ISZ[String]],
    nameOpt: Option[String],
    reporter: Reporter
  ): Option[String] = {
    val lOpt: Option[String] = licenseOpt match {
      case Some(f) => Some(ops.StringOps(f.read).trim)
      case _ => None()
    }
    var uris = ISZ[String]()
    var sources = ISZ[(Option[String], String)]()
    for (src <- srcs) {
      val srcText = src.read
      uris = uris :+ src.name
      sources = sources :+ ((Some(src.toUri), srcText))
    }
    val fOpt: Option[String] = Some(st"${(uris, ", ")}".render)
    val packageName: ISZ[String] = packageNameOpt match {
      case Some(pn) => pn
      case _ => ISZ()
    }
    val r = SerializerGen.gen(mode, sources, packageName, reporter, lOpt, fOpt, nameOpt)
    return if (reporter.hasError) None() else Some(r.render)
  }
}
