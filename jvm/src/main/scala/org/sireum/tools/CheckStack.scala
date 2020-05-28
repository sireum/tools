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

object CheckStack {

  val NO_SIREUM_HOME: Z = -1
  val NOT_LINUX: Z = -2
  val PERL_UNAVAILABLE: Z = -3
  val OBJDUMP_UNAVAILABLE: Z = -4
  val OBJDUMP_ERROR: Z = -5

  def run(sireumHome: Os.Path, version: String, paths: ISZ[Os.Path], objdump: String, arch: String): Z = {

    var out = ISZ[String]()

    def dump(path: Os.Path): B = {
      val r = Os.proc(ISZ(objdump, "-d", path.string)).run()
      if (r.exitCode == 0) {
        out = out :+ r.out
      } else {
        eprintln(s"Could not disassemble $path")
        return F
      }
      return T
    }

    val checkstack = sireumHome / "bin" / "linux" / ".checkstack"
    val ver = sireumHome / "bin" / "linux" / ".checkstack.ver"
    if (!checkstack.exists || !ver.exists || ver.read != version) {
      checkstack.downloadFrom(s"https://raw.githubusercontent.com/torvalds/linux/$version/scripts/checkstack.pl")
      ver.write(version)
    }

    if (!Os.isLinux) {
      eprintln("This tool is only available under Linux")
      return NOT_LINUX
    }

    if (Os.proc(ISZ("perl", "-v")).run().exitCode != 0) {
      eprintln("This tool requires perl")
      return PERL_UNAVAILABLE
    }

    if (Os.proc(ISZ(objdump, "--version")).run().exitCode != 0) {
      eprintln(s"Could not find $objdump")
      return OBJDUMP_UNAVAILABLE
    }

    for (path <- paths) {
      if (path.isFile) {
        if (!dump(path)) {
          return OBJDUMP_ERROR
        }
      } else {
        for (p <- Os.Path.walk(path, F, F, (_: Os.Path) => T)) {
          if (ops.StringOps(p.name).endsWith(".o")) {
            if (!dump(p)) {
              return OBJDUMP_ERROR
            }
          }
        }
      }
    }
    val outs = st"""${(out, "\n\n")}""".render
    return Os.proc(ISZ("perl", checkstack.string, arch)).input(outs).console.run().exitCode
  }
}
