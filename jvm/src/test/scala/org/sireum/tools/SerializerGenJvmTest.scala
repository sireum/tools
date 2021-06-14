/*
 Copyright (c) 2017-2021, Robby, Kansas State University
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

import org.sireum.message._
import org.sireum._
import Paths._
import org.sireum.test.SireumSpec

class SerializerGenJvmTest extends SireumSpec {

  *(gen(ISZ(slangInfoPath, slangAstPath, slangTypedPath), slangMsgPackPath, SerializerGen.Mode.MessagePack))

  *(gen(ISZ(slangInfoPath, slangAstPath, slangTypedPath), slangJSONPath, SerializerGen.Mode.JSON))

  def gen(srcs: ISZ[Os.Path], dest: Os.Path, mode: SerializerGen.Mode.Type): Boolean = {
    val reporter = Reporter.create
    val rOpt =
      SerializerGenJvm.run(
        mode,
        Some(licensePath),
        srcs,
        Some(ISZ[String]("org", "sireum", "lang", "tipe")),
        None(),
        reporter
      )
    reporter.printMessages()
    rOpt match {
      case Some(r) =>
        if (dest.exists) {
          val expected = dest.read.value
          val result = r.value
          if (!(result =~= expected)) {
            val dmp = new DiffMatchPatch()
            Console.err.println(dmp.patch_toText(dmp.patch_make(expected, result)))
            Console.err.flush()
            //dest.writeOver(r)
            //Console.err.println(r)
            //Console.err.flush()
            false
          } else !reporter.hasIssue
        } else {
          dest.writeOver(r)
          Console.err.println(r)
          Console.err.flush()
          false
        }
      case _ => false
    }
  }
}
