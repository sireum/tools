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

import org.sireum.Os

object Paths {
  val rootDir: Os.Path = Os.cwd
  val licensePath: Os.Path = {
    val f = rootDir / "license.txt"
    if (f.exists) f else rootDir.up / "license.txt"
  }
  val cliConfigPath = rootDir / "kekinian" / "cli" / "jvm" / "src" / "main" / "scala" / "org" / "sireum" / "cli.sc"
  val cliPath = rootDir / "kekinian" / "cli" / "jvm" / "src" / "main" / "scala" / "org" / "sireum" / "Cli.scala"
  val slangAstPackagePath = rootDir / "slang" / "ast" / "shared" / "src" / "main" / "scala" / "org" / "sireum" / "lang" / "ast"
  val slangAstPath = slangAstPackagePath / "AST.scala"
  val slangTypedPath = slangAstPackagePath / "Typed.scala"
  val slangMTransformerPath = slangAstPackagePath / "MTransformer.scala"
  val slangTransformerPath = slangAstPackagePath / "Transformer.scala"
  val slangTipePath = rootDir / "slang" / "tipe" / "shared" / "src" / "main" / "scala" / "org" / "sireum" / "lang"
  val slangInfoPath = slangTipePath / "symbol" / "Info.scala"
  val slangJSONPath = slangTipePath / "tipe" / "JSON.scala"
  val slangMsgPackPath = slangTipePath / "tipe" / "MsgPack.scala"
}
