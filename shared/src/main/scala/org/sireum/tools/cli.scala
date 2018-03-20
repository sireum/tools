// #Sireum
/*
 Copyright (c) 2018, Robby, Kansas State University
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
import org.sireum.cli.CliOpt._

object cli {
  val cliGenTool: Tool = Tool(
    name = "cligen",
    command = "cligen",
    description = "Command-line interface (CLI) generator",
    header = "Sireum CLI Generator",
    usage = "<option>* <config-file>",
    opts = ISZ(
      Opt(name = "packageName", longKey = "package", shortKey = Some('p'),
        tpe = Type.Str(sep = Some('.'), default = Some("cli")),
        description = "Package name for the CLI processor"),
      Opt(name = "name", longKey = "name", shortKey = Some('n'),
        tpe = Type.Str(sep = None(), default = Some("Cli")),
        description = "Type simple name for the CLI @record class processor"),
      Opt(name = "width", longKey = "width", shortKey = Some('w'),
        tpe = Type.Num(sep = Some(','), default = 0, min = Some(0), max = None()),
        description = "First (key) column (default: 25) and second column (default: 55) max width"),
      Opt(name = "license", longKey = "license", shortKey = Some('l'),
        tpe = Type.Path(multiple = F, default = None()), description = "License file to be inserted in the file header"),
      Opt(name = "outputDir", longKey = "output-dir", shortKey = Some('o'),
        tpe = Type.Path(multiple = F, default = Some(".")), description = "Output directory for the generated CLI Slang file")
    ),
    groups = ISZ()
  )

  val transformerGenTool: Tool = Tool(
    name = "transgen",
    command = "transgen",
    description = "Transformer (visitor/rewriter) generator",
    header = "Sireum Transformer Generator",
    usage = "<option>* <slang-file>",
    opts = ISZ(
      Opt(name = "modes", longKey = "modes", shortKey = Some('m'),
        tpe = Type.Choice(name = "TransformerMode", sep = Some(','), elements = ISZ("immutable", "mutable")),
        description = "Transformer mode"),
      Opt(name = "name", longKey = "name", shortKey = Some('n'),
        tpe = Type.Str(sep = None(), default = None()),
        description = "Type simple name for the transformers (default: \"Transformer\" or \"MTransformer\")"),
      Opt(name = "license", longKey = "license", shortKey = Some('l'),
        tpe = Type.Path(multiple = F, default = None()), description = "License file to be inserted in the file header"),
      Opt(name = "outputDir", longKey = "output-dir", shortKey = Some('o'),
        tpe = Type.Path(multiple = F, default = Some(".")), description = "Output directory for the generated transformer Slang files")
    ),
    groups = ISZ()
  )

  val serializerGenTool: Tool = Tool(
    name = "sergen",
    command = "sergen",
    description = "De/Serializer generator",
    header = "Sireum De/Serializer Generator",
    usage = "<option>* <slang-file>",
    opts = ISZ(
      Opt(name = "modes", longKey = "modes", shortKey = Some('m'),
        tpe = Type.Choice(name = "SerializerMode", sep = Some(','), elements = ISZ("json", "msgpack")),
        description = "De/serializer mode"),
      Opt(name = "packageName", longKey = "package", shortKey = Some('p'),
        tpe = Type.Str(sep = Some('.'), default = None()),
        description = "Type simple name for the de/serializers (default: \"Json\" or \"MsgPack\")"),
      Opt(name = "name", longKey = "name", shortKey = Some('n'),
        tpe = Type.Str(sep = None(), default = None()),
        description = "Type simple name for the de/serializers (default: \"Json\" or \"MsgPack\")"),
      Opt(name = "license", longKey = "license", shortKey = Some('l'),
        tpe = Type.Path(multiple = F, default = None()), description = "License file to be inserted in the file header"),
      Opt(name = "outputDir", longKey = "output-dir", shortKey = Some('o'),
        tpe = Type.Path(multiple = F, default = Some(".")), description = "Output directory for the generated de/serializer Slang files")
    ),
    groups = ISZ()
  )

  val group: Group = Group(
    name = "tools",
    description = "tools",
    header = "Sireum Tools",
    unlisted = F,
    subs = ISZ(cliGenTool, serializerGenTool, transformerGenTool)
  )
}
