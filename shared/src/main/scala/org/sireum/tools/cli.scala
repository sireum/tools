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

  val iveGenTool: Tool = Tool(
    name = "ivegen",
    command = "ivegen",
    description = "Sireum IVE project generator",
    header = "Sireum IVE Project Generator",
    usage = "<option>* <project-parent-directory>",
    opts = ISZ(
      Opt(name = "jdk", longKey = "jdk", shortKey = Some('j'),
        tpe = Type.Str(sep = None(), default = Some("Java")),
        description = "JDK name"),
      Opt(name = "mode", longKey = "mode", shortKey = Some('m'),
        tpe = Type.Choice(name = "IveMode", sep = None(), elements = ISZ("idea", "mill")),
        description = "Project format (use idea for Slang script project and mill for full Slang development)"),
      Opt(name = "projectName", longKey = "name", shortKey = Some('n'),
        tpe = Type.Str(sep = None(), default = Some("hello")),
        description = "Project name"),
      Opt(name = "moduleName", longKey = "module", shortKey = None(),
        tpe = Type.Str(sep = None(), default = None()),
        description = "Module name (default: project name)"),
      Opt(name = "packageName", longKey = "package", shortKey = Some('p'),
        tpe = Type.Str(sep = Some('.'), default = None()),
        description = "Fully qualified app package name"),
      Opt(name = "appName", longKey = "app", shortKey = None(),
        tpe = Type.Str(sep = None(), default = None()),
        description = "App/script name (default: \"app\" in mill mode; otherwise, \"script\")"),
      Opt(name = "millPath", longKey = "mill-path", shortKey = None(),
        tpe = Type.Flag(default = F),
        description = "Use mill available in the PATH environment variable (only in mill mode)"),
      Opt(name = "force", longKey = "force", shortKey = Some('f'),
        tpe = Type.Flag(default = F),
        description = "Force regeneration of JDK and library tables"),
      Opt(name = "compile", longKey = "no-compile", shortKey = Some('c'),
        tpe = Type.Flag(default = T),
        description = "Only generate mill project without code compilation"),
    ),
    groups = ISZ()
  )

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
    usage = "<option>* <slang-file>+",
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
        description = "Package name for the de/serializers"),
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

  val bcGenTool: Tool = Tool(
    name = "bcgen",
    command = "bcgen",
    description = "Bit encoder/decoder generator",
    header = "Sireum BitCodec Generator",
    usage = "<option>* <spec-file>",
    opts = ISZ(
      Opt(name = "mode", longKey = "mode", shortKey = Some('m'),
        tpe = Type.Choice(name = "BitCodecMode", sep = Some(','), elements = ISZ("program", "script", "json", "dot")),
        description = "Generated codec unit mode"),
      Opt(name = "isLittleEndian", longKey = "little", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Generate little-endian bitcodec instead of big-endian"),
      Opt(name = "isMutable", longKey = "mutable", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Use MS instead of IS on decode methods"),
      Opt(name = "packageName", longKey = "package", shortKey = Some('p'),
        tpe = Type.Str(sep = Some('.'), default = None()),
        description = "Package name for the codec"),
      Opt(name = "name", longKey = "name", shortKey = Some('n'),
        tpe = Type.Str(sep = None(), default = Some("BitCodec")),
        description = "Object and filename for the codec (script always uses BitCodec as the object name)"),
      Opt(name = "license", longKey = "license", shortKey = Some('l'),
        tpe = Type.Path(multiple = F, default = None()), description = "License file to be inserted in the file header"),
      Opt(name = "outputDir", longKey = "output-dir", shortKey = Some('o'),
        tpe = Type.Path(multiple = F, default = Some(".")), description = "Output directory for the generated codec files"),
      Opt(name = "traits", longKey = "traits", shortKey = Some('t'),
        tpe = Type.Str(sep = Some(';'), default = None()), description = "Fully-qualified name of @sig traits for all bitcodec types to extend")
    ),
    groups = ISZ()
  )

  val checkstackTool: Tool = Tool(
    name = "checkstack",
    command = "checkstack",
    description = "Native function stack size check tool",
    header = "Sireum CheckStack",
    usage = "<option>* ( <file> | <dir> )*",
    opts = ISZ(
      Opt(name = "mode", longKey = "mode", shortKey = Some('m'),
        tpe = Type.Choice(name = "CheckStackMode", sep = None(), elements =  ISZ("dotsu", "bin")),
        description = "Analysis mode"),
    ),
    groups = ISZ(
      OptGroup(name = "Binary Mode", opts = ISZ(
        Opt(name = "objdump", longKey = "objdump", shortKey = Some('o'),
          tpe = Type.Str(None(), Some("objdump")),
          description = "Name of object file dumper"),
        Opt(name = "arch", longKey = "arch", shortKey = Some('a'),
          tpe = Type.Choice("CheckStackArch", None(), ISZ(
            "amd64", "x86", "aarch64", "arm", "powerpc", "openrisc", "mips", "mips64", "m68k", "ia64", "nios2", "parisc",
            "s390x", "sh64", "sparc"
          )),
          description = "Target architecture")
      )),
      OptGroup(name = "Output Mode", opts = ISZ(
        Opt(name = "format", longKey = "format", shortKey = Some('f'),
          tpe = Type.Choice("CheckStackFormat", None(), ISZ(
            "plain", "csv", "html", "md", "rst"
          )),
          description = "Output format")
      ))
    ),
  )

  val group: Group = Group(
    name = "tools",
    description = "Utility tools",
    header = "Sireum Utility Tools",
    unlisted = F,
    subs = ISZ(bcGenTool, checkstackTool, cliGenTool, iveGenTool, serializerGenTool, transformerGenTool)
  )
}
