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

object CheckStack {

  @enum object Format {
    "Plain"
    "Html"
    "Md"
    "Rst"
    "Csv"
  }

  @datatype trait Template {
    @pure def row(elements: ISZ[String]): ST
    @pure def main(rows: ISZ[ST]): ST
    @pure def format(lines: ISZ[String]): ST
  }

  object Template {

    @pure def splitLine(line: String): ISZ[String] = {
      return ops.StringOps(line).split((c: C) => c == '\t' || c == ':' || c == ' ' || c == '[' || c == ']')
    }

    @datatype trait Dotsu extends Template {
      @pure def format(lines: ISZ[String]): ST = {
        var rows = ISZ[ST]()
        for (line <- lines) {
          val s = splitLine(line)
          if (s.size == 6) {
            rows = rows :+ row(s)
          } else {
            halt(s"Invalid input line: '$line'")
          }
        }
        return main(rows)
      }
    }

    object Dotsu {

      @datatype class Plain extends Dotsu {
        @strictpure def row(elements: ISZ[String]): ST = {
          val ISZ(filename, line, column, fname, size, alloc) = elements
          st"$filename:$line:$column:$fname $size $alloc"
        }
        @strictpure def main(rows: ISZ[ST]): ST = st"""${(rows, "\n")}"""
      }

      @datatype class Csv extends Dotsu {
        @strictpure def row(elements: ISZ[String]): ST = {
          val ISZ(filename, line, column, fname, size, alloc) = elements
          st"$filename,$line,$column,$fname,$size,$alloc"
        }
        @strictpure def main(rows: ISZ[ST]): ST =
          st"""Filename,Line,Column,Function,Bytes,Allocation
              |${(rows, "\n")}"""
      }

      @datatype class Html extends Dotsu {
        @strictpure def row(elements: ISZ[String]): ST = {
          val ISZ(filename, line, column, fname, size, alloc) = elements
          st"""<tr>
              |  <td>$filename [$line,$column]<br/>$fname</td>
              |  <td>$size</td>
              |  <td>$alloc</td>
              |</tr>"""
        }
        @strictpure def main(rows: ISZ[ST]): ST =
          st"""<table class="checkstack_table">
              |  <tr>
              |    <th>Function</th>
              |    <th>Bytes</th>
              |    <th>Allocation</th>
              |  </tr>
              |  ${(rows, "\n")}
              |</table>"""
      }

      @datatype class Md extends Dotsu {
        @strictpure def row(elements: ISZ[String]): ST = {
          val ISZ(filename, line, column, fname, size, alloc) = elements
          st"| $filename | $line:$column | $fname | $size | $alloc |"
        }
        @strictpure def main(rows: ISZ[ST]): ST =
          st"""| Filename | Position | Function | Bytes | Allocation |
               || :--- | :---: | :--- | ---: | :---: |
               |${(rows, "\n")}"""
      }

      @datatype class Rst extends Dotsu {
        @strictpure def row(elements: ISZ[String]): ST = {
          val ISZ(filename, line, column, fname, size, alloc) = elements
          st"""* - $filename [$line, $column]
              |    $fname
              |  - $size
              |  - $alloc"""
        }

        @strictpure def main(rows: ISZ[ST]): ST =
          st""".. list-table:: Checkstack Result
              |   :widths: 80 10 10
              |   :header-rows: 1
              |
              |   * - Function
              |     - Bytes
              |     - Allocation
              |   ${(rows, "\n")}"""
      }
    }

    @datatype trait Bin extends Template {
      @pure def format(lines: ISZ[String]): ST = {
        var rows = ISZ[ST]()
        for (line <- lines) {
          val s = splitLine(line)
          if (s.size == 4) {
            rows = rows :+ row(s)
          } else {
            halt(s"Invalid input line: '$line'")
          }
        }
        return main(rows)
      }
    }

    object Bin {

      @datatype class Plain extends Bin {
        @strictpure def row(elements: ISZ[String]): ST = {
          val ISZ(offset, fname, filename, size) = elements
          st"$filename:$offset:$fname\t$size"
        }
        @strictpure def main(rows: ISZ[ST]): ST = st"""${(rows, "\n")}"""
      }

      @datatype class Csv extends Bin {
        @strictpure def row(elements: ISZ[String]): ST = {
          val ISZ(offset, fname, filename, size) = elements
          st"$filename,$offset,$fname,$size"
        }
        @strictpure def main(rows: ISZ[ST]): ST =
          st"""Filename,Offset,Function,Bytes
              |${(rows, "\n")}"""
      }

      @datatype class Html extends Bin {
        @strictpure def row(elements: ISZ[String]): ST = {
          val ISZ(offset, fname, filename, size) = elements
          st"""<tr>
              |  <td>$filename [$offset]<br/>$fname</td>
              |  <td>$size</td>
              |</tr>"""
        }
        @strictpure def main(rows: ISZ[ST]): ST =
          st"""<table class="checkstack_table">
              |  <tr>
              |    <th>Function</th>
              |    <th>Bytes</th>
              |  </tr>
              |  ${(rows, "\n")}
              |</table>"""
      }

      @datatype class Md extends Bin {
        @strictpure def row(elements: ISZ[String]): ST = {
          val ISZ(offset, fname, filename, size) = elements
          st"| $filename | $offset | $fname | $size |"
        }
        @strictpure def main(rows: ISZ[ST]): ST =
          st"""| Filename | Offset | Function | Bytes |
               || :--- | ---: | :--- | ---: |
               |${(rows, "\n")}"""
      }

      @datatype class Rst extends Bin {
        @strictpure def row(elements: ISZ[String]): ST = {
          val ISZ(offset, fname, filename, size) = elements
          st"""* - $filename [$offset]
              |    $fname
              |  - $size"""
        }

        @strictpure def main(rows: ISZ[ST]): ST =
          st""".. list-table:: Checkstack Result
              |   :widths: 80 10 10
              |   :header-rows: 1
              |
              |   * - Function
              |     - Bytes
              |   ${(rows, "\n")}"""
      }
    }

  }

  val NO_SIREUM_HOME: Z = -1
  val NOT_LINUX: Z = -2
  val PERL_UNAVAILABLE: Z = -3
  val OBJDUMP_UNAVAILABLE: Z = -4
  val OBJDUMP_ERROR: Z = -5
  val NO_INPUT: Z = -6
  val INVALID_INPUT: Z = -7

  def run(sireumHome: Os.Path, checkstack: Os.Path, paths: ISZ[Os.Path], isBin: B, objdump: String, arch: String,
          format: Format.Type): Z = {

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

    def dotsu(): Z = {
      @pure def ltDotsu(s1: String, s2: String): B = {
        val s1s = Template.splitLine(s1)
        if (s1s.size != 6) {
          halt(
            st"""Ill-formed .su line:
                |$s1""".render)
        }
        val n1: Z = Z(s1s(4)) match {
          case Some(m1) => m1
          case _ =>
            halt(
              st"""Ill-formed .su line:
                  |$s1""".render)
        }
        val s2s = Template.splitLine(s2)
        if (s2s.size != 6) {
          halt(
            st"""Ill-formed .su line:
                |$s2""".render)
        }
        val n2: Z = Z(s2s(4)) match {
          case Some(m2) => m2
          case _ =>
            halt(
              st"""Ill-formed .su line:
                  |$s2""".render)
        }
        if (n1 > n2) {
          return T
        } else if (n1 == n2) {
          return s1s(0).size < s2s(0).size
        } else {
          return F
        }
      }
      for (path <- paths) {
        for (file <- Os.Path.walk(path, F, F, (p: Os.Path) => ops.StringOps(p.name).endsWith(".su"))) {
          out = out ++ file.readLines
        }
      }
      if (out.isEmpty) {
        eprintln("Could not find .su files")
        return NO_INPUT
      }

      val template: Template = format match {
        case CheckStack.Format.Plain => Template.Dotsu.Plain()
        case CheckStack.Format.Csv => Template.Dotsu.Csv()
        case CheckStack.Format.Md => Template.Dotsu.Md()
        case CheckStack.Format.Rst => Template.Dotsu.Rst()
        case CheckStack.Format.Html => Template.Dotsu.Html()
      }

      println(template.format(ops.ISZOps(out).sortWith(ltDotsu _)).render)
      return 0
    }

    def bin(): Z = {
      @pure def ltBin(s1: String, s2: String): B = {
        val s1s = Template.splitLine(s1)
        if (s1s.size != 4) {
          halt(
            st"""Ill-formed .su line:
                |$s1""".render)
        }
        val n1: Z = Z(s1s(3)) match {
          case Some(m1) => m1
          case _ =>
            halt(
              st"""Ill-formed .su line:
                  |$s1""".render)
        }
        val s2s = Template.splitLine(s2)
        if (s2s.size != 4) {
          halt(
            st"""Ill-formed .su line:
                |$s2""".render)
        }
        val n2: Z = Z(s2s(3)) match {
          case Some(m2) => m2
          case _ =>
            halt(
              st"""Ill-formed .su line:
                  |$s2""".render)
        }
        if (n1 > n2) {
          return T
        } else if (n1 == n2) {
          return s1s(3).size < s2s(3).size
        } else {
          return F
        }
      }
      if (!Os.isLinuxAmd) {
        eprintln("Binary mode is only available under Linux")
        return NOT_LINUX
      }
      if (Os.proc(ISZ("perl", "-v")).run().exitCode != 0) {
        eprintln("Binary mode requires perl")
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
      if (out.isEmpty) {
        eprintln("Could not find .o files")
        return NO_INPUT
      }
      val outs = st"""${(out, "\n\n")}""".render
      val r = Os.proc(ISZ("perl", checkstack.string, arch)).input(outs).redirectErr.run()
      val lines = ops.ISZOps(ops.StringOps(r.out).split((c: C) => c == '\n')).sortWith(ltBin _)

      val template: Template = format match {
        case CheckStack.Format.Plain => Template.Bin.Plain()
        case CheckStack.Format.Csv => Template.Bin.Csv()
        case CheckStack.Format.Md => Template.Bin.Md()
        case CheckStack.Format.Rst => Template.Bin.Rst()
        case CheckStack.Format.Html => Template.Bin.Html()
      }

      println(template.format(lines).render)

      return r.exitCode
    }

    if (isBin) {
      return bin()
    } else {
      return dotsu()
    }

  }
}
