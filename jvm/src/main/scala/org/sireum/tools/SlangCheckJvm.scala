// #Sireum
package org.sireum.tools

import org.sireum._
import org.sireum.lang.{FrontEnd, ast => AST}
import org.sireum.message.Reporter
import org.sireum.tools.{SlangCheck => SC}
import org.sireum.lang.tipe.{TypeChecker, TypeHierarchy, TypeOutliner}

object SlangCheckJvm {
  def run(sources: ISZ[Os.Path],
          destDir: Os.Path,
          testDir: Os.Path,
          reporter: Reporter): Unit = {
    if (sources.isEmpty) { //Checks if files are present
      reporter.error(None(), "SlangCheckGenerator", "Expecting a program input")
      return
    }

    def readFile(f: Os.Path): (Option[String], String) = {
      return (Some(f.toUri), f.read)
    }

    var programs = ISZ[AST.TopUnit.Program]() //list of all slang programs
    for (src <- sources) { //go through all folders
      val srcText = src.read //read files
      val r = lang.parser.Parser.parseTopUnit[AST.TopUnit](srcText, F, F, Some(src.toUri), reporter) //parse program
      r match { //adds all programs that are valid to programs
        case Some(p: AST.TopUnit.Program) =>
          programs = programs :+ p
        case _ =>
          reporter.error(None(), "SlangCheckGenerator", s"$src is not a Slang program")
          return
      }
    }


    var sources2 = ISZ[FrontEnd.Input]()
    for (p <- sources) {
      val x = readFile(p)
      sources2 = sources2 :+ FrontEnd.Input(x._2, x._1)
    }

    var th: TypeHierarchy = {
      val (thl, rep): (TypeHierarchy, Reporter) = {
        val p = FrontEnd.libraryReporter
        (p._1.typeHierarchy, p._2)
      }

      if (rep.hasError) {
        rep.printMessages()
      }

      thl
    }

    print()

    val t = FrontEnd.parseProgramAndGloballyResolve(0, sources2,
      th.nameMap, th.typeMap)

    th = TypeHierarchy.build(F, th(nameMap = t._3, typeMap = t._4), reporter)

    th = TypeOutliner.checkOutline(0, T, th, reporter)

    th = TypeChecker.checkComponents(0, T, th, th.nameMap, th.typeMap, reporter)

    print()

    val results = SC.gen(for (source <- sources) yield source.toUri, programs, reporter, th)
    if (!reporter.hasError) {
      for (r <- results) {
        val destFile = destDir /+ r._1
        destFile.writeOver(r._2.render)
        println(s"Wrote: $destFile")
      }
    }

    val testResults = SlangCheckTest.gen(for (source <- sources) yield source.toUri, programs, reporter, th)
    if (!reporter.hasError) {
      for (r <- testResults) {
        val destFile = testDir /+ r._1
        destFile.writeOver(r._2.render)
        println(s"Wrote: $destFile")
      }
    }
  }
}
