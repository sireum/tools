package org.sireum.tools.slangcheck

import org.sireum._
import org.sireum.message.Reporter
import org.sireum.test.TestSuite
import org.sireum.tools.{SlangCheckJvm => SCJVM}

class SlangCheckTest extends TestSuite with TestUtil {

  val resourceDir: Os.Path = Os.path(implicitly[sourcecode.File].value).up.up.up.up.up.up / "resources" / "org" / "sireum" / "tools" / "slangcheck"

  val generateExpected: B = T

  val runTipe: B = T //&& willingToWait

  val runGeneratedTests: B = T //&& willingToWait

  val verbose: B = F

  val sireum: Os.Path = Os.path(Os.env("SIREUM_HOME").get) / "bin" / (if (Os.isWin) "sireum.bat" else "sireum")

  "isolette" in {
    test("isolette", "isolette", x => !x.value.native.contains("component"))
  }

  "temp_control" in {
    test("temp_control", "tc")
  }

  "option_argument" in {
    test("option_argument", "oa")
  }

  "is_argument" in {
    test("is_argument", "is")
  }

  "ms_argument" in {
    test("ms_argument", "ms")
  }

//  "is_is_argument" in {
//    test("is_is_argument", "ms")
//  }

  "datatype_trait" in {
    test("datatype_trait", "dttr")
  }

  def test(expectedName: String, packageName: String, filter: Os.Path => B = x => T): Unit = {

    var failureReasons: ISZ[String] = ISZ()

    val resultsDir = copy(expectedName, "results")

    // the following becomes a hyperlink in IVE. You can then use IVE's "Compare Directories"
    // to manually see any changes
    println(s"Result Dir: ${resultsDir.toUri}")

    val reporter = Reporter.create

    val dataFiles = Os.Path.walk(resultsDir / "src" / "main", F, F, p => p.ext == string"scala" && !ops.StringOps(p.name).contains("SlangCheck"))
    val destDir = resultsDir / "src" / "main" / "data"
    val testDir = resultsDir / "src" / "test"

    println("Generating SlangCheck artifacts ...")
    val results = SCJVM.run(ISZ(packageName), dataFiles, reporter)

    failureReasons = failureReasons ++ (for(e <- reporter.errors) yield e.text)

    if (!reporter.hasError) {
      for (r <- results._1) {
        val destFile = destDir / packageName /+ r._1
        destFile.writeOver(r._2.render)
        if (verbose) {
          println(s"Wrote: $destFile")
        }
      }

      for (r <- results._2) {
        val destFile = testDir /packageName /+ r._1
        destFile.writeOver(r._2.render)
        if (verbose) {
          println(s"Wrote: $destFile")
        }
      }
    }

    if (generateExpected) {
      assert (!isCI, "generateExpected should be F when code is pushed to github")

      val expectedDir = getExpectedDir(resultsDir)
      expectedDir.removeAll()
      resultsDir.copyOverTo(expectedDir)
      println(s"Replaced: ${expectedDir}")
    } else {
      if (!compare(resultsDir, filter)) {
        failureReasons = failureReasons :+ "Results did not match expected"
      }
    }

    if (runTipe) {
      println("Running tipe ...")
      val tresults = proc"$sireum proyek tipe .".at(resultsDir).run()
      if(!tresults.ok) {
        failureReasons = failureReasons :+ "Type checking failed"
        println(tresults.out)
        println(tresults.err)
      }
    }

    if (runGeneratedTests) {
      println("Compiling via proyek ...")
      val cresults = proc"$sireum proyek compile .".at(resultsDir).run()
      if(!cresults.ok) {
        failureReasons = failureReasons :+ "Compilation failed"
        println(cresults.out)
        println(cresults.err)
      }

      if (cresults.ok) {
        println("Running generated test cases ...")
        val ttresults = proc"$sireum proyek test .".at(resultsDir).run()
        println(s"Generated Tests: ${if (ttresults.ok) "passing" else "failing"}")

        // TODO: generated test could be failing due to 'requirements too strict'
        //if (!passed) {
        //  failureReasons = failureReasons :+ "Generated unit tests produced a failure"
        //}
      }
    }

    assert (failureReasons.size == 0)
  }
}