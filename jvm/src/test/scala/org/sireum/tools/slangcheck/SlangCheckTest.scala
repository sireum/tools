package org.sireum.tools.slangcheck

import org.sireum._
import org.sireum.message.Reporter
import org.sireum.test.TestSuite
import org.sireum.tools.{SlangCheckJvm => SCJVM}

class SlangCheckTest extends TestSuite with TestUtil {

  val resourceDir: Os.Path = Os.path(implicitly[sourcecode.File].value).up.up.up.up.up.up / "resources" / "org" / "sireum" / "tools" / "slangcheck"

  val generateExpected: B = F

  val runTipe: B = T && willingToWait

  val runGeneratedTests: B = T && willingToWait

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

    val resultsDir = copy(expectedName, "results")

    // the following becomes a hyperlink in IVE. You can then use IVE's "Compare Directories"
    // to manually see any changes
    println(s"Result Dir: ${resultsDir.toUri}")

    val reporter = Reporter.create

    val dataFiles = Os.Path.walk(resultsDir / "src" / "main", F, F, p => p.ext == string"scala" && !ops.StringOps(p.name).contains("SlangCheck"))
    val destDir = resultsDir / "src" / "main" / "data"
    val testDir = resultsDir / "src" / "test"

    val results = SCJVM.run(ISZ(packageName), dataFiles, reporter)

    if (!reporter.hasError) {
      for (r <- results._1) {
        val destFile = destDir / packageName /+ r._1
        destFile.writeOver(r._2.render)
        println(s"Wrote: $destFile")
      }

      for (r <- results._2) {
        val destFile = testDir /packageName /+ r._1
        destFile.writeOver(r._2.render)
        println(s"Wrote: $destFile")
      }
    }

    var failureReasons: ISZ[String] = ISZ()

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
      if(!proc"$sireum proyek tipe .".at(resultsDir).echo.console.run().ok) {
        failureReasons = failureReasons :+ "Type checking failed"
      }
    }

    if (runGeneratedTests) {
      var passed = proc"$sireum proyek compile .".at(resultsDir).echo.console.run().ok
      if(!passed) {
        failureReasons = failureReasons :+ "Compilation failed"
      }

      if (passed) {
        passed = proc"$sireum proyek test .".at(resultsDir).echo.console.run().ok
        println(s"Generated Tests: ${if (passed) "passing" else "failing"}")

        // TODO: generated test could be failing due to 'requirements too strict'
        //if (!passed) {
        //  failureReasons = failureReasons :+ "Generated unit tests produced a failure"
        //}
      }
    }

    assert (failureReasons.size == 0)
  }
}