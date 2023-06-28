package org.sireum.tools.slangcheck.resources

import org.sireum._
import org.sireum.message.Reporter
import org.sireum.test.TestSuite
import org.sireum.tools.slangcheck.TestUtil
import org.sireum.tools.{SlangCheckJvm => SCJVM}

class SlangCheckTest extends TestSuite {

  val generateExpected: B = F

  val runTipe: B = T

  val runGeneratedTests: B = F

  val verbose: B = F

  val sireum: Os.Path = Os.path(Os.env("SIREUM_HOME").get) / "bin" / (if (Os.isWin) "sireum.bat" else "sireum")

  "isolette" in {
    test("isolette", "isolette")
  }

  "temp_control" in {
    test("temp_control", "tc")
  }

//  "option_argument" in {
//    test("option_argument", "oa")
//  }

  //  "is_argument" in {
  //    test("is_argument", "is")
  //  }

  def test(expectedName: String, packageName: String): Unit = {

    val resultsDir = TestUtil.copy(expectedName)

    // the following becomes a hyperlink in IVE. You can then use IVE's "Compare Directories"
    // to manually see any changes
    println(s"Result Dir: ${resultsDir.toUri}")

    val reporter = Reporter.create

    val artDir = resultsDir / "src" / "main" / "art" / "DataContent.scala"
    val dataFiles = Os.Path.walk(resultsDir / "src" / "main" / "data", F, F, p => p.ext == string"scala" && !ops.StringOps(p.name).contains("SlangCheck")) :+ artDir
    val destDir = resultsDir / "src" / "main" / "data"
    val testDir = resultsDir / "src" / "test"

    val results = SCJVM.run(ISZ(packageName), dataFiles, reporter)

    if (!reporter.hasError) {
      for (r <- results._1) {
        val destFile = destDir /+ r._1
        destFile.writeOver(ops.StringOps(r._2.render).replaceAllLiterally("\r\n", "\n"))
        println(s"Wrote: $destFile")
      }

      for (r <- results._2) {
        val destFile = testDir /+ r._1
        destFile.writeOver(ops.StringOps(r._2.render).replaceAllLiterally("\r\n", "\n"))
        println(s"Wrote: $destFile")
      }
    }

    var passing: B = T

    if (generateExpected) {
      assert (!TestUtil.isCI, "generateExpected should be F when code is pushed to github")

      val expectedDir = TestUtil.getExpectedDir(resultsDir)
      expectedDir.removeAll()
      resultsDir.copyOverTo(expectedDir)
      println(s"Replaced: ${expectedDir}")
    } else {
      passing = TestUtil.compare(resultsDir)
    }

    if (runTipe || TestUtil.isCI) {
      passing = passing & proc"$sireum proyek tipe .".at(resultsDir).echo.console.run().ok
    }

    if (runGeneratedTests && !TestUtil.isCI) {
      val passed = proc"$sireum proyek test .".at(resultsDir).echo.console.run().ok
      passing = passing & (TestUtil.isCI || passed)
    }

    assert(passing)
  }
}