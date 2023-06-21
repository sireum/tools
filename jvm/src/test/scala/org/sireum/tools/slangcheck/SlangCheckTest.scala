package org.sireum.tools.slangcheck.resources

import org.sireum._
import org.sireum.test.TestSuite
import org.sireum.tools.slangcheck.TestUtil
import org.sireum.tools.{SlangCheckJvm => SCJVM}
import org.sireum.$internal.RC
import org.sireum.message.Reporter

class SlangCheckTest extends TestSuite {

  val generateExpected: B = F

  val runGeneratedTests: B = F || TestUtil.isCI

  val verbose: B = F

  val sireum: Os.Path = Os.path(Os.env("SIREUM_HOME").get) / "bin" / (if (Os.isWin) "sireum.bat" else "sireum")

  "isolette" in {
    test("isolette", "isolette")
  }

  "temp_control" in {
    test("temp_control", "tc")
  }

  "option_argument" in {
    test("option_argument", "oa")
  }

//  "is_argument" in {
//    test("is_argument", "is")
//  }

  def test(str: String, pn: String): Unit = {

    val resultsDir = TestUtil.copy(str)

    // the following becomes a hyperlink in IVE. You can then use IVE's "Compare Directories"
    // to manually see any changes
    println(s"Result Dir: ${resultsDir.toUri}")

    val reporter = Reporter.create

    val artDir = resultsDir / "src" / "main" / "art" / "DataContent.scala"
    val dataFiles = Os.Path.walk(resultsDir/ "src" / "main" / "data", F, F, p => p.ext == string"scala" && !ops.StringOps(p.name).contains("SlangCheck")) :+ artDir

    println(dataFiles)

    val destDir = resultsDir / "src" / "main" / "data"
    val testDir = resultsDir / "src" / "test"

    SCJVM.run(dataFiles, destDir, testDir, reporter)

    var passing: B = T

    if (generateExpected) {
      val expectedDir = TestUtil.getExpectedDir(resultsDir)
      expectedDir.removeAll()
      resultsDir.copyOverTo(expectedDir)
      println(s"Replaced: ${expectedDir}")
    } else {
      passing = TestUtil.compare(resultsDir)
    }

    if (runGeneratedTests && !TestUtil.isCI) {
      val passed = proc"$sireum proyek test .".at(resultsDir).echo.console.run().ok
      passing = passing & (TestUtil.isCI || passed)
    }

    assert(passing)
  }
}