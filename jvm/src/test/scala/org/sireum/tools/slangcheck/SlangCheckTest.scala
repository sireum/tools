package org.sireum.tools.slangcheck.resources

import org.sireum._
import org.sireum.test.TestSuite
import org.sireum.tools.slangcheck.TestUtil

class SlangCheckTest extends TestSuite {

  val generateExpected: B = F

  val runGeneratedTests: B = T || TestUtil.isCI

  val sireum = Os.path(Os.env("SIREUM_HOME").get) / "bin" / (if (Os.isWin) "sireum.bat" else "sireum")

  "isolette" in {
    test("isolette")
  }

  "temp_control" in {
    test("temp_control")
  }

  def test(str: String): Unit = {

    val resultsDir = TestUtil.copy(str)

    // the following becomes a hyperlink in IVE. You can then use IVE's "Compare Directories"
    // to manually see any changes
    println(s"Result Dir: ${resultsDir.toUri}")

    // TODO: run slangcheck, possibly filtering out previously generated slangcheck artifacts

    //(resultsDir / "delme").write("delme") // simulate a change

    if (generateExpected) {
      val expectedDir = TestUtil.getExpectedDir(resultsDir)
      expectedDir.removeAll()
      resultsDir.copyOverTo(expectedDir)
      println(s"Replaced: ${expectedDir}")
    } else {
      assert(TestUtil.compare(resultsDir))
    }

    if (runGeneratedTests) {
      proc"$sireum proyek test .".at(resultsDir).echo.console.runCheck()
    }
  }
}
