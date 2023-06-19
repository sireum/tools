package org.sireum.tools.slangcheck.resources

import org.sireum._
import org.sireum.test.TestSuite
import org.sireum.tools.slangcheck.TestUtil
import org.sireum.tools.{SlangCheckJvm => SCJVM}
import org.sireum.$internal.RC
import org.sireum.message.Reporter

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

    def dataSources: scala.collection.Map[scala.Vector[Predef.String], Predef.String] =
      RC.text(Vector("../../../../../resources/org/sireum/tools/slangcheck")) { (p, f) => p.last.contains(".scala") && !p.last.contains("SlangCheck") }

    val reporter = Reporter.create

    //SCJVM.run(ISZ(), resultsDir, resultsDir, reporter) // TODO: How to get File paths from vector to Os.path

    //(resultsDir / "delme").write("delme") // simulate a change

    var passing: B = T

    if (generateExpected) {
      val expectedDir = TestUtil.getExpectedDir(resultsDir)
      expectedDir.removeAll()
      resultsDir.copyOverTo(expectedDir)
      println(s"Replaced: ${expectedDir}")
    } else {
      passing = TestUtil.compare(resultsDir)
    }

    if (runGeneratedTests) {
      passing = passing & proc"$sireum proyek test .".at(resultsDir).echo.console.run().ok
    }

    assert(passing)
  }
}