package org.sireum.tools.slangcheck

import org.sireum._
import org.sireum.message.Reporter
import org.sireum.test.TestSuite
import org.sireum.tools.SlangCheckJvm

class SlangCheckHAMRTest extends TestSuite {

  val sireum: Os.Path = Os.path(Os.env("SIREUM_HOME").get) / "bin" / (if (Os.isWin) "sireum.bat" else "sireum")

  "isolette" in {
    test("isolette", "isolette")
  }

  "temp_control" in {
    test("temp_control", "tc")
  }

  def test(projName: String, packageName: String): Unit = {

    if (!TestUtil.willingToWait) {
      return
    }

    val resultsDir = TestUtil.copy(projName, "hamr_results", x => x.name != string"project.cmd")

    println(s"Results: ${resultsDir.toUri}")

    val json = (TestUtil.resourceDir / projName / ".slang").list.filter(p => p.ext == string"json")
    assert (json.size == 1)

    println("Running codegen ...")
    proc"$sireum hamr codegen --package-name $packageName --output-dir $resultsDir ${json(0)}".echo.console.runCheck()

    val files = {
      val slangcheckBin = ops.StringOps((resultsDir / "bin" / "slangcheck.cmd").read)
      val searchString = "val files: ISZ[String] = ISZ("
      val start = slangcheckBin.stringIndexOf(searchString)
      def toFile(s: String): Os.Path = {
        val ss = ops.StringOps(ops.StringOps(s).trim)
        return resultsDir / ss.substring(4, ss.stringIndexOf("scala") + 5)
      }
      for (file <- ops.StringOps(ops.StringOps(slangcheckBin.substring(start + searchString.length, slangcheckBin.stringIndexOfFrom(")", start))).replaceAllLiterally("\r\n", "|")).split(c => c == C('|'))) yield toFile(file)
    }

    val reporter = Reporter.create

    val results = SlangCheckJvm.run(ISZ(packageName), files, reporter)

    assert (!reporter.hasError, reporter.errors)

    val dataDir = resultsDir / "src" / "main" / "data" / packageName
    for (r <- results._1) {
      val destFile = dataDir /+ r._1
      assert (destFile.exists)
      destFile.writeOver(ops.StringOps(r._2.render).replaceAllLiterally("\n\n", "\n"))
      println(s"Replaced: $destFile")
    }

    println("Running tipe ...")
    proc"$sireum proyek tipe .".at(resultsDir).echo.console.runCheck()

    println("Compiling ...")
    proc"$sireum proyek compile .".at(resultsDir).echo.console.runCheck()
  }
}
