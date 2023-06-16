package org.sireum.tools.slangcheck

import org.sireum.$internal.RC
import org.sireum._

object TestUtil {
  val isCI: B = Os.env("GITLAB_CI").nonEmpty || Os.env("GITHUB_ACTIONS").nonEmpty || Os.env("BUILD_ID").nonEmpty
  val resourceDir: Os.Path = Os.path(implicitly[sourcecode.File].value).up.up.up.up.up.up / "resources" / "org" / "sireum" / "tools" / "slangcheck"

  def resources: scala.collection.Map[scala.Vector[Predef.String], Predef.String] =
    RC.text(Vector("../../../../../resources/org/sireum/tools/slangcheck")) { (p, f) => true }

  def copy(s: String): Os.Path = {
    val resultDir = resourceDir / s"${s}_results"
    resultDir.removeAll()
    for (r <- resources.filter(f => f._1(0) == s.native)) {
      val f = resultDir /+ ISZ[org.sireum.String](r._1.drop(1).map(f => org.sireum.String(f)): _*)
      f.writeOver(r._2)
      if (f.ext.native == "cmd") {
        f.chmod("700") // make project.cmd executable
      }
    }
    return resultDir
  }

  def getExpectedDir(resultsDir: Os.Path): Os.Path =
    return resultsDir.up / (ops.StringOps(resultsDir.name).replaceAllLiterally("_results", ""))

  def compare(results: Os.Path): B = {
    val expected = getExpectedDir(results)

    def toMap(dir: Os.Path): Map[String, Os.Path] = {
      var ret = Map.empty[String, Os.Path]
      def iter(d: Os.Path): Unit = {
        if (d.isFile) ret = ret + dir.relativize(d).value ~> d
        else for (dd <- d.list) yield iter(dd)
      }
      iter(dir)
      return ret
    }

    val eMap = toMap(expected)
    val rMap = toMap(results)

    def compareKeys(a: Set[String], b: Set[String], dir: Os.Path): B = {
      val diff = a -- b.elements
      for (d <- diff.elements) {
        cprintln(T, s"Couldn't match ${(dir / d).toUri}")
      }
      return diff.isEmpty
    }

    val (eSet, rSet) = (Set.empty ++ eMap.keys, Set.empty ++ rMap.keys)
    var sameContent = compareKeys(eSet, rSet, expected) & compareKeys(rSet, eSet, results)

    if (sameContent) {
      for (e <- eMap.keys) {
        val ef = expected / e
        val rf = results / e
        if (ef.read != rf.read) {
          sameContent = F
          cprint(T, s"File content differs: ${rf.toUri}")
        }
      }
    }
    return sameContent
  }
}
