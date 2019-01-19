// #Sireum
/*
 Copyright (c) 2019, Robby, Kansas State University
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

object IveGen {

  object Internal {

    def inspection: ST = {
      return st"""<component name="InspectionProjectProfileManager">
      |  <profile version="1.0">
      |    <option name="myName" value="Project Default" />
      |    <inspection_tool class="ComparingUnrelatedTypes" enabled="false" level="WARNING" enabled_by_default="false" />
      |    <inspection_tool class="RemoveRedundantReturn" enabled="false" level="WARNING" enabled_by_default="false" />
      |  </profile>
      |</component>"""
    }

    def misc(jdkName: String): ST = {
      return st"""<?xml version="1.0" encoding="UTF-8"?>
      |<project version="4">
      |  <component name="ProjectRootManager" version="2" languageLevel="JDK_11" default="true" project-jdk-name="$jdkName" project-jdk-type="JavaSDK">
      |    <output url="file://$$PROJECT_DIR$$/out" />
      |  </component>
      |</project>"""
    }

    def scalaSettings: ST = {
      return st"""<?xml version="1.0" encoding="UTF-8"?>
      |<project version="4">
      |  <component name="ScalaProjectSettings">
      |    <option name="autoRunDelay" value="3000" />
      |    <option name="dontCacheCompoundTypes" value="true" />
      |    <option name="inProcessMode" value="false" />
      |    <option name="intInjectionMapping">
      |      <map>
      |        <entry key="xml" value="XML" />
      |      </map>
      |    </option>
      |    <option name="metaTrimMethodBodies" value="false" />
      |    <option name="scFileMode" value="Ammonite" />
      |    <option name="scalaMetaMode" value="Disabled" />
      |    <option name="showNotFoundImplicitArguments" value="false" />
      |    <option name="treatDocCommentAsBlockComment" value="true" />
      |    <option name="treatScratchFilesAsWorksheet" value="false" />
      |  </component>
      |</project>"""
    }
  }

  def mill(
    name: String,
    projectPath: String,
    jdkName: String,
    scalaVer: String,
    scalacPluginVer: String
  ): Map[ISZ[String], ST] = {
    def workspace: ST = {
      return st"""<?xml version="1.0" encoding="UTF-8"?>
      |<project version="4">
      |  <component name="RunManager">
      |    <configuration default="true" type="Application" factoryName="Application">
      |      <method v="2" />
      |    </configuration>
      |    <configuration default="true" type="JUnit" factoryName="JUnit">
      |      <option name="MAIN_CLASS_NAME" value="" />
      |      <option name="METHOD_NAME" value="" />
      |      <option name="TEST_OBJECT" value="class" />
      |      <option name="PARAMETERS" value="" />
      |      <method v="2" />
      |    </configuration>
      |    <configuration default="true" type="ScalaTestRunConfiguration" factoryName="ScalaTest">
      |      <setting name="path" value="" />
      |      <setting name="vmparams" value="" />
      |      <setting name="params" value="" />
      |      <setting name="workingDirectory" value="file://$$PROJECT_DIR$$" />
      |      <setting name="searchForTest" value="Across module dependencies" />
      |      <setting name="showProgressMessages" value="true" />
      |      <setting name="useSbt" value="false" />
      |      <setting name="useUiWithSbt" value="false" />
      |      <envs />
      |      <setting name="testKind" value="Class" />
      |      <setting name="path" value="" />
      |      <method v="2" />
      |    </configuration>
      |    <configuration default="true" type="Specs2RunConfiguration" factoryName="Specs2">
      |      <setting name="path" value="" />
      |      <setting name="vmparams" value="" />
      |      <setting name="params" value="" />
      |      <setting name="workingDirectory" value="file://$$PROJECT_DIR$$" />
      |      <setting name="searchForTest" value="Across module dependencies" />
      |      <setting name="showProgressMessages" value="true" />
      |      <setting name="useSbt" value="false" />
      |      <setting name="useUiWithSbt" value="false" />
      |      <envs />
      |      <setting name="testKind" value="Class" />
      |      <setting name="path" value="" />
      |      <method v="2" />
      |    </configuration>
      |    <configuration default="true" type="TestNG">
      |      <option name="SUITE_NAME" value="" />
      |      <option name="MAIN_CLASS_NAME" value="" />
      |      <option name="GROUP_NAME" value="" />
      |      <option name="PARAMETERS" value="" />
      |      <option name="OUTPUT_DIRECTORY" value="" />
      |      <option name="TEST_SEARCH_SCOPE">
      |        <value defaultName="moduleWithDependencies" />
      |      </option>
      |      <option name="PROPERTIES_FILE" value="" />
      |      <properties />
      |      <listeners />
      |      <method v="2" />
      |    </configuration>
      |    <configuration default="true" type="uTestRunConfiguration" factoryName="utest">
      |      <setting name="path" value="" />
      |      <setting name="vmparams" value="" />
      |      <setting name="params" value="" />
      |      <setting name="workingDirectory" value="file://$$PROJECT_DIR$$" />
      |      <setting name="searchForTest" value="Across module dependencies" />
      |      <setting name="showProgressMessages" value="true" />
      |      <setting name="useSbt" value="false" />
      |      <setting name="useUiWithSbt" value="false" />
      |      <envs />
      |      <setting name="testKind" value="Class" />
      |      <setting name="path" value="" />
      |      <method v="2" />
      |    </configuration>
      |  </component>
      |</project>"""
    }
    def app: ST = {
      return st"""// #Sireum
      |import org.sireum._
      |
      |// Note: Run "mill -w $name.compile" in $projectPath in order for
      |// mill to recompile upon code modification
      |
      |object app extends App {
      |  def main(args: ISZ[String]): Z = {
      |    println("Hello World!")
      |    return 0
      |  }
      |}"""
    }
    def build: ST = {
      return st"""import mill._, scalalib._
      |
      |object `$name` extends ScalaModule {
      |  override def scalaVersion = "$scalaVer"
      |  override def ivyDeps = super.ivyDeps() ++ Agg(
      |    jpLatest(isCross = false, owner = "sireum", repo = "runtime", lib = "library")
      |  )
      |  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
      |    ivy"org.sireum::scalac-plugin:$scalacPluginVer"
      |  )
      |  override def repositories = super.repositories ++ Seq(
      |    coursier.maven.MavenRepository("https://jitpack.io")
      |  )
      |}
      |
      |
      |private def jpLatest(isCross: Boolean, owner: String, repo: String, lib: String = "",
      |             branchOrHash: Either[String, String] = Left("master")): Dep = {
      |  import ammonite.ops._
      |  def ghLatestCommit(owner: String, repo: String, branch: String): String = {
      |    val out = %%('git, "ls-remote", s"https://github.com/$$owner/$$repo.git")(pwd).out
      |    for (line <- out.lines if line.contains(s"refs/heads/$$branch"))
      |      return line.substring(0, line.indexWhere(_.isWhitespace))
      |    throw new RuntimeException(s"Could not determine latest commit for https://github.com/$$owner/$$repo.git branch $$branch!")
      |  }
      |  val hash = branchOrHash match {
      |    case Left(branch) => ghLatestCommit(owner, repo, branch)
      |    case Right(h) => h
      |  }
      |  val l = if ("" == lib) repo else lib
      |  owner match {
      |    case "sireum" => if (isCross) ivy"org.sireum.$$repo::$$l::$$hash" else ivy"org.sireum.$$repo::$$l:$$hash"
      |    case _ => if (isCross) ivy"com.github.$$owner.$$repo::$$l::$$hash" else ivy"com.github.$$owner.$$repo::$$l:$$hash"
      |  }
      |}
      |
      |"""
    }
    return Map ++ ISZ[(ISZ[String], ST)](
      ISZ[String](".idea", "inspectionProfiles", "Project_Default.xml") ~> Internal.inspection,
      ISZ[String](".idea", "misc.xml") ~> Internal.misc(jdkName),
      ISZ[String](".idea", "scala_settings.xml") ~> Internal.scalaSettings,
      ISZ[String](".idea", "workspace.xml") ~> workspace,
      ISZ[String](s"$name", "src", s"app.scala") ~> app,
      ISZ[String](s"build.sc") ~> build
    )
  }

  def idea(
    isWin: B,
    sireumHome: String,
    name: String,
    projectPath: String,
    jdkName: String,
    scalaVer: String,
    scalacPluginVer: String
  ): Map[ISZ[String], ST] = {
    val sep: String = if (isWin) "\\" else "/"
    def scalaLib: ST = {
      val scalaLibDir: ST = st"$sireumHome/bin/scala/lib"
      return st"""<component name="libraryTable">
      |  <library name="Scala" type="Scala">
      |    <properties>
      |      <compiler-classpath>
      |        <root url="file://$scalaLibDir/scala-compiler.jar" />
      |        <root url="file://$scalaLibDir/scala-library.jar" />
      |        <root url="file://$scalaLibDir/scala-reflect.jar" />
      |      </compiler-classpath>
      |    </properties>
      |    <CLASSES>
      |      <root url="jar://$scalaLibDir/scala-library.jar!/" />
      |      <root url="jar://$scalaLibDir/scala-reflect.jar!/" />
      |    </CLASSES>
      |    <JAVADOC>
      |      <root url="http://www.scala-lang.org/api/$scalaVer/" />
      |    </JAVADOC>
      |    <SOURCES />
      |  </library>
      |</component>"""
    }
    def sireumLib: ST = {
      return st"""<component name="libraryTable">
      |  <library name="Sireum">
      |    <CLASSES>
      |      <root url="jar://$sireumHome/bin/sireum.jar!/" />
      |    </CLASSES>
      |    <JAVADOC />
      |    <SOURCES />
      |  </library>
      |</component>"""
    }
    def scriptRunner: ST = {
      return st"""<component name="ProjectRunConfigurationManager">
      |  <configuration default="false" name="Slang Script Runner" type="Application" factoryName="Application" singleton="false">
      |    <option name="MAIN_CLASS_NAME" value="org.sireum.Sireum" />
      |    <module name="$name" />
      |    <option name="PROGRAM_PARAMETERS" value="slang run $$FilePath$$" />
      |    <method v="2" />
      |  </configuration>
      |</component>"""
    }
    def modules: ST = {
      return st"""<?xml version="1.0" encoding="UTF-8"?>
      |<project version="4">
      |  <component name="ProjectModuleManager">
      |    <modules>
      |      <module fileurl="file://$$PROJECT_DIR$$/$name.iml" filepath="$$PROJECT_DIR$$/$name.iml" />
      |    </modules>
      |  </component>
      |</project>"""
    }
    def scalaCompiler: ST = {
      return st"""<?xml version="1.0" encoding="UTF-8"?>
      |<project version="4">
      |  <component name="ScalaCompilerConfiguration">
      |    <option name="deprecationWarnings" value="true" />
      |    <option name="uncheckedWarnings" value="true" />
      |    <option name="featureWarnings" value="true" />
      |    <option name="explainTypeErrors" value="true" />
      |    <option name="specialization" value="false" />
      |    <plugins>
      |      <plugin path="file://$sireumHome/lib/scalac-plugin-$scalacPluginVer.jar" />
      |    </plugins>
      |  </component>
      |</project>"""
    }
    def workspace: ST = {
      val slangRun = st"$sireumHome${sep}bin${sep}slang-run.${if (isWin) "bat" else "sh"}"
      return st"""<?xml version="1.0" encoding="UTF-8"?>
      |<project version="4">
      |  <component name="RunManager" selected="Ammonite.Run script.sc">
      |    <configuration name="Run script.sc" type="ScalaAmmoniteRunConfigurationType" factoryName="Ammonite" singleton="false" temporary="true">
      |      <setting name="execName" value="$slangRun" />
      |      <setting name="fileName" value="$projectPath${sep}src${sep}script.sc" />
      |      <setting name="scriptParameters" value="" />
      |      <method v="2" />
      |    </configuration>
      |    <configuration default="true" type="ScalaAmmoniteRunConfigurationType" factoryName="Ammonite" singleton="false">
      |      <setting name="execName" value="$slangRun" />
      |      <setting name="fileName" value="" />
      |      <setting name="scriptParameters" value="" />
      |      <method v="2" />
      |    </configuration>
      |    <list>
      |      <item itemvalue="Ammonite.Run script.sc" />
      |      <item itemvalue="Application.Slang Script Runner" />
      |    </list>
      |    <recent_temporary>
      |      <list>
      |        <item itemvalue="Ammonite.Run script.sc" />
      |      </list>
      |    </recent_temporary>
      |  </component>
      |</project>"""
    }
    def script: ST = {
      return st"""// #Sireum
      |import org.sireum._
      |
      |println("Hello World!")"""
    }
    def iml: ST = {
      return st"""<?xml version="1.0" encoding="UTF-8"?>
      |<module type="JAVA_MODULE" version="4">
      |  <component name="NewModuleRootManager" inherit-compiler-output="true">
      |    <exclude-output />
      |    <content url="file://$$MODULE_DIR$$">
      |      <sourceFolder url="file://$$MODULE_DIR$$/src" isTestSource="false" />
      |    </content>
      |    <orderEntry type="inheritedJdk" />
      |    <orderEntry type="sourceFolder" forTests="false" />
      |    <orderEntry type="library" name="Scala" level="project" />
      |    <orderEntry type="library" name="Sireum" level="project" />
      |  </component>
      |</module>"""
    }
    return Map ++ ISZ[(ISZ[String], ST)](
      ISZ[String](".idea", "inspectionProfiles", "Project_Default.xml") ~> Internal.inspection,
      ISZ[String](".idea", "libraries", "Scala.xml") ~> scalaLib,
      ISZ[String](".idea", "libraries", "Sireum.xml") ~> sireumLib,
      ISZ[String](".idea", "runConfigurations", "Slang_Script_Runner.xml") ~> scriptRunner,
      ISZ[String](".idea", "misc.xml") ~> Internal.misc(jdkName),
      ISZ[String](".idea", "modules.xml") ~> modules,
      ISZ[String](".idea", "scala_compiler.xml") ~> scalaCompiler,
      ISZ[String](".idea", "scala_settings.xml") ~> Internal.scalaSettings,
      ISZ[String](".idea", "workspace.xml") ~> workspace,
      ISZ[String]("src", "script.sc") ~> script,
      ISZ[String](s"$name.iml") ~> iml
    )
  }
}
