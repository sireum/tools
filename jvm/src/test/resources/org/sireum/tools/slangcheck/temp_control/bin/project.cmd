::/*#! 2> /dev/null                                   #
@ 2>/dev/null # 2>nul & echo off & goto BOF           #
if [ -z ${SIREUM_HOME} ]; then                        #
  echo "Please set SIREUM_HOME env var"               #
  exit -1                                             #
fi                                                    #
exec ${SIREUM_HOME}/bin/sireum slang run "$0" "$@"    #
:BOF
setlocal
if not defined SIREUM_HOME (
  echo Please set SIREUM_HOME env var
  exit /B -1
)
%SIREUM_HOME%\\bin\\sireum.bat slang run "%0" %*
exit /B %errorlevel%
::!#*/
// #Sireum

import org.sireum._
import org.sireum.project.{Module, Project, Target}

val home: Os.Path = Os.slashDir.up.canon

val slangModule: Module = Module(
  id = "temp_control",
  basePath = (home / "src").string,
  subPathOpt = None(),
  deps = ISZ(),
  targets = ISZ(Target.Jvm),
  ivyDeps = ISZ("org.sireum.kekinian::library:"),
  sources = ISZ("main"),
  resources = ISZ(),
  testSources = ISZ("test"),
  testResources = ISZ(),
  publishInfoOpt = None()
)

val prj: Project = Project.empty + slangModule

println(project.JSON.fromProject(prj, T))