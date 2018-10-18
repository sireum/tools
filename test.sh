#!/bin/bash -e
export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
cd ${SCRIPT_HOME}
./prelude.sh
./sireum slang tipe --verbose -r -s runtime/library:slang:kekinian/cli:shared:jvm
rm -fR out
./mill-standalone tools.jvm.tests.test
