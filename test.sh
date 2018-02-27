#!/bin/bash -e
rm -fR runtime slang mill versions.properties out
wget -q http://files.sireum.org/mill
chmod +x mill
wget -q https://raw.githubusercontent.com/sireum/v3/master/versions.properties
git clone https://github.com/sireum/runtime runtime
git clone https://github.com/sireum/slang slang
TERM=xterm-color JAVA_OPTS="-Dorg.sireum.version.file=versions.properties" ./mill tools.jvm.tests.test
