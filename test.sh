#!/bin/bash -e
rm -fR runtime slang cli mill-standalone versions.properties out
curl -Lo mill-standalone http://files.sireum.org/mill-standalone
chmod +x mill-standalone
curl -Lo versions.properties https://raw.githubusercontent.com/sireum/kekinian/master/versions.properties
curl -Lo license.txt https://raw.githubusercontent.com/sireum/kekinian/master/license.txt
git clone --depth 1 https://github.com/sireum/runtime
git clone --depth 1 https://github.com/sireum/slang
git clone --depth 1 https://github.com/sireum/cli
./mill-standalone tools.jvm.tests.test
