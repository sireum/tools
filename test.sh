#!/bin/bash -e
rm -fR sireum-v3
git clone --depth 1 https://github.com/sireum/v3 sireum-v3
cd sireum-v3
rm -fR runtime slang tools
git clone --depth 1 https://github.com/sireum/tools
git clone --depth 1 https://github.com/sireum/runtime
git clone --depth 1 https://github.com/sireum/slang
cd tools
ln -s ../runtime
ln -s ../slang
wget -q http://files.sireum.org/mill-standalone
chmod +x mill-standalone
SIREUM_HOME=".." ./mill-standalone tools.jvm.tests.test
