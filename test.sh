#!/bin/bash -e
rm -fR sireum-v3
git clone https://github.com/sireum/v3 sireum-v3
cd sireum-v3
rm -fR tools
ln -s .. tools
cd tools
rm -fR mill runtime slang
wget -q http://files.sireum.org/mill
chmod +x mill
git clone https://github.com/sireum/runtime runtime
git clone https://github.com/sireum/slang slang
TERM=xterm-color SIREUM_HOME=".." ./mill tools.jvm.tests.test
