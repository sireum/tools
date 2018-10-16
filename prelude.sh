#!/bin/bash -e
export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
cd ${SCRIPT_HOME}
if [ ! -e mill-standalone ]; then
  curl -Lo mill-standalone http://files.sireum.org/mill-standalone
  chmod +x mill-standalone
fi
if [ ! -f sireum ]; then
  curl -Lo sireum http://files.sireum.org/sireum
  chmod +x sireum
fi
if [ ! -e versions.properties ]; then
  curl -Lo versions.properties https://raw.githubusercontent.com/sireum/kekinian/master/versions.properties
fi
if [ ! -e license.txt ]; then
  curl -Lo license.txt https://raw.githubusercontent.com/sireum/kekinian/master/license.txt
fi
rm -fR runtime slang cli
git clone --depth=1 https://github.com/sireum/runtime
git clone --depth=1 https://github.com/sireum/slang
git clone --depth=1 https://github.com/sireum/cli
