#!/bin/bash -e
export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
cd ${SCRIPT_HOME}
if [ ! -e mill-standalone ]; then
  curl -c /dev/null -Lo mill-standalone http://files.sireum.org/mill-standalone
  chmod +x mill-standalone
fi
if [ ! -f sireum ]; then
  curl -c /dev/null -Lo sireum http://files.sireum.org/sireum
  chmod +x sireum
fi
rm -fR runtime slang kekinian
git clone --depth=1 https://github.com/sireum/runtime
git clone --depth=1 https://github.com/sireum/slang
git clone --depth=1 https://github.com/sireum/kekinian
if [ ! -e versions.properties ]; then
  cp kekinian/versions.properties .
fi
if [ ! -e license.txt ]; then
  cp kekinian/license.txt .
fi
