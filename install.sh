#!/usr/bin/env bash

# Install cask and language dependencies.

set -e

if [ ! -d ~/.cask ]; then
  curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
fi

pushd $(dirname $0) > /dev/null
  ~/.cask/bin/cask install
popd > /dev/null

# On OS X, homebrew pip will configure a local install path by
# default, and passing --user on the commandline will cause an error.
pip_cmd="pip install --upgrade"
# For linux distros, --user is necessary to install as a non-privileged user.
if ! which sw_vers &> /dev/null ; then
  pip_cmd="$pip_cmd --user"
fi
$pip_cmd jedi rope flake8 importmagic

if which go &> /dev/null; then
  tool_urls=(
    'github.com/rogpeppe/godef'
    'github.com/dougm/goflymake'
    'github.com/kisielk/errcheck'
    'github.com/nsf/gocode'
    'github.com/tools/godep'
    'golang.org/x/tools/cmd/cover'
    'golang.org/x/tools/cmd/gorename'
  )
  export GOPATH="${HOME}/go"
  for tool_url in ${tool_urls[@]}; do
    go get "${tool_url}"
  done
fi
