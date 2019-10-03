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

# Install gimme
if [ ! -f ~/bin/gimme ]; then
  curl -sL -o ~/bin/gimme https://raw.githubusercontent.com/travis-ci/gimme/5256888df1e749fdb6acabc8f9e07a52f05b9f28/gimme
  chmod +x ~/bin/gimme
fi

# Install golang dev tools
eval $(~/bin/gimme 1.13.1)
tool_urls=(
  'github.com/derekparker/delve/cmd/dlv'
  'github.com/dougm/goflymake'
  'github.com/kisielk/errcheck'
  'github.com/nsf/gocode'
  'github.com/rogpeppe/godef'
  'github.com/tools/godep'
  'golang.org/x/tools/cmd/cover'
  'golang.org/x/tools/cmd/gorename'
  'golang.org/x/tools/cmd/guru'
)
export GOPATH=~/go
for tool_url in ${tool_urls[@]}; do
  go get -u "${tool_url}"
done
