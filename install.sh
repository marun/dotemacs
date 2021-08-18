#!/usr/bin/env bash

# Install cask and language dependencies.

set -e

# Install cask and language dependencies.

if [ ! -d ~/.cask ]; then
  git clone https://github.com/cask/cask ~/.cask
  pushd ~/.cask > /dev/null
    make install
  popd > /dev/null
fi

pushd $(dirname $0) > /dev/null
  ~/.cask/bin/cask install
popd > /dev/null

# Install gimme
if [ ! -f ~/bin/gimme ]; then
  curl -sL -o ~/bin/gimme https://raw.githubusercontent.com/travis-ci/gimme/5256888df1e749fdb6acabc8f9e07a52f05b9f28/gimme
  chmod +x ~/bin/gimme
fi

# Install golang dev tools
eval $(~/bin/gimme 1.17)
tool_urls=(
  'github.com/go-delve/delve/cmd/dlv'
  'github.com/kisielk/errcheck'
  'github.com/nsf/gocode'
  'github.com/rogpeppe/godef'
  'github.com/tools/godep'
  'golang.org/x/tools/cmd/cover'
  'golang.org/x/tools/cmd/gorename'
  'golang.org/x/tools/cmd/guru'
  'golang.org/x/tools/gopls'
)
export GOPATH=~/go
for tool_url in ${tool_urls[@]}; do
  go get -u "${tool_url}"
done
