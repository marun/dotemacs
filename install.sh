#!/usr/bin/env bash

# Install cask and language dependencies.

set -e

if [ ! -d ~/.cask ]; then
  curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
fi

EMACS_DIR="$(dirname $0)"
HOME_DIR="$(dirname "${EMACS_DIR}")"
pushd "${EMACS_DIR}" > /dev/null
  ${HOME_DIR}/.cask/bin/cask install
popd > /dev/null

# On OS X, homebrew pip will configure a local install path by
# default, and passing --user on the commandline will cause an error.
pip_cmd="pip install --upgrade"
# For linux distros, --user is necessary to install as a non-privileged user.
if ! which sw_vers &> /dev/null ; then
  pip_cmd="$pip_cmd --user"
fi
$pip_cmd jedi rope flake8 importmagic

eval $(gimme 1.11.2)
tool_urls=(
  'github.com/rogpeppe/godef'
  'github.com/dougm/goflymake'
  'github.com/kisielk/errcheck'
  'github.com/nsf/gocode'
  'github.com/tools/godep'
  'golang.org/x/tools/cmd/cover'
  'golang.org/x/tools/cmd/gorename'
  'golang.org/x/tools/cmd/guru'
)
export GOPATH="${HOME}/go"
for tool_url in ${tool_urls[@]}; do
  go get -u "${tool_url}"
done
