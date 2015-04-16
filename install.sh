#!/usr/bin/env bash

# Install cask and language dependencies.

set -e

if [ ! -d ~/.cask ]; then
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
fi

cask install

# On OS X, homebrew pip will configure a local install path by
# default, and passing --user on the commandline will cause an error.
pip_cmd="pip install --upgrade"
# For linux distros, --user is necessary to install as a non-priveleged user.
if ! which sw_vers &> /dev/null ; then
    pip_cmd="$pip_cmd --user"
fi
$pip_cmd jedi rope flake8 importmagic

go_get="go get -u"
if which go &> /dev/null ; then
    $go_get code.google.com/p/rog-go/exp/cmd/godef
    $go_get github.com/dougm/goflymake
    $go_get github.com/kisielk/errcheck
    $go_get github.com/nsf/gocode
fi
