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
