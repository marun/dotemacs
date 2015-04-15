#!/usr/bin/env bash

# Install cask and its python dependencies.

set -e

if [ ! -d ~/.cask ]; then
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
fi

# On OS X, homebrew pip will configure a local install path by
# default, and passing --user on the commandline will cause an error.
pip_cmd="pip install --upgrade"

# For linux distros, --user is necessary to install as a non-priveleged user.
if ! which sw_vers &> /dev/null ; then
    pip_cmd="$pip_cmd --user"
fi

cask install
$pip_cmd jedi rope flake8 importmagic
